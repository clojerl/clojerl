-module(clj_env).

-export([
         default/0,
         context/1,
         context/2,
         push_expr/2,
         pop_expr/1,
         last_exprs/2,
         exprs/1,

         find_or_create_ns/2,
         current_ns/1,
         current_ns/2,
         update_ns/3,
         find_ns/2,
         resolve_ns/2,

         add_locals_scope/1,
         remove_locals_scope/1,
         get_local/2,
         put_local/3,
         put_locals/2,

         get/2,
         get/3,
         put/3,
         remove/2,

         update_var/2,
         find_var/2
        ]).

-type context() :: expr | return | statement.

-type env() :: #{ namespaces => []
                , context    => context()
                , exprs      => []
                , current_ns => 'clojerl.Symbol':type()
                , locals     => clj_scope:scope()
                }.

-export_type([env/0]).

-spec default() -> env().
default() ->
  UserSym = clj_core:symbol(<<"$user">>),
  UserBin = clj_core:str(UserSym),
  UserNs = clj_namespace:new(UserSym),

  #{ namespaces => #{UserBin => UserNs}
   , context    => expr
   , exprs      => []
   , current_ns => UserSym
   , locals     => clj_scope:new()
   }.

-spec context(env()) -> context().
context(#{context := Ctx}) -> Ctx.

-spec context(env(), context()) -> env().
context(Env, Ctx) -> Env#{context => Ctx}.

-spec push_expr(env(), map()) -> env().
push_expr(Env = #{exprs := Exprs}, Expr) ->
  Env#{exprs => [Expr | Exprs]};
push_expr(Env, Expr) ->
  Env#{exprs => [Expr]}.

-spec pop_expr(env()) -> env().
pop_expr(Env = #{exprs := [H | Exprs]}) ->
  {H, Env#{exprs => Exprs}};
pop_expr(Env) ->
  {undefined, Env}.

-spec last_exprs(env(), integer()) -> {[any()], env()}.
last_exprs(Env = #{exprs := AllExprs}, N) when N >= 0 ->
  Exprs = lists:sublist(AllExprs, N),
  RestExprs = lists:sublist(AllExprs, N + 1, length(AllExprs)),
  {lists:reverse(Exprs),
   Env#{exprs => RestExprs}}.

-spec exprs(env()) -> [any()].
exprs(#{exprs := AllExprs}) -> lists:reverse(AllExprs).

-spec find_or_create_ns(env(), 'clojerl.Symbol':type()) -> env().
find_or_create_ns(Env, NsSym) ->
  case find_ns(Env, NsSym) of
    undefined ->
      Ns = clj_namespace:new(NsSym),
      {Ns, current_ns(add_ns(Env, Ns), NsSym)};
    Ns ->
      {Ns, current_ns(Env, NsSym)}
  end.

%% @private
-spec add_ns(env(), clj_namespace:namespace()) -> env().
add_ns(Env = #{namespaces := Namespaces}, Ns) ->
  NsSym = clj_namespace:name(Ns),
  case find_ns(Env, NsSym) of
    undefined ->
      NewNamespaces = maps:put(clj_core:str(NsSym), Ns, Namespaces),
      Env#{namespaces => NewNamespaces};
    _ ->
      Env
  end.

-spec current_ns(env()) -> 'clojure.Symbol':type().
current_ns(#{current_ns := CurrentNs}) ->
  CurrentNs.

-spec current_ns(env(), 'clojerl.Symbol':type()) -> env().
current_ns(Env, CurrentNs) ->
  case find_ns(Env, CurrentNs) of
    undefined ->
      throw(<<"The specified namespace does not exist">>);
    _ ->
      Env#{current_ns => CurrentNs}
  end.

-spec update_ns(env(), 'clojerl.Symbol':type(), function()) ->
  clj_namespace:namespace().
update_ns(Env = #{namespaces := Nss}, Name, Fun) ->
  NameBin = clj_core:str(Name),
  case maps:get(NameBin, Nss, undefined) of
    undefined ->
      Env;
    Ns ->
      NewNs = Fun(Ns),
      NewNss = maps:put(NameBin, NewNs, Nss),
      Env#{namespaces => NewNss}
  end.

%% @doc Tries to find a namespace by its name symbol.
-spec find_ns(env(), 'clojerl.Symbol':type()) -> clj_namespace:namespace().
find_ns(_Env = #{namespaces := Nss}, SymNs) ->
  maps:get(clj_core:str(SymNs), Nss, undefined).

%% @doc Tries to find a namespace by its name symbol or by itsalias
%%      in the current namespace if it is there.
-spec resolve_ns(env(), 'clojerl.Symbol':type()) -> clj_namespace:namespace().
resolve_ns(Env, SymNs) ->
  case find_ns(Env, SymNs) of
    undefined ->
      CurrentNsSym = current_ns(Env),
      CurrentNs = find_ns(Env, CurrentNsSym),
      AliasedNsSym = clj_namespace:alias(CurrentNs, SymNs),
      find_ns(Env, AliasedNsSym);
    Ns ->
      Ns
  end.

-spec add_locals_scope(env()) -> env().
add_locals_scope(Env = #{locals := ParentLocals}) ->
  Env#{locals => clj_scope:new(ParentLocals)}.

-spec remove_locals_scope(env()) -> env().
remove_locals_scope(Env = #{locals := Locals}) ->
  Env#{locals => clj_scope:parent(Locals)}.

-spec get_local(env(), 'clojerl.Symbol':type()) -> any().
get_local(_Env = #{locals := Locals}, Sym) ->
  clj_scope:get(Locals, clj_core:str(Sym)).

-spec put_local(env(), 'clojerl.Symbol':type(), any()) -> env().
put_local(Env = #{locals := Locals}, Sym, Local) ->
  Env#{locals => clj_scope:put(Locals, clj_core:str(Sym), Local)}.

-spec put_locals(env(), [map()]) -> env().
put_locals(Env, Locals) ->
  PutLocalFun = fun(Local, EnvAcc) ->
                    Name = maps:get(name, Local),
                    put_local(EnvAcc, Name, Local)
                end,
  lists:foldl(PutLocalFun, Env, Locals).

-spec get(env(), atom()) -> any().
get(Env, Name) ->
  get(Env, Name, undefined).

-spec get(env(), atom(), any()) -> any().
get(Env, Name, Default) ->
  maps:get(Name, Env, Default).

-spec put(env(), atom(), any()) -> any().
put(Env, Name, Value) ->
  maps:put(Name, Value, Env).

-spec remove(env(), atom()) -> ok.
remove(Env, Name) ->
  maps:remove(Name, Env).

-spec update_var(env(), 'clojerl.Var':type()) -> clj_namespace:namespace().
update_var(Env, Var) ->
  VarNsSym = clj_core:symbol(clj_core:namespace(Var)),
  Fun = fun(Ns) -> clj_namespace:update_var(Ns, Var) end,
  update_ns(Env, VarNsSym, Fun).

%% @private
-spec find_var(env(), 'clojerl.Symbol':type()) ->
  {'clojerl.Var':type() | undefined, env()}.
find_var(Env, Symbol) ->
  find_var(Env, Symbol, true).

%% @private
-spec find_var(env(), 'clojerl.Symbol':type(), boolean()) ->
  {'clojerl.Var':type() | undefined, env()}.
find_var(Env, Symbol, LoadModule) ->
  NsSym = case clj_core:namespace(Symbol) of
            undefined -> current_ns(Env);
            NsStr -> clj_core:symbol(NsStr)
          end,
  case find_ns(Env, NsSym) of
    undefined when LoadModule ->
      find_var(load_module_vars(Env, NsSym), Symbol, false);
    undefined ->
      {undefined, Env};
    Ns ->
      NameSym = clj_core:symbol(clj_core:name(Symbol)),
      {clj_namespace:def(Ns, NameSym), Env}
  end.

%% @private
%% @doc Tries to get the vars from the module associated to the
%%      namespace. If the module is not found or if it doesn't
%%      have a 'vars' attribute, then a dummy module is added with
%%      no vars.
-spec load_module_vars(env(), 'clojerl.Symbol':type()) -> env().
load_module_vars(Env, NsSym) ->
  NameStr = clj_core:name(NsSym),
  Module  = binary_to_atom(NameStr, utf8),

  _ = code:ensure_loaded(Module),

  Vars = case erlang:function_exported(Module, module_info, 1) of
           true ->
             Attrs = Module:module_info(attributes),
             case lists:keyfind(vars, 1, Attrs) of
               {vars, [VarsMap]} -> maps:values(VarsMap);
               false             -> []
             end;
           false -> []
         end,
  NewNs  = clj_namespace:new(NsSym),
  NewNs1 = lists:foldl(fun add_var/2, NewNs, Vars),
  add_ns(Env, NewNs1).

-spec add_var('clojerl.Var':type(), clj_namespace:namespace()) ->
  clj_namespace:namespace().
add_var(Var, Ns) ->
  clj_namespace:update_var(Ns, Var).
