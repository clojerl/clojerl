-module(clj_env).

-export([
         default/0,
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
         get_local/2,
         update_var/2,
         find_var/2
        ]).

-type context() :: expr | return | statement.

-type env() :: #{namespaces => [],
                 context => context(),
                 exprs => [],
                 current_ns => 'clojerl.Symbol':type(),
                 locals => #{'clojerl.Symbol':type() => any()}}.

-export_type([env/0]).

-spec default() -> env().
default() ->
  UserSym = clj_core:symbol(<<"user">>),
  UserNs = clj_namespace:new(UserSym),
  #{namespaces => #{UserSym => UserNs},
    context    => expr,
    exprs      => [],
    current_ns => UserSym,
    locals     => #{}}.

-spec context(env(), context()) -> env().
context(Env, Ctx) -> Env#{context => Ctx}.

-spec push_expr(env(), erl_syntax:syntaxTree()) -> env().
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

-spec add_ns(env(), clj_namespace:namespace()) -> env().
add_ns(Env = #{namespaces := Namespaces}, Ns) ->
  NsSym = clj_namespace:name(Ns),
  case find_ns(Env, NsSym) of
    undefined ->
      NewNamespaces = maps:put(NsSym, Ns, Namespaces),
      Env#{namespaces => NewNamespaces};
    _ ->
      Env
  end.

-spec current_ns(env()) -> clj_namespace:namespace().
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
  case maps:get(Name, Nss, undefined) of
    undefined ->
      Env;
    Ns ->
      NewNs = Fun(Ns),
      NewNss = maps:put(Name, NewNs, Nss),
      Env#{namespaces => NewNss}
  end.

-spec find_ns(env(), 'clojerl.Symbol':type()) -> clj_namespace:namespace().
find_ns(_Env = #{namespaces := Nss}, SymNs) ->
  maps:get(SymNs, Nss, undefined).

-spec get_local(env(), 'clojerl.Symbol':type()) -> clj_namespace:namespace().
get_local(_Env = #{locals := Locals}, Sym) ->
  maps:get(Sym, Locals, undefined).

-spec update_var(env(), 'clojerl.Var':type()) -> clj_namespace:namespace().
update_var(Env, Var) ->
  VarNsSym = 'clojerl.Var':namespace(Var),
  Fun = fun(Ns) -> clj_namespace:update_var(Ns, Var) end,
  update_ns(Env, VarNsSym, Fun).

-spec find_var(env(), 'clojerl.Symbol':type()) -> 'clojerl.Var':type().
find_var(Env, Symbol) ->
  NsSym = clj_core:symbol(clj_core:namespace(Symbol)),
  case find_ns(Env, NsSym) of
    undefined ->
      undefined;
    Ns ->
      NameSym = clj_core:symbol(clj_core:name(Symbol)),
      clj_namespace:def(Ns, NameSym)
  end.
