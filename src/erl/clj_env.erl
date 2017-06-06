-module(clj_env).

-include("clojerl.hrl").

-export([ default/0
        , context/1
        , location/1

        , push_expr/2
        , pop_expr/1
        , last_exprs/2
        , exprs/1

        , add_locals_scope/1
        , remove_locals_scope/1
        , get_local/2
        , put_local/3
        , put_locals/2

        , get/2
        , get/3
        , push/2
        , update/3
        , put/3
        , pop/1
        ]).

-type context() :: expr | return | statement.

-type env() :: #{ context    => context()
                , exprs      => [map()]
                , locals     => clj_scope:scope()
                , mapping    => clj_scope:scope()
                }.

-export_type([env/0]).

-spec default() -> env().
default() ->
  #{ exprs      => []
   , locals     => clj_scope:new()
   , mapping    => clj_scope:new()
   }.

-spec context(env()) -> context().
context(Env) -> get(context, expr, Env).

-spec location(env()) -> ?NIL | clj_reader:location().
location(Env) -> get(location, Env).

-spec push_expr(map(), env()) -> env().
push_expr(Expr, Env = #{exprs := Exprs}) ->
  Env#{exprs => [Expr | Exprs]}.

-spec pop_expr(env()) -> {map(), env()}.
pop_expr(Env = #{exprs := [H | Exprs]}) ->
  {H, Env#{exprs => Exprs}}.

-spec last_exprs(integer(), env()) -> {[any()], env()}.
last_exprs(N, Env = #{exprs := AllExprs}) when N >= 0 ->
  Exprs = lists:sublist(AllExprs, N),
  RestExprs = lists:sublist(AllExprs, N + 1, length(AllExprs)),
  {lists:reverse(Exprs),
   Env#{exprs => RestExprs}}.

-spec exprs(env()) -> [any()].
exprs(#{exprs := AllExprs}) -> lists:reverse(AllExprs).

-spec add_locals_scope(env()) -> env().
add_locals_scope(Env = #{locals := ParentLocals}) ->
  Env#{locals => clj_scope:new(ParentLocals)}.

-spec remove_locals_scope(env()) -> env().
remove_locals_scope(Env = #{locals := Locals}) ->
  Env#{locals => clj_scope:parent(Locals)}.

-spec get_local('clojerl.Symbol':type(), env()) -> any().
get_local(Sym, _Env = #{locals := Locals}) ->
  clj_scope:get(clj_rt:str(Sym), Locals).

-spec put_local('clojerl.Symbol':type(), any(), env()) -> env().
put_local(Sym, Local, Env = #{locals := Locals}) ->
  Env#{locals => clj_scope:put(clj_rt:str(Sym), Local, Locals)}.

-spec put_locals([map()], env()) -> env().
put_locals(Locals, Env) ->
  PutLocalFun = fun(Local, EnvAcc) ->
                    Name = maps:get(name, Local),
                    put_local(Name, Local, EnvAcc)
                end,
  lists:foldl(PutLocalFun, Env, Locals).

-spec get(atom(), env()) -> any().
get(Name, Env) ->
  get(Name, ?NIL, Env).

-spec get(atom(), any(), env()) -> any().
get(Name, Default, #{mapping := Mapping}) ->
  clj_scope:get(Name, Default, Mapping).

-spec push(map(), env()) -> env().
push(Mapping, Env = #{mapping := Parent}) ->
  Child = clj_scope:new(Parent),
  Env#{mapping := clj_scope:put(Mapping, Child)}.

-spec update(any(), any(), env()) -> env().
update(Name, Value, Env = #{mapping := Mapping}) ->
  Env#{mapping := clj_scope:update(Name, Value, Mapping)}.

-spec put(any(), any(), env()) -> env().
put(Name, Value, Env = #{mapping := Mapping}) ->
  Env#{mapping := clj_scope:put(Name, Value, Mapping)}.

-spec pop(env()) -> env().
pop(Env = #{mapping := Parent}) ->
  Env#{mapping := clj_scope:parent(Parent)}.
