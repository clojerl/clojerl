-module(clj_env).

-include("clojerl.hrl").

-export([ default/0
        , context/1
        , context/2
        , location/1
        , location/2
        , maybe_update_location/2
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
        , put/3
        , remove/2
        ]).

-type context() :: expr | return | statement.

-type env() :: #{ context    => context()
                , location   => ?NIL | clj_reader:location()
                , exprs      => []
                , locals     => clj_scope:scope()
                }.

-export_type([env/0]).

-spec default() -> env().
default() ->
  #{ context    => expr
   , location   => ?NIL
   , exprs      => []
   , locals     => clj_scope:new()
   }.

-spec context(env()) -> context().
context(#{context := Ctx}) -> Ctx.

-spec context(context(), env()) -> env().
context(Ctx, Env) -> Env#{context => Ctx}.

-spec location(env()) -> ?NIL | clj_reader:location().
location(#{location := Location}) ->
  Location.

-spec location(?NIL | clj_reader:location(), env()) -> env().
location(Location, Env) ->
  Env#{location => Location}.

-spec maybe_update_location(?NIL | map(), env()) -> env().
maybe_update_location(?NIL, Env) ->
  Env;
maybe_update_location(Location, Env) ->
  Env#{location => Location}.

-spec push_expr(map(), env()) -> env().
push_expr(Expr, Env = #{exprs := Exprs}) ->
  Env#{exprs => [Expr | Exprs]}.

-spec pop_expr(env()) -> env().
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
  clj_scope:get(clj_core:str(Sym), Locals).

-spec put_local('clojerl.Symbol':type(), any(), env()) -> env().
put_local(Sym, Local, Env = #{locals := Locals}) ->
  Env#{locals => clj_scope:put(clj_core:str(Sym), Local, Locals)}.

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
get(Name, Default, Env) ->
  maps:get(Name, Env, Default).

-spec put(atom(), any(), env()) -> any().
put(Name, Value, Env) ->
  maps:put(Name, Value, Env).

-spec remove(atom(), env()) -> ok.
remove(Name, Env) ->
  maps:remove(Name, Env).
