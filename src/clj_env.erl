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

-spec context(env(), context()) -> env().
context(Env, Ctx) -> Env#{context => Ctx}.

-spec location(env()) -> ?NIL | clj_reader:location().
location(#{location := Location}) ->
  Location.

-spec location(env(), ?NIL | clj_reader:location()) -> env().
location(Env, Location) ->
  Env#{location => Location}.

-spec maybe_update_location(env(), ?NIL | map()) -> env().
maybe_update_location(Env, ?NIL) ->
  Env;
maybe_update_location(Env, Location) ->
  Env#{location => Location}.


-spec push_expr(env(), map()) -> env().
push_expr(Env = #{exprs := Exprs}, Expr) ->
  Env#{exprs => [Expr | Exprs]}.

-spec pop_expr(env()) -> env().
pop_expr(Env = #{exprs := [H | Exprs]}) ->
  {H, Env#{exprs => Exprs}}.

-spec last_exprs(env(), integer()) -> {[any()], env()}.
last_exprs(Env = #{exprs := AllExprs}, N) when N >= 0 ->
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
  get(Env, Name, ?NIL).

-spec get(env(), atom(), any()) -> any().
get(Env, Name, Default) ->
  maps:get(Name, Env, Default).

-spec put(env(), atom(), any()) -> any().
put(Env, Name, Value) ->
  maps:put(Name, Value, Env).

-spec remove(env(), atom()) -> ok.
remove(Env, Name) ->
  maps:remove(Name, Env).
