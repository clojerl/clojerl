%% @doc Clojerl compilation environment.
%%
%% `clj_env:env()' holds the information for the current compilation
%% environment. It is used by all the compilation stages: the
%% `clj_reader', the `clj_analyzer' and the `clj_emitter'.
%%
%% Some examples of the information it keeps are:
%% <ul>
%%   <li>Local variables available in the current scope.</li>
%%   <li>Current location in the file or string being processed.</li>
%%   <li>Stack of expressions added by the `clj_analyzer'.</li>
%%   <li>... and more.</li>
%% </ul>
-module(clj_env).

-include("clojerl.hrl").
-include("clojerl_expr.hrl").

-export([ default/0
        , context/1
        , location/1
        , time/1
        , time/3

        , push_expr/2
        , pop_expr/1
        , last_exprs/2
        , exprs/1

        , add_locals_scope/1
        , remove_locals_scope/1
        , get_local/2
        , get_locals/1
        , put_local/3
        , put_locals/2
        , save_locals_scope/1
        , restore_locals_scope/1

        , get/2
        , get/3
        , push/2
        , update/3
        , put/3
        , pop/1
        ]).

-type context() :: expr | return | statement.

-type env() :: #{ context      => context()
                , exprs        => [expr()]
                , locals       => clj_scope:scope()
                , saved_locals => clj_scope:scope() | ?NIL
                , mapping      => clj_scope:scope()
                }.

-export_type([env/0]).

%% @doc Returns a default new environment.
-spec default() -> env().
default() ->
  #{ context      => expr
   , exprs        => []
   , locals       => clj_scope:new()
   , saved_locals => ?NIL
   , mapping      => clj_scope:new()
   }.

%% @doc Returns the current type of context.
-spec context(env()) -> context().
context(Env) -> get(context, expr, Env).

%% @doc Returns the current location in the file or string.
%%
%% When the location is not available it returns `undefined'.
-spec location(env()) -> ?NIL | clj_reader:location().
location(Env) -> get(location, Env).

%% @doc Returns the timing information stored by the compiler.
%%
%% Timing information is only stored and kept in the environment by
%% the compiler when the user asks for a timed compilation.
-spec time(env()) -> map().
time(Env) ->
  get(times, #{}, Env).

%% @doc Increase the time for `Key' by `Delta'.
-spec time(any(), number(), env()) -> env().
time(Key, Delta, Env) ->
  Times0 = get(times, #{}, Env),
  T      = maps:get(Key, Times0, 0),
  Times1 = Times0#{Key => T + Delta},
  put(times, Times1, Env).

%% @doc Push a Clojerl expression to the stack.
-spec push_expr(expr(), env()) -> env().
push_expr(Expr, Env = #{exprs := Exprs}) ->
  Env#{exprs => [Expr | Exprs]}.

%% @doc Pop a Clojerl expression from the stack.
-spec pop_expr(env()) -> {expr(), env()}.
pop_expr(Env = #{exprs := [H | Exprs]}) ->
  {H, Env#{exprs => Exprs}}.

%% @doc Returns the last `N' expressions that where pushed to the
%% stack.
%%
%% The order of the returned expression is reversed to obtain a FIFO
%% result. The environment returned does not contain the returned
%% expressions.
-spec last_exprs(integer(), env()) -> {Exprs :: [any()], Env :: env()}.
last_exprs(N, Env = #{exprs := AllExprs}) when N >= 0 ->
  Exprs = lists:sublist(AllExprs, N),
  RestExprs = lists:sublist(AllExprs, N + 1, length(AllExprs)),
  {lists:reverse(Exprs),
   Env#{exprs => RestExprs}}.

%% @doc Returns all the expressions on the stack, but in FIFO order.
-spec exprs(env()) -> [any()].
exprs(#{exprs := AllExprs}) -> lists:reverse(AllExprs).

%% @doc Push a new local variables scope.
%%
%% A single local variable can be registered in the current scope by
%% using {@link put_local/2}. It's possible to register multiple at the
%% same time through {@link put_locals/2}.
%%
%% The scope can be popped using {@link remove_locals_scope/1}.
-spec add_locals_scope(env()) -> env().
add_locals_scope(Env = #{locals := ParentLocals}) ->
  Env#{locals => clj_scope:new(ParentLocals)}.

%% @doc Pop a local variables scope.
%%
%% The scope can be popped using {@link remove_locals_scope/1}.
-spec remove_locals_scope(env()) -> env().
remove_locals_scope(Env = #{locals := Locals}) ->
  Env#{locals => clj_scope:parent(Locals)}.

%% @doc Saves the local variables in the current scope and empties the
%% scope.
%%
%% The saved local variables can be restored using {@link
%% restore_locals_scope/1}.
-spec save_locals_scope(env()) -> clj_scope:scope().
save_locals_scope(#{locals := Locals} = Env) ->
  Env#{locals := clj_scope:new(), saved_locals := Locals}.

%% @doc Restore the saved local variables to the current scope.
%%
%% See {@link save_locals_scope/1}.
-spec restore_locals_scope(env()) -> env().
restore_locals_scope(#{saved_locals := LocalsCache} = Env) ->
  Env#{locals := LocalsCache, saved_locals := ?NIL}.

%% @doc Returns the information for the local variable with the name
%% `Sym' or `undefined' if not found.
-spec get_local('clojerl.Symbol':type(), env()) -> expr().
get_local(Sym, _Env = #{locals := Locals}) ->
  clj_scope:get(clj_rt:str(Sym), Locals).

%% @doc Returns all available local variables in the current scope.
-spec get_locals(env()) -> [expr()].
get_locals(#{locals := Locals}) ->
  clj_scope:values(Locals).

%% @doc Adds the expression for the local variable named `Sym'.
-spec put_local('clojerl.Symbol':type(), expr(), env()) -> env().
put_local(Sym, Local, Env = #{locals := Locals}) ->
  Env#{locals => clj_scope:put(clj_rt:str(Sym), Local, Locals)}.

%% @doc Adds the expression for multiple local variables.
-spec put_locals([expr()], env()) -> env().
put_locals(Locals, Env) ->
  PutLocalFun = fun(Local, EnvAcc) ->
                    Name = maps:get(name, Local),
                    put_local(Name, Local, EnvAcc)
                end,
  lists:foldl(PutLocalFun, Env, Locals).

%% @equiv get(Name, undefined, Env)
-spec get(atom(), env()) -> any().
get(Name, Env) ->
  get(Name, ?NIL, Env).

%% @doc Get a value stored in the environment or `Default' if not
%% found.
-spec get(atom(), any(), env()) -> any().
get(Name, Default, #{mapping := Mapping}) ->
  clj_scope:get(Name, Default, Mapping).

%% @doc Push a custom `Mapping' into the custom scope {@link clj_scope}.
-spec push(map(), env()) -> env().
push(Mapping, Env = #{mapping := Parent}) ->
  Child = clj_scope:new(Parent),
  Env#{mapping := clj_scope:put(Mapping, Child)}.

%% @doc Update a value in the custom scope {@link clj_scope:update/3}.
-spec update(any(), any(), env()) -> env().
update(Name, Value, Env = #{mapping := Mapping}) ->
  Env#{mapping := clj_scope:update(Name, Value, Mapping)}.

%% @doc Put a value in the custom scope {@link clj_scope:put/3}.
-spec put(any(), any(), env()) -> env().
put(Name, Value, Env = #{mapping := Mapping}) ->
  Env#{mapping := clj_scope:put(Name, Value, Mapping)}.

%% @doc Pop the custom scope.
-spec pop(env()) -> env().
pop(Env = #{mapping := Parent}) ->
  Env#{mapping := clj_scope:parent(Parent)}.
