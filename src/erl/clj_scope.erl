-module(clj_scope).

-include("clojerl.hrl").

-export([ new/0
        , new/1
        , parent/1
        , get/2
        , get/3
        , put/2
        , put/3
        , update/3
        , to_map/2
        ]).

-type scope() :: #{ parent   => scope() | ?NIL
                  , mappings => map()
                  }.

-spec new() -> scope().
new() -> new(?NIL).

-spec new(scope() | ?NIL) -> scope().
new(Parent) ->
 #{ parent   => Parent
  , mappings => #{}
  }.

-spec parent(scope()) -> scope() | ?NIL.
parent(?NIL)  -> ?NIL;
parent(Scope) -> maps:get(parent, Scope).

-spec get(any(), scope()) -> any().
get(Key, Scope) ->
  get(Key, ?NIL, Scope).

-spec get(any(), any(), scope()) -> any().
get(Key, Default, Scope) ->
  do_get(Key, Default, Scope).

-spec put(any(), any(), scope()) -> scope().
put(Key, Value, Scope = #{mappings := Mappings}) ->
  Scope#{mappings => Mappings#{Key => Value}}.

-spec put(map(), scope()) -> scope().
put(Map, Scope) ->
  PutFun = fun(Key, Acc) ->
               put(Key, maps:get(Key, Map), Acc)
           end,
  lists:foldl(PutFun, Scope, maps:keys(Map)).

-spec update(any(), any(), scope()) -> scope() | not_found.
update(Key, Value, Scope) ->
  do_update(Key, Value, Scope).

-spec to_map(function(), scope()) -> any().
to_map(Fun, Scope) ->
  do_to_map(Fun, #{}, Scope).

%% @private
-spec do_to_map(function(), map(), scope() | ?NIL) -> map().
do_to_map(_, Map, ?NIL) ->
  Map;
do_to_map(Fun, Map, #{parent := Parent, mappings := Mappings}) ->
  Mappings1 = maps:map(Fun, Mappings),
  do_to_map(Fun, maps:merge(Mappings1, Map), Parent).

%% @private
-spec do_get(any(), any(), scope() | ?NIL) -> any().
do_get(_, Default, ?NIL) ->
  Default;
do_get(Key, Default, Scope = #{mappings := Mappings}) ->
  case maps:is_key(Key, Mappings) of
    false ->
      do_get(Key, Default, parent(Scope));
    true ->
      maps:get(Key, Mappings)
  end.

%% @private
-spec do_update(any(), any(), scope() | ?NIL) -> scope() | not_found.
do_update(_K, _V, ?NIL) ->
  not_found;
do_update(K, V, Scope = #{mappings := Mappings, parent := Parent}) ->
  case maps:is_key(K, Mappings) of
    true ->
      Scope#{mappings => Mappings#{K => V}};
    false ->
      case do_update(K, V, Parent) of
        not_found -> not_found;
        NewParent -> Scope#{parent => NewParent}
      end
  end.
