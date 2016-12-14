-module(clj_scope).

-include("clojerl.hrl").

-export([ new/0
        , new/1
        , parent/1
        , get/2
        , put/3
        , update/3
        , to_map/2
        ]).

-type scope() :: #{ parent   => scope() | ?NIL
                  , mappings => map()
                  }.

-spec new() -> scope().
new() -> new(?NIL).

-spec new(scope()) -> scope().
new(Parent) ->
 #{ parent   => Parent
  , mappings => #{}
  }.

-spec parent(scope()) -> scope() | ?NIL.
parent(?NIL)  -> ?NIL;
parent(Scope) -> maps:get(parent, Scope).

-spec get(any(), scope()) -> any().
get(Key, Scope) ->
  do_get(Key, Scope).

-spec put(any(), any(), scope()) -> scope().
put(Key, Value, Scope = #{mappings := Mappings}) ->
  Scope#{mappings => Mappings#{Key => Value}}.

-spec update(any(), any(), scope()) -> scope().
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
-spec do_get(any(), scope() | ?NIL) -> any().
do_get(_, ?NIL) ->
  ?NIL;
do_get(Key, Scope = #{mappings := Mappings}) ->
  case maps:is_key(Key, Mappings) of
    false ->
      do_get(Key, parent(Scope));
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
