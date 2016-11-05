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

-spec get(scope(), any()) -> any().
get(Scope, Key) ->
  do_get(Scope, Key).

-spec put(scope(), any(), any()) -> scope().
put(Scope = #{mappings := Mappings}, Key, Value) ->
  Scope#{mappings => Mappings#{Key => Value}}.

-spec update(scope(), any(), any()) -> scope().
update(Scope, Key, Value) ->
  do_update(Scope, Key, Value).

-spec to_map(scope(), function()) -> any().
to_map(Scope, Fun) ->
  do_to_map(Scope, Fun, #{}).

%% @private
-spec do_to_map(scope() | ?NIL, function(), map()) -> map().
do_to_map(?NIL, _, Map) ->
  Map;
do_to_map(#{parent := Parent, mappings := Mappings}, Fun, Map) ->
  Mappings1 = maps:map(Fun, Mappings),
  do_to_map(Parent, Fun, maps:merge(Mappings1, Map)).

%% @private
-spec do_get(scope() | ?NIL, any()) -> any().
do_get(?NIL, _) ->
  ?NIL;
do_get(Scope = #{mappings := Mappings}, Key) ->
  case maps:is_key(Key, Mappings) of
    false ->
      do_get(parent(Scope), Key);
    true ->
      maps:get(Key, Mappings)
  end.

%% @private
-spec do_update(scope() | ?NIL, any(), any()) -> scope().
do_update(?NIL, K, _) ->
  throw({not_found, K});
do_update(Scope = #{mappings := Mappings, parent := Parent}, K, V) ->
  case maps:is_key(K, Mappings) of
    true ->
      Scope#{mappings => Mappings#{K => V}};
    false ->
      Scope#{parent => do_update(Parent, K, V)}
  end.
