-module('clojerl.Set.clojerl.ILookup').

-behavior('clojerl.ILookup').

-export([get/2, get/3]).

-spec get(any(), any()) -> any().
get({'clojerl.Set', _, _} = Set, Key) ->
  get(Set, Key, undefined).

-spec get(any(), any(), any()) -> any().
get({'clojerl.Set', Set, _}, Key, NotFound) ->
  case gb_sets:is_member(Key, Set) of
    true -> Key;
    false -> NotFound
  end.
