-module('clojerl.Map').

-export([
         new/1
        ]).

-type type() :: {?MODULE, map()}.

-spec new(list()) -> type().
new(KeyValues) ->
  KeyValuePairs = build_key_values([], KeyValues),
  {?MODULE, maps:from_list(KeyValuePairs)}.

-spec build_key_values(list(), list()) -> [{any(), any()}].
build_key_values(KeyValues, []) ->
  lists:reverse(KeyValues);
build_key_values(KeyValues, [K, V | Items]) ->
  build_key_values([{K, V} | KeyValues], Items).
