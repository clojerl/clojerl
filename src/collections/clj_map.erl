-module(clj_map).

-export([
         new/1
        ]).

-include("include/clj_types.hrl").

-spec new(list()) -> 'map*'().
new(KeyValues) ->
  KeyValuePairs = build_key_values([], KeyValues),
  {map, maps:from_list(KeyValuePairs)}.

build_key_values(KeyValues, []) ->
  lists:reverse(KeyValues);
build_key_values(KeyValues, [K, V | Items]) ->
  build_key_values([{K, V} | KeyValues], Items).
