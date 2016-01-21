-module('clojerl.Map').

-export([ new/1
        , keys/1
        , vals/1
        ]).

-export([ 'clojerl.ILookup.get'/2
        , 'clojerl.ILookup.get'/3
        ]).

-type type() :: {?MODULE, map(), map()}.

-spec new(list()) -> type().
new(KeyValues) when is_list(KeyValues) ->
  KeyValuePairs = build_key_values([], KeyValues),
  {?MODULE, maps:from_list(KeyValuePairs), #{}}.

-spec build_key_values(list(), list()) -> [{any(), any()}].
build_key_values(KeyValues, []) ->
  lists:reverse(KeyValues);
build_key_values(KeyValues, [K, V | Items]) ->
  build_key_values([{K, V} | KeyValues], Items).

-spec keys(type()) -> list().
keys({_, Map, _}) -> maps:keys(Map).

-spec vals(type()) -> list().
vals({_, Map, _}) -> maps:values(Map).

-spec 'clojerl.ILookup.get'(any(), any()) -> any().
'clojerl.ILookup.get'({?MODULE, _, _} = Map, Key) ->
  'clojerl.ILookup.get'(Map, Key, undefined).

-spec 'clojerl.ILookup.get'(any(), any(), any()) -> any().
'clojerl.ILookup.get'({?MODULE, Map, _}, Key, NotFound) ->
  maps:get(Key, Map, NotFound).
