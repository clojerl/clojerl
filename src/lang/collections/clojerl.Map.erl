-module('clojerl.Map').

-behavior('clojerl.ILookup').

-export([
         new/1,
         keys/1,
         vals/1
        ]).

%% ILookup
-export([get/2, get/3]).

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

%%------------------------------------------------------------------------------
%% ILookup
%%------------------------------------------------------------------------------

-spec get(any(), any()) -> any().
get({?MODULE, _, _} = Map, Key) ->
  get(Map, Key, undefined).

-spec get(any(), any(), any()) -> any().
get({?MODULE, Map, _}, Key, NotFound) ->
  maps:get(Key, Map, NotFound).
