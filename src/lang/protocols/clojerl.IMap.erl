-module('clojerl.IMap').

-export([keys/1, vals/1, without/2]).

-type type() :: any().

-callback 'clojerl.IMap.keys'(IMap :: type()) -> list().
-callback 'clojerl.IMap.vals'(IMap :: type()) -> list().
-callback 'clojerl.IMap.without'(IMap :: type(), Key :: any()) -> type().

-spec keys(type()) -> list().
keys(Map) ->
  'clojerl.protocol':resolve(?MODULE, keys, [Map]).

-spec vals(type()) -> list().
vals(Map) ->
  'clojerl.protocol':resolve(?MODULE, vals, [Map]).

-spec without(type(), any()) -> type().
without(Map, Key) ->
  'clojerl.protocol':resolve(?MODULE, without, [Map, Key]).
