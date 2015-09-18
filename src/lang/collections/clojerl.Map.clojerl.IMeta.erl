-module('clojerl.Map.clojerl.IMeta').

-behavior('clojerl.IMeta').

-export([meta/1, with_meta/2]).

-spec meta('clojerl.Map':type()) -> any().
meta({_, _, Info}) -> maps:get(meta, Info, undefined).

-spec with_meta('clojerl.Map':type(), 'clojerl.Map':type()) ->
  'clojerl.Map':type().
with_meta({_, Map, Info}, Metadata) ->
  {'clojerl.Map', Map, Info#{meta => Metadata}}.
