-module('clojerl.Map.clojerl.IMeta').

-behavior('clojerl.IMeta').

-export([meta/1, with_meta/2]).

-spec meta('clojerl.Map':type()) -> any().
meta({'clojerl.Map', _}) ->
  undefined.

-spec with_meta('clojerl.Map':type(), 'clojerl.Map':type()) ->
  'clojerl.Map':type().
with_meta({'clojerl.Map', _} = Map, _Metadata) ->
  Map.
