-module('clojerl.Set.clojerl.IMeta').

-behavior('clojerl.IMeta').

-export([meta/1, with_meta/2]).

-spec meta('clojerl.Set':type()) -> any().
meta({'clojerl.Set', _}) ->
  undefined.

-spec with_meta('clojerl.Set':type(), 'clojerl.Set':type()) ->
  'clojerl.Set':type().
with_meta({'clojerl.Set', _} = Set, _Metadata) ->
  Set.
