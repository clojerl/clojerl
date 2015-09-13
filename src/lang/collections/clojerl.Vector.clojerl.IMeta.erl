-module('clojerl.Vector.clojerl.IMeta').

-behavior('clojerl.IMeta').

-export([meta/1, with_meta/2]).

-spec meta('clojerl.Vector':type()) -> any().
meta({'clojerl.Vector', _}) ->
  undefined.

-spec with_meta('clojerl.Vector':type(), 'clojerl.Vector':type()) ->
  'clojerl.Vector':type().
with_meta({'clojerl.Vector', _} = Vector, _Metadata) ->
  Vector.
