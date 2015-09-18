-module('clojerl.Vector.clojerl.IMeta').

-behavior('clojerl.IMeta').

-export([meta/1, with_meta/2]).

-spec meta('clojerl.Vector':type()) -> any().
meta({'clojerl.Vector', _, Info}) ->
  maps:get(meta, Info, undefined).

-spec with_meta('clojerl.Vector':type(), 'clojerl.Vector':type()) ->
  'clojerl.Vector':type().
with_meta({'clojerl.Vector', Vector, Info}, Metadata) ->
  {'clojerl.Vector', Vector, Info#{meta => Metadata}}.
