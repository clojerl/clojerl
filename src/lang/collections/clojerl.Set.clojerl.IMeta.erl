-module('clojerl.Set.clojerl.IMeta').

-behavior('clojerl.IMeta').

-export([meta/1, with_meta/2]).

-spec meta('clojerl.Set':type()) -> any().
meta({'clojerl.Set', _, Info}) ->
  maps:get(meta, Info, undefined).

-spec with_meta('clojerl.Set':type(), 'clojerl.Set':type()) ->
  'clojerl.Set':type().
with_meta({'clojerl.Set', Set, Info}, Metadata) ->
  {'clojerl.Set', Set, Info#{meta => Metadata}}.
