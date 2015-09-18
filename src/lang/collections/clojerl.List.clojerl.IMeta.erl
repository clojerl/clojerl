-module('clojerl.List.clojerl.IMeta').

-behavior('clojerl.IMeta').

-export([meta/1, with_meta/2]).

-spec meta('clojerl.List':type()) -> any().
meta({'clojerl.List', _, Info}) ->
  maps:get(meta, Info, undefined).

-spec with_meta(any(), 'clojerl.Map':type()) -> any().
with_meta({'clojerl.List', Data, Info}, Metadata) ->
  {'clojerl.List', Data, Info#{meta => Metadata}}.
