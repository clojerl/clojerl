-module('clojerl.Symbol.clojerl.IMeta').
-behavior('clojerl.IMeta').

-export([meta/1, with_meta/2]).

-spec meta('clojerl.Symbol':type()) -> any().
meta({'clojerl.Symbol', Data}) ->
  maps:get(meta, Data, undefined).

-spec with_meta(any(), 'clojerl.Map':type()) -> any().
with_meta({'clojerl.Symbol', Data}, Metadata) ->
  {'clojerl.Symbol', maps:put(meta, Metadata, Data)}.
