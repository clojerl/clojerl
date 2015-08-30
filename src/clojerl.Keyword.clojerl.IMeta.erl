-module('clojerl.Keyword.clojerl.IMeta').
-behavior('clojerl.IMeta').

-export([meta/1, with_meta/2]).

-spec meta('clojerl.Keyword':type()) -> any().
meta({'clojerl.Keyword', Data}) ->
  maps:get(meta, Data, undefined).

-spec with_meta(any(), 'clojerl.Map':type()) -> any().
with_meta({'clojerl.Keyword', Data}, Metadata) ->
  {'clojerl.Keyword', maps:put(meta, Metadata, Data)}.
