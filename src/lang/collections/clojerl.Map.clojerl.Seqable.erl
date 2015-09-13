-module('clojerl.Map.clojerl.Seqable').

-behaviour('clojerl.Seqable').

-export([seq/1]).

seq({_, []}) -> undefined;
seq({_, Map}) ->
  FoldFun = fun(K, V, List) ->
                [clj_core:vector([K, V]) | List]
            end,
  Items = maps:fold(FoldFun, [], Map),
  clj_core:list(Items).
