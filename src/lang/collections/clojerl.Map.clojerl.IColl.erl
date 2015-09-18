-module('clojerl.Map.clojerl.IColl').

-behavior('clojerl.IColl').

-export([count/1, cons/2, empty/1, equiv/2]).

-spec count('clojerl.Map':type()) -> integer().
count({_, Map, _}) -> maps:size(Map).

-spec cons('clojerl.Map':type(), any()) -> 'clojerl.List':type().
cons({_, Map, _}, X) ->
  FoldFun = fun(K, V, List) ->
                [clj_core:vector([K, V]) | List]
            end,
  Items = maps:fold(FoldFun, [], Map),
  clj_core:list([X | Items]).

-spec empty('clojerl.Map':type()) -> 'clojerl.Map':type().
empty(_) -> 'clojerl.Map':new([]).

-spec equiv('clojerl.Map':type(), any()) -> boolean().
equiv(X, X) -> true;
equiv(_, _) -> false.
