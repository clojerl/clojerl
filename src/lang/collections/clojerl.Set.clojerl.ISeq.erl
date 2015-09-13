-module('clojerl.Set.clojerl.ISeq').

-behaviour('clojerl.ISeq').

-export([
         first/1,
         next/1,
         more/1
        ]).

-spec first('clojerl.Set':type()) -> undefined | any().
first({_, Set}) ->
  Iterator = gb_sets:iterator(Set),
  case gb_sets:next(Iterator) of
    none -> undefined;
    {X, _} -> X
  end.

-spec next('clojerl.Set':type()) -> undefined | 'clojerl.List':type().
next({_, Set}) ->
  case gb_sets:to_list(Set) of
    [] -> undefined;
    [_ | []] -> undefined;
    [_ | Items] -> 'clojerl.List':new(Items)
  end.

-spec more('clojerl.Set':type()) -> undefined | 'clojerl.List':type().
more({_, GbSet} = Set) ->
  Items = gb_sets:to_list(GbSet),
  'clojerl.List':new(Items).
