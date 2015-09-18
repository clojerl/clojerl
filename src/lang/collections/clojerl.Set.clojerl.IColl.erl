-module('clojerl.Set.clojerl.IColl').

-behavior('clojerl.IColl').

-export([count/1, cons/2, empty/1, equiv/2]).

-spec count('clojerl.Set':type()) -> integer().
count({_, Set, _}) -> gb_sets:size(Set).

-spec cons('clojerl.Set':type(), any()) -> 'clojerl.List':type().
cons({_, Set, _}, X) ->
  Items = gb_sets:to_list(Set),
  clj_core:list([X | Items]).

-spec empty('clojerl.Set':type()) -> 'clojerl.Set':type().
empty(_) -> 'clojerl.Set':new([]).

-spec equiv('clojerl.Set':type(), any()) -> boolean().
equiv(X, X) -> true;
equiv(_, _) -> false.
