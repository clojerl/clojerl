-module('clojerl.Vector.clojerl.IColl').

-behavior('clojerl.IColl').

-export([count/1, cons/2, empty/1, equiv/2]).

-spec count('clojerl.Vector':type()) -> integer().
count({_, Array}) -> array:size(Array).

-spec cons('clojerl.Vector':type(), any()) -> 'clojerl.Vector':type().
cons({T, Array}, X) ->
  NewArray = array:set(array:size(Array), X, Array),
  {T, NewArray}.

-spec empty('clojerl.Vector':type()) -> 'clojerl.Vector':type().
empty(_) -> 'clojerl.Vector':new([]).

-spec equiv('clojerl.Vector':type(), any()) -> boolean().
equiv(X, X) -> true;
equiv(_, _) -> false.
