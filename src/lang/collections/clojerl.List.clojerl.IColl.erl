-module('clojerl.List.clojerl.IColl').

-behaviour('clojerl.IColl').

-export([count/1, cons/2, equiv/2]).

-spec count('clojerl.List':type()) -> integer().
count({_, Items}) -> length(Items).

-spec cons('clojerl.List':type(), any()) -> 'clojerl.List':type().
cons({T, []}, X) -> {T, [X]};
cons({T, Items}, X) -> {T, [X | Items]}.

-spec equiv('clojerl.List':type(), any()) -> boolean().
equiv(X, X) -> true;
equiv(_, _) -> false.
