-module('clojerl.Set.clojerl.Seqable').

-behaviour('clojerl.Seqable').

-export([seq/1]).

seq({_, [], _}) -> undefined;
seq({_, Set, _}) -> gb_sets:to_list(Set).
