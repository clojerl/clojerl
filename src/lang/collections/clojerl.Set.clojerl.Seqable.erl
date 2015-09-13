-module('clojerl.Set.clojerl.Seqable').

-behaviour('clojerl.Seqable').

-export([seq/1]).

seq({_, []}) -> undefined;
seq({_, Set}) -> clj_core:list(gb_sets:to_list(Set)).
