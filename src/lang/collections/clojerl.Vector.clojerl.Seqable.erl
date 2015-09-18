-module('clojerl.Vector.clojerl.Seqable').

-behaviour('clojerl.Seqable').

-export([seq/1]).

seq({_, [], _}) -> undefined;
seq({_, Array, _}) -> clj_core:list(array:to_list(Array)).
