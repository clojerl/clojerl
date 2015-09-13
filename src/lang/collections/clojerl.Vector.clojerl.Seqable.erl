-module('clojerl.Vector.clojerl.Seqable').

-behaviour('clojerl.Seqable').

-export([seq/1]).

seq({_, []}) -> undefined;
seq({_, Array}) -> clj_core:list(array:to_list(Array)).
