-module('clojerl.Seqable').

-export([seq/1]).

-type type() :: list().

-callback seq(Seqable :: any()) -> type().

-spec seq(any()) -> type().
seq(Seqable) ->
  'clojerl.protocol':resolve(?MODULE, seq, [Seqable]).
