-module('clojerl.Seqable').

-export([seq/1]).

-type type() :: list().

-callback 'clojerl.Seqable.seq'(Seqable :: any()) -> type().

-spec seq(any()) -> type().
seq(Seqable) ->
  'clojerl.protocol':resolve(?MODULE, seq, [Seqable]).
