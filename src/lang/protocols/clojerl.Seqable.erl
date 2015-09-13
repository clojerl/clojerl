-module('clojerl.Seqable').

-export([seq/1]).

-type type() :: any().

-callback seq(Seqable :: type()) -> type().

-spec seq(type()) -> type().
seq(Seqable) ->
  'clojerl.protocol':resolve(?MODULE, seq, [Seqable]).
