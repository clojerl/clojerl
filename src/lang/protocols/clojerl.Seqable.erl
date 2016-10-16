-module('clojerl.Seqable').

-clojure(true).
-protocol(true).

-export([seq/1, to_list/1]).

-type type() :: list().

-callback seq(Seqable :: any()) -> type().

-spec seq(any()) -> type().
seq(Seqable) ->
  'clojerl.protocol':resolve(?MODULE, seq, Seqable).

-spec to_list(any()) -> list().
to_list(Seqable) ->
  'clojerl.protocol':resolve(?MODULE, to_list, Seqable).
