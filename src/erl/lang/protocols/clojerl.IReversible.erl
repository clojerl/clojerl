-module('clojerl.IReversible').

-clojure(true).
-protocol(true).

-export([rseq/1]).

-type type() :: any().

-callback rseq(any()) -> type().

-spec rseq(type()) -> type().
rseq(Chunk) ->
  clj_protocol:resolve(?MODULE, rseq, Chunk).
