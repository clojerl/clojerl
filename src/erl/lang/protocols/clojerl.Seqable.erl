-module('clojerl.Seqable').

-include("clojerl.hrl").

-clojure(true).
-protocol(true).

-export([seq/1, to_list/1]).

-callback seq(Seqable :: any()) -> any().

-spec seq(any()) -> any().
seq(Seqable) ->
  clj_protocol:resolve(?MODULE, seq, Seqable).

-spec to_list(any()) -> [any()].
to_list(Seqable) ->
  clj_protocol:resolve(?MODULE, to_list, Seqable).
