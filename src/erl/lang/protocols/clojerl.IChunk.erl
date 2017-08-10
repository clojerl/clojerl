-module('clojerl.IChunk').

-clojure(true).
-protocol(true).

-export([drop_first/1]).

-type type() :: any().

-callback drop_first(Chunk :: type()) -> type().

-spec drop_first(type()) -> type().
drop_first(Chunk) ->
  clj_protocol:resolve(?MODULE, drop_first, Chunk).
