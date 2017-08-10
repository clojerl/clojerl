-module('clojerl.IChunkedSeq').

-clojure(true).
-protocol(true).

-export([chunked_first/1, chunked_next/1, chunked_more/1]).

-type type() :: any().

-callback chunked_first(IChunkedSeq :: type()) -> any().
-callback chunked_next(IChunkedSeq :: type()) -> type().
-callback chunked_more(IChunkedSeq :: type()) -> type().

-spec chunked_first(type()) -> any().
chunked_first(Seq) ->
  clj_protocol:resolve(?MODULE, chunked_first, Seq).

-spec chunked_next(type()) -> type().
chunked_next(Seq) ->
  clj_protocol:resolve(?MODULE, chunked_next, Seq).

-spec chunked_more(type()) -> type().
chunked_more(Seq) ->
  clj_protocol:resolve(?MODULE, chunked_more, Seq).
