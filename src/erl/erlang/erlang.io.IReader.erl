-module('erlang.io.IReader').

-export([read/1, read/2, read_line/1, skip/2]).

-type type() :: any().

-callback read(A :: type()) -> binary() | eof.

-callback read(A :: type(), L :: integer()) -> binary() | eof.

-callback read_line(A :: type()) -> binary() | eof.

-callback skip(A :: type(), L :: integer()) -> integer() | eof.

-spec read(type()) -> binary() | eof.
read(Reader) ->
  clj_protocol:resolve(?MODULE, read, Reader).

-spec read(type(), integer()) -> binary() | eof.
read(Reader, Length) ->
  clj_protocol:resolve(?MODULE, read, Reader, Length).

-spec read_line(type()) -> binary() | eof.
read_line(Reader) ->
  clj_protocol:resolve(?MODULE, read_line, Reader).

-spec skip(type(), integer()) -> integer() | eof.
skip(Reader, N) ->
  clj_protocol:resolve(?MODULE, skip, Reader, N).
