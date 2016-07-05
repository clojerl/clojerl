-module('erlang.io.IReader').

-export([read/1, read/2, read_line/1, skip/2, unread/2]).

-type type() :: any().

-callback read(A :: type()) -> binary() | eof.

-callback read(A :: type(), L :: integer()) -> binary() | eof.

-callback read_line(A :: type()) -> binary() | eof.

-callback skip(A :: type(), L :: integer()) -> integer() | eof.

-callback unread(A :: type(), C :: binary()) -> type().

-spec read(type()) -> binary() | eof.
read(Reader) ->
  'clojerl.protocol':resolve(?MODULE, read, [Reader]).

-spec read(type(), integer()) -> binary() | eof.
read(Reader, Length) ->
  'clojerl.protocol':resolve(?MODULE, read, [Reader, Length]).

-spec read_line(type()) -> binary() | eof.
read_line(Reader) ->
  'clojerl.protocol':resolve(?MODULE, read_line, [Reader]).

-spec skip(type(), integer()) -> integer() | eof.
skip(Reader, N) ->
  'clojerl.protocol':resolve(?MODULE, skip, [Reader, N]).

-spec unread(type(), binary()) -> type().
unread(Reader, Ch) ->
  'clojerl.protocol':resolve(?MODULE, unread, [Reader, Ch]).
