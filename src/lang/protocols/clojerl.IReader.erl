-module('clojerl.IReader').

-export([read/1, read/2, read_line/1, skip/2]).

-type type() :: any().

-callback 'clojerl.IReader.read'(A :: type()) ->
  binary() | eof.

-callback 'clojerl.IReader.read'(A :: type(), L :: integer()) ->
  binary() | eof.

-callback 'clojerl.IReader.read_line'(A :: type()) ->
  binary() | eof.

-callback 'clojerl.IReader.skip'(A :: type(), L :: integer()) ->
  integer() | eof.

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
