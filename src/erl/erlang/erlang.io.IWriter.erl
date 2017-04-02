-module('erlang.io.IWriter').

-export([write/2, write/3]).

-callback write(A :: any(), K :: binary()) -> any().
-callback write(A :: any(), K :: binary(), L :: list()) -> any().

-spec write(any(), binary()) -> any().
write(Writer, Str) ->
  clj_protocol:resolve(?MODULE, write, Writer, Str).

-spec write(any(), binary(), any()) -> any().
write(Writer, Format, Values) ->
  clj_protocol:resolve(?MODULE, write, Writer, Format, Values).
