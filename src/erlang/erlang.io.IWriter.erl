-module('erlang.io.IWriter').

-export([write/2, write/3]).

-type type() :: any().

-callback write(A :: type(), K :: binary()) -> type().
-callback write(A :: type(), K :: binary(), L :: list()) -> type().

-spec write(type(), binary()) -> type().
write(Writer, Str) ->
  'clojerl.protocol':resolve(?MODULE, write, Writer, Str).

-spec write(type(), binary(), any()) -> type().
write(Writer, Format, Values) ->
  'clojerl.protocol':resolve(?MODULE, write, Writer, Format, Values).
