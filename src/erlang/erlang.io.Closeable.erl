-module('erlang.io.Closeable').

-include("clojerl.hrl").

-export([close/1]).

-type type() :: any().

-callback close(C :: type()) -> ?NIL.

-spec close(type()) -> ?NIL.
close(X) ->
  clj_protocol:resolve(?MODULE, close, X).
