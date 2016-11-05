-module('erlang.io.Closeable').

-include("clojerl.hrl").

-export([close/1]).

-type type() :: any().

-callback close(C :: type()) -> ?NIL.

-spec close(type()) -> ?NIL.
close(X) ->
  'clojerl.protocol':resolve(?MODULE, close, X).
