-module('erlang.io.IPushbackReader').

-export([unread/2]).

-type type() :: any().

-callback unread(A :: type(), C :: binary()) -> type().

-spec unread(type(), binary()) -> type().
unread(Reader, Ch) ->
  clj_protocol:resolve(?MODULE, unread, Reader, Ch).
