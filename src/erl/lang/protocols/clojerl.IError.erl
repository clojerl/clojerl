-module('clojerl.IError').

-clojure(true).
-protocol(true).

-export([message/1]).

-type type() :: any().

-callback message(A :: type()) -> binary().

-spec message(type()) -> binary().
message(Error) ->
  clj_protocol:resolve(?MODULE, message, Error).
