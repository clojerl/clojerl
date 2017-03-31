-module('clojerl.IEquiv').

-clojure(true).
-protocol(true).

-export([equiv/2]).

-type type() :: any().

-callback equiv(type(), type()) -> boolean().

-spec equiv(type(), type()) -> type().
equiv(X, Y) ->
  clj_protocol:resolve(?MODULE, equiv, X, Y).
