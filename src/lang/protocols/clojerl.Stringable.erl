-module('clojerl.Stringable').

-clojure(true).
-protocol(true).

-export([str/1]).

-type type() :: binary().

-callback str(any()) -> type().

-spec str(any()) -> type().
str(X) -> clj_protocol:resolve(?MODULE, str, X).
