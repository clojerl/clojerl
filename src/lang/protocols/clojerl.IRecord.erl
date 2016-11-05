-module('clojerl.IRecord').

-include("clojerl.hrl").

-clojure(true).
-protocol(true).

-callback '_'(any()) -> ?NIL.
