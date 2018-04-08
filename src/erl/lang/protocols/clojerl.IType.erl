-module('clojerl.IType').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['__satisfies?__'/1]).

-callback '_'(any()) -> any().

?SATISFIES(_) -> false.
