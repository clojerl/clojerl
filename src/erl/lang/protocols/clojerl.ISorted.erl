-module('clojerl.ISorted').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['__satisfies?__'/1]).

-callback '_'(any()) -> any().

?SATISFIES('clojerl.SortedMap') -> true;
?SATISFIES('clojerl.SortedSet') -> true;
?SATISFIES(_) -> false.
