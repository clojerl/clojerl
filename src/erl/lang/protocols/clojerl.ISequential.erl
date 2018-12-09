-module('clojerl.ISequential').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export([?SATISFIES/1]).

-callback '_'(any()) -> any().

?SATISFIES('erlang.Tuple') -> true;
?SATISFIES('erlang.List') -> true;
?SATISFIES('clojerl.BitString') -> true;
?SATISFIES('clojerl.LazySeq') -> true;
?SATISFIES('clojerl.Range') -> true;
?SATISFIES('clojerl.Vector.RSeq') -> true;
?SATISFIES('clojerl.List') -> true;
?SATISFIES('clojerl.Vector') -> true;
?SATISFIES('clojerl.Cons') -> true;
?SATISFIES('clojerl.Vector.ChunkedSeq') -> true;
?SATISFIES('clojerl.ChunkedCons') -> true;
?SATISFIES(_) -> false.
