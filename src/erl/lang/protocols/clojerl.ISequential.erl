-module('clojerl.ISequential').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export([?SATISFIES/1]).

-callback '_'(any()) -> any().

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.TransducerSeq'} -> true;
    #{?TYPE := 'clojerl.LazySeq'} -> true;
    #{?TYPE := 'clojerl.Range'} -> true;
    #{?TYPE := 'clojerl.Vector.RSeq'} -> true;
    #{?TYPE := 'clojerl.Cycle'} -> true;
    #{?TYPE := 'clojerl.List'} -> true;
    #{?TYPE := 'clojerl.Iterate'} -> true;
    #{?TYPE := 'clojerl.Vector'} -> true;
    #{?TYPE := 'clojerl.Cons'} -> true;
    #{?TYPE := 'clojerl.Repeat'} -> true;
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} -> true;
    #{?TYPE := 'clojerl.ChunkedCons'} -> true;
    #{?TYPE := _} -> false;
    ZZZ when is_bitstring(ZZZ) -> true;
    ZZZ when is_list(ZZZ) -> true;
    ZZZ when is_tuple(ZZZ) -> true;
    _ -> false
  end.
