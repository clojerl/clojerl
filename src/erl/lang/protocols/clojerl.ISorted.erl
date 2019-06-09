-module('clojerl.ISorted').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export([?SATISFIES/1]).

-callback '_'(any()) -> any().

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.SortedMap'} -> true;
    #{?TYPE := 'clojerl.SortedSet'} -> true;
    #{?TYPE := _} -> false;
    ?NIL -> false;
    _ -> false
  end.
