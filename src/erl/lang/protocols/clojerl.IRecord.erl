-module('clojerl.IRecord').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export([?SATISFIES/1]).

-callback '_'(any()) -> any().

?SATISFIES(X) ->
  case X of
    #{?TYPE := _} -> false;
    _ -> false
  end.
