-module('clojerl.IRecord').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback '_'(any()) -> any().
-optional_callbacks(['_'/1]).

?SATISFIES(X) ->
  case X of
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    _ -> false
  end.
