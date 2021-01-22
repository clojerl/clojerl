%%% Code generate by scripts/generate-protocols
-module('clojerl.IType').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback '_IType'(any()) -> any().
-optional_callbacks(['_IType'/1]).

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
