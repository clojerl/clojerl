%%% Code generate by scripts/generate-protocols
-module('clojerl.IVector').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback '_IVector'(any()) -> any().
-optional_callbacks(['_IVector'/1]).

-export_type([type/0]).
-type type() :: #{_ => _}.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Subvec'} ->  true;
    #{?TYPE := 'clojerl.Vector'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.Subvec' -> true;
    'clojerl.Vector' -> true;
    _ -> false
  end.
