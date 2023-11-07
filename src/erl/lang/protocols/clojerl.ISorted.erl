%%% Code generate by scripts/generate-protocols
-module('clojerl.ISorted').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'comparator'(any()) -> any().
-callback 'entryKey'(any(), any()) -> any().
-callback 'seq'(any(), any()) -> any().
-callback 'seqFrom'(any(), any(), any()) -> any().
-optional_callbacks(['comparator'/1, 'entryKey'/2, 'seq'/2, 'seqFrom'/3]).

-export_type([type/0]).
-type type() :: #{_ => _}.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.SortedMap'} ->  true;
    #{?TYPE := 'clojerl.SortedSet'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.SortedMap' -> true;
    'clojerl.SortedSet' -> true;
    _ -> false
  end.
