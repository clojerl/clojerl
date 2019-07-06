-module('clojerl.IAssociative').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['contains_key'/2, 'entry_at'/2, 'assoc'/3]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'contains_key'(any(), any()) -> any().
-callback 'entry_at'(any(), any()) -> any().
-callback 'assoc'(any(), any(), any()) -> any().

'contains_key'(Assoc, Key) ->
  case Assoc of
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'contains_key'(Assoc, Key);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'contains_key'(Assoc, Key);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'contains_key'(Assoc, Key);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'contains_key'(Assoc, Key);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'contains_key', Assoc);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'contains_key', Assoc);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'contains_key', Assoc);
    X_ when erlang:is_map(X_) ->
      'erlang.Map':'contains_key'(Assoc, Key);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'contains_key', Assoc);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'contains_key', Assoc)
  end.

'entry_at'(Assoc, Key) ->
  case Assoc of
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'entry_at'(Assoc, Key);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'entry_at'(Assoc, Key);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'entry_at'(Assoc, Key);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'entry_at'(Assoc, Key);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'entry_at', Assoc);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'entry_at', Assoc);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'entry_at', Assoc);
    X_ when erlang:is_map(X_) ->
      'erlang.Map':'entry_at'(Assoc, Key);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'entry_at', Assoc);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'entry_at', Assoc)
  end.

'assoc'(Assoc, Key, Value) ->
  case Assoc of
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'assoc'(Assoc, Key, Value);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'assoc'(Assoc, Key, Value);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'assoc'(Assoc, Key, Value);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'assoc'(Assoc, Key, Value);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'assoc', Assoc);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'assoc', Assoc);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'assoc', Assoc);
    X_ when erlang:is_map(X_) ->
      'erlang.Map':'assoc'(Assoc, Key, Value);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'assoc', Assoc);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'assoc', Assoc)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.SortedMap'} ->  true;
    #{?TYPE := 'clojerl.Map'} ->  true;
    #{?TYPE := 'clojerl.Vector'} ->  true;
    #{?TYPE := 'clojerl.TupleMap'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    X_ when erlang:is_map(X_) ->  true;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.SortedMap' -> true;
    'clojerl.Map' -> true;
    'clojerl.Vector' -> true;
    'clojerl.TupleMap' -> true;
    'erlang.Map' -> true;
    _ -> false
  end.
