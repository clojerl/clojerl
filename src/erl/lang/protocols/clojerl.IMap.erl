-module('clojerl.IMap').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['keys'/1, 'vals'/1, 'without'/2]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'keys'(any()) -> any().
-callback 'vals'(any()) -> any().
-callback 'without'(any(), any()) -> any().
-optional_callbacks(['keys'/1, 'vals'/1, 'without'/2]).

'keys'(Map) ->
  case Map of
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'keys'(Map);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'keys'(Map);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'keys'(Map);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'keys', Map);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'keys', Map);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'keys', Map);
    X_ when erlang:is_map(X_) ->
      'erlang.Map':'keys'(Map);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'keys', Map);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'keys', Map)
  end.

'vals'(Map) ->
  case Map of
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'vals'(Map);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'vals'(Map);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'vals'(Map);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'vals', Map);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'vals', Map);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'vals', Map);
    X_ when erlang:is_map(X_) ->
      'erlang.Map':'vals'(Map);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'vals', Map);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'vals', Map)
  end.

'without'(Map, Key) ->
  case Map of
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'without'(Map, Key);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'without'(Map, Key);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'without'(Map, Key);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'without', Map);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'without', Map);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'without', Map);
    X_ when erlang:is_map(X_) ->
      'erlang.Map':'without'(Map, Key);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'without', Map);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'without', Map)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Map'} ->  true;
    #{?TYPE := 'clojerl.SortedMap'} ->  true;
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
    'clojerl.Map' -> true;
    'clojerl.SortedMap' -> true;
    'clojerl.TupleMap' -> true;
    'erlang.Map' -> true;
    _ -> false
  end.
