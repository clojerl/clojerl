-module('clojerl.IMap').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['keys'/1, 'vals'/1, 'without'/2]).
-export([?SATISFIES/1]).

-callback 'keys'(any()) -> any().
-callback 'vals'(any()) -> any().
-callback 'without'(any(), any()) -> any().

'keys'(Map) ->
  case Map of
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'keys'(Map);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'keys'(Map);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'keys'(Map);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'keys', Map);
    ZZZ when is_map(ZZZ) ->
      'erlang.Map':'keys'(Map);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'keys', Map);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'keys', Map)
  end.

'vals'(Map) ->
  case Map of
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'vals'(Map);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'vals'(Map);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'vals'(Map);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'vals', Map);
    ZZZ when is_map(ZZZ) ->
      'erlang.Map':'vals'(Map);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'vals', Map);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'vals', Map)
  end.

'without'(Map, Key) ->
  case Map of
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'without'(Map, Key);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'without'(Map, Key);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'without'(Map, Key);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'without', Map);
    ZZZ when is_map(ZZZ) ->
      'erlang.Map':'without'(Map, Key);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'without', Map);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'without', Map)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.SortedMap'} -> true;
    #{?TYPE := 'clojerl.TupleMap'} -> true;
    #{?TYPE := 'clojerl.Map'} -> true;
    #{?TYPE := _} -> false;
    ZZZ when is_map(ZZZ) -> true;
    ?NIL -> false;
    _ -> false
  end.
