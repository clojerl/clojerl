-module('clojerl.IMap').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['keys'/1, 'vals'/1, 'without'/2]).
-export([?SATISFIES/1]).

-callback 'keys'(any()) -> any().
-callback 'vals'(any()) -> any().
-callback 'without'(any(), any()) -> any().

'keys'(Map) ->
  case clj_rt:type_module(Map) of
    'erlang.Map' ->
      'erlang.Map':'keys'(Map);
    'clojerl.Map' ->
      'clojerl.Map':'keys'(Map);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'keys'(Map);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'keys'(Map);
    _ ->
      clj_protocol:resolve(?MODULE, 'keys', Map)
  end.

'vals'(Map) ->
  case clj_rt:type_module(Map) of
    'erlang.Map' ->
      'erlang.Map':'vals'(Map);
    'clojerl.Map' ->
      'clojerl.Map':'vals'(Map);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'vals'(Map);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'vals'(Map);
    _ ->
      clj_protocol:resolve(?MODULE, 'vals', Map)
  end.

'without'(Map, Key) ->
  case clj_rt:type_module(Map) of
    'erlang.Map' ->
      'erlang.Map':'without'(Map, Key);
    'clojerl.Map' ->
      'clojerl.Map':'without'(Map, Key);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'without'(Map, Key);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'without'(Map, Key);
    _ ->
      clj_protocol:resolve(?MODULE, 'without', Map, Key)
  end.

?SATISFIES('erlang.Map') -> true;
?SATISFIES('clojerl.Map') -> true;
?SATISFIES('clojerl.SortedMap') -> true;
?SATISFIES('clojerl.TupleMap') -> true;
?SATISFIES(_) -> false.
