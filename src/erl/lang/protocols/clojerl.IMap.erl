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
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'keys'(Map);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'keys'(Map);
    'clojerl.Map' ->
      'clojerl.Map':'keys'(Map);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'keys', Type)
  end.

'vals'(Map) ->
  case clj_rt:type_module(Map) of
    'erlang.Map' ->
      'erlang.Map':'vals'(Map);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'vals'(Map);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'vals'(Map);
    'clojerl.Map' ->
      'clojerl.Map':'vals'(Map);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'vals', Type)
  end.

'without'(Map, Key) ->
  case clj_rt:type_module(Map) of
    'erlang.Map' ->
      'erlang.Map':'without'(Map, Key);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'without'(Map, Key);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'without'(Map, Key);
    'clojerl.Map' ->
      'clojerl.Map':'without'(Map, Key);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'without', Type)
  end.

?SATISFIES('erlang.Map') -> true;
?SATISFIES('clojerl.SortedMap') -> true;
?SATISFIES('clojerl.TupleMap') -> true;
?SATISFIES('clojerl.Map') -> true;
?SATISFIES(_) -> false.
