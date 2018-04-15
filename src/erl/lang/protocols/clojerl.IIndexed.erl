-module('clojerl.IIndexed').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['nth'/2, 'nth'/3]).
-export([?SATISFIES/1]).

-callback 'nth'(any(), any()) -> any().
-callback 'nth'(any(), any(), any()) -> any().

'nth'(Coll, N) ->
  case clj_rt:type_module(Coll) of
    'erlang.Tuple' ->
      'erlang.Tuple':'nth'(Coll, N);
    'clojerl.TupleChunk' ->
      'clojerl.TupleChunk':'nth'(Coll, N);
    'clojerl.Vector' ->
      'clojerl.Vector':'nth'(Coll, N);
    _ ->
      clj_protocol:resolve(?MODULE, 'nth', Coll, N)
  end.

'nth'(Coll, N, NotFound) ->
  case clj_rt:type_module(Coll) of
    'erlang.Tuple' ->
      'erlang.Tuple':'nth'(Coll, N, NotFound);
    'clojerl.TupleChunk' ->
      'clojerl.TupleChunk':'nth'(Coll, N, NotFound);
    'clojerl.Vector' ->
      'clojerl.Vector':'nth'(Coll, N, NotFound);
    _ ->
      clj_protocol:resolve(?MODULE, 'nth', Coll, N, NotFound)
  end.

?SATISFIES('erlang.Tuple') -> true;
?SATISFIES('clojerl.TupleChunk') -> true;
?SATISFIES('clojerl.Vector') -> true;
?SATISFIES(_) -> false.
