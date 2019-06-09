-module('clojerl.IIndexed').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['nth'/2, 'nth'/3]).
-export([?SATISFIES/1]).

-callback 'nth'(any(), any()) -> any().
-callback 'nth'(any(), any(), any()) -> any().

'nth'(Coll, N) ->
  case Coll of
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'nth'(Coll, N);
    #{?TYPE := 'clojerl.TupleChunk'} ->
      'clojerl.TupleChunk':'nth'(Coll, N);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'nth', Coll);
    ZZZ when is_tuple(ZZZ) ->
      'erlang.Tuple':'nth'(Coll, N);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'nth', Coll);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'nth', Coll)
  end.

'nth'(Coll, N, NotFound) ->
  case Coll of
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'nth'(Coll, N, NotFound);
    #{?TYPE := 'clojerl.TupleChunk'} ->
      'clojerl.TupleChunk':'nth'(Coll, N, NotFound);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'nth', Coll);
    ZZZ when is_tuple(ZZZ) ->
      'erlang.Tuple':'nth'(Coll, N, NotFound);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'nth', Coll);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'nth', Coll)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Vector'} -> true;
    #{?TYPE := 'clojerl.TupleChunk'} -> true;
    #{?TYPE := _} -> false;
    ZZZ when is_tuple(ZZZ) -> true;
    ?NIL -> false;
    _ -> false
  end.
