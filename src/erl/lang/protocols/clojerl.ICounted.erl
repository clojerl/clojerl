-module('clojerl.ICounted').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['count'/1]).
-export([?SATISFIES/1]).

-callback 'count'(any()) -> any().

'count'(Seq) ->
  case Seq of
    #{?TYPE := 'erlang.io.StringWriter'} ->
      'erlang.io.StringWriter':'count'(Seq);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'count'(Seq);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'count'(Seq);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'count'(Seq);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'count'(Seq);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'count'(Seq);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'count'(Seq);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'count'(Seq);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'count'(Seq);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'count'(Seq);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'count'(Seq);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'count'(Seq);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'count'(Seq);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'count'(Seq);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'count'(Seq);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'count'(Seq);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'count'(Seq);
    #{?TYPE := 'clojerl.TupleChunk'} ->
      'clojerl.TupleChunk':'count'(Seq);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'count', Seq);
    ZZZ when is_binary(ZZZ) ->
      'clojerl.String':'count'(Seq);
    ZZZ when is_bitstring(ZZZ) ->
      'clojerl.BitString':'count'(Seq);
    ZZZ when is_list(ZZZ) ->
      'erlang.List':'count'(Seq);
    ZZZ when is_map(ZZZ) ->
      'erlang.Map':'count'(Seq);
    ZZZ when is_tuple(ZZZ) ->
      'erlang.Tuple':'count'(Seq);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'count', Seq);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'count', Seq)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'erlang.io.StringWriter'} -> true;
    #{?TYPE := 'clojerl.LazySeq'} -> true;
    #{?TYPE := 'clojerl.SortedMap'} -> true;
    #{?TYPE := 'clojerl.Range'} -> true;
    #{?TYPE := 'clojerl.TupleMap'} -> true;
    #{?TYPE := 'clojerl.Vector.RSeq'} -> true;
    #{?TYPE := 'clojerl.Cycle'} -> true;
    #{?TYPE := 'clojerl.List'} -> true;
    #{?TYPE := 'clojerl.Iterate'} -> true;
    #{?TYPE := 'clojerl.Vector'} -> true;
    #{?TYPE := 'clojerl.Map'} -> true;
    #{?TYPE := 'clojerl.Cons'} -> true;
    #{?TYPE := 'clojerl.Repeat'} -> true;
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} -> true;
    #{?TYPE := 'clojerl.Set'} -> true;
    #{?TYPE := 'clojerl.ChunkedCons'} -> true;
    #{?TYPE := 'clojerl.SortedSet'} -> true;
    #{?TYPE := 'clojerl.TupleChunk'} -> true;
    #{?TYPE := _} -> false;
    ZZZ when is_binary(ZZZ) -> true;
    ZZZ when is_bitstring(ZZZ) -> true;
    ZZZ when is_list(ZZZ) -> true;
    ZZZ when is_map(ZZZ) -> true;
    ZZZ when is_tuple(ZZZ) -> true;
    ?NIL -> false;
    _ -> false
  end.
