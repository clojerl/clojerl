-module('clojerl.ICounted').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['count'/1]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'count'(any()) -> any().

'count'(Seq) ->
  case Seq of
    #{?TYPE := 'clojerl.TupleChunk'} ->
      'clojerl.TupleChunk':'count'(Seq);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'count'(Seq);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'count'(Seq);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'count'(Seq);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'count'(Seq);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'count'(Seq);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'count'(Seq);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'count'(Seq);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'count'(Seq);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'count'(Seq);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'count'(Seq);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'count'(Seq);
    #{?TYPE := 'erlang.io.StringWriter'} ->
      'erlang.io.StringWriter':'count'(Seq);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'count'(Seq);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'count'(Seq);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'count'(Seq);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'count'(Seq);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'count'(Seq);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'count', Seq);
    X_ when is_binary(X_) ->
      'clojerl.String':'count'(Seq);
    X_ when is_bitstring(X_) ->
      'clojerl.BitString':'count'(Seq);
    X_ when is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'count', Seq);
    X_ when is_list(X_) ->
      'erlang.List':'count'(Seq);
    X_ when is_map(X_) ->
      'erlang.Map':'count'(Seq);
    X_ when is_tuple(X_) ->
      'erlang.Tuple':'count'(Seq);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'count', Seq);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'count', Seq)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.TupleChunk'} ->  true;
    #{?TYPE := 'clojerl.Iterate'} ->  true;
    #{?TYPE := 'clojerl.Range'} ->  true;
    #{?TYPE := 'clojerl.SortedMap'} ->  true;
    #{?TYPE := 'clojerl.Set'} ->  true;
    #{?TYPE := 'clojerl.Map'} ->  true;
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->  true;
    #{?TYPE := 'clojerl.List'} ->  true;
    #{?TYPE := 'clojerl.LazySeq'} ->  true;
    #{?TYPE := 'clojerl.Vector'} ->  true;
    #{?TYPE := 'clojerl.SortedSet'} ->  true;
    #{?TYPE := 'clojerl.Vector.RSeq'} ->  true;
    #{?TYPE := 'erlang.io.StringWriter'} ->  true;
    #{?TYPE := 'clojerl.Cycle'} ->  true;
    #{?TYPE := 'clojerl.Repeat'} ->  true;
    #{?TYPE := 'clojerl.Cons'} ->  true;
    #{?TYPE := 'clojerl.ChunkedCons'} ->  true;
    #{?TYPE := 'clojerl.TupleMap'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when is_binary(X_) ->  true;
    X_ when is_bitstring(X_) ->  true;
    X_ when is_boolean(X_) ->  false;
    X_ when is_list(X_) ->  true;
    X_ when is_map(X_) ->  true;
    X_ when is_tuple(X_) ->  true;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.TupleChunk' -> true;
    'clojerl.Iterate' -> true;
    'clojerl.Range' -> true;
    'clojerl.SortedMap' -> true;
    'clojerl.Set' -> true;
    'clojerl.Map' -> true;
    'clojerl.Vector.ChunkedSeq' -> true;
    'clojerl.List' -> true;
    'clojerl.LazySeq' -> true;
    'clojerl.Vector' -> true;
    'clojerl.SortedSet' -> true;
    'clojerl.Vector.RSeq' -> true;
    'erlang.io.StringWriter' -> true;
    'clojerl.Cycle' -> true;
    'clojerl.Repeat' -> true;
    'clojerl.Cons' -> true;
    'clojerl.ChunkedCons' -> true;
    'clojerl.TupleMap' -> true;
    'clojerl.String' -> true;
    'clojerl.BitString' -> true;
    'erlang.List' -> true;
    'erlang.Map' -> true;
    'erlang.Tuple' -> true;
    _ -> false
  end.
