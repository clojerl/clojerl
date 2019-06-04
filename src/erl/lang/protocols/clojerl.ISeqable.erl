-module('clojerl.ISeqable').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['seq'/1, 'to_list'/1]).
-export([?SATISFIES/1]).

-callback 'seq'(any()) -> any().
-callback 'to_list'(any()) -> any().

'seq'(X) ->
  case X of
    #{?TYPE := 'clojerl.TransducerSeq'} ->
      'clojerl.TransducerSeq':'seq'(X);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'seq'(X);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'seq'(X);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'seq'(X);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'seq'(X);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'seq'(X);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'seq'(X);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'seq'(X);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'seq'(X);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'seq'(X);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'seq'(X);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'seq'(X);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'seq'(X);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'seq'(X);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'seq'(X);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'seq'(X);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'seq'(X);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'seq', X);
    ZZZ when is_binary(ZZZ) ->
      'clojerl.String':'seq'(X);
    ZZZ when is_bitstring(ZZZ) ->
      'clojerl.BitString':'seq'(X);
    ZZZ when is_list(ZZZ) ->
      'erlang.List':'seq'(X);
    ZZZ when is_map(ZZZ) ->
      'erlang.Map':'seq'(X);
    ZZZ when is_tuple(ZZZ) ->
      'erlang.Tuple':'seq'(X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'seq', X)
  end.

'to_list'(X) ->
  case X of
    #{?TYPE := 'clojerl.TransducerSeq'} ->
      'clojerl.TransducerSeq':'to_list'(X);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'to_list'(X);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'to_list'(X);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'to_list'(X);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'to_list'(X);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'to_list'(X);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'to_list'(X);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'to_list'(X);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'to_list'(X);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'to_list'(X);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'to_list'(X);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'to_list'(X);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'to_list'(X);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'to_list'(X);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'to_list'(X);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'to_list'(X);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'to_list'(X);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'to_list', X);
    ZZZ when is_binary(ZZZ) ->
      'clojerl.String':'to_list'(X);
    ZZZ when is_bitstring(ZZZ) ->
      'clojerl.BitString':'to_list'(X);
    ZZZ when is_list(ZZZ) ->
      'erlang.List':'to_list'(X);
    ZZZ when is_map(ZZZ) ->
      'erlang.Map':'to_list'(X);
    ZZZ when is_tuple(ZZZ) ->
      'erlang.Tuple':'to_list'(X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'to_list', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.TransducerSeq'} -> true;
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
    #{?TYPE := _} -> false;
    ZZZ when is_binary(ZZZ) -> true;
    ZZZ when is_bitstring(ZZZ) -> true;
    ZZZ when is_list(ZZZ) -> true;
    ZZZ when is_map(ZZZ) -> true;
    ZZZ when is_tuple(ZZZ) -> true;
    _ -> false
  end.
