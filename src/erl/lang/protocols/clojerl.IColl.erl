-module('clojerl.IColl').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['cons'/2, 'empty'/1]).
-export([?SATISFIES/1]).

-callback 'cons'(any(), any()) -> any().
-callback 'empty'(any()) -> any().

'cons'(Coll, Item) ->
  case Coll of
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'cons'(Coll, Item);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'cons', Coll);
    ZZZ when is_list(ZZZ) ->
      'erlang.List':'cons'(Coll, Item);
    ZZZ when is_map(ZZZ) ->
      'erlang.Map':'cons'(Coll, Item);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'cons', Coll);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'cons', Coll)
  end.

'empty'(Coll) ->
  case Coll of
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'empty'(Coll);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'empty'(Coll);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'empty'(Coll);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'empty'(Coll);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'empty'(Coll);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'empty'(Coll);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'empty'(Coll);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'empty'(Coll);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'empty'(Coll);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'empty'(Coll);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'empty'(Coll);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'empty'(Coll);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'empty'(Coll);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'empty'(Coll);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'empty'(Coll);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'empty'(Coll);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'empty', Coll);
    ZZZ when is_list(ZZZ) ->
      'erlang.List':'empty'(Coll);
    ZZZ when is_map(ZZZ) ->
      'erlang.Map':'empty'(Coll);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'empty', Coll);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'empty', Coll)
  end.

?SATISFIES(X) ->
  case X of
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
    ZZZ when is_list(ZZZ) -> true;
    ZZZ when is_map(ZZZ) -> true;
    ?NIL -> false;
    _ -> false
  end.
