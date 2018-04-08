-module('clojerl.IColl').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['cons'/2, 'empty'/1, '__satisfies?__'/1]).

-callback 'cons'(any(), any()) -> any().
-callback 'empty'(any()) -> any().

'cons'(Coll, Item) ->
  case clj_rt:type_module(Coll) of
    'erlang.List' ->
      'erlang.List':'cons'(Coll, Item);
    'erlang.Map' ->
      'erlang.Map':'cons'(Coll, Item);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'cons'(Coll, Item);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'cons'(Coll, Item);
    'clojerl.List' ->
      'clojerl.List':'cons'(Coll, Item);
    'clojerl.Map' ->
      'clojerl.Map':'cons'(Coll, Item);
    'clojerl.Range' ->
      'clojerl.Range':'cons'(Coll, Item);
    'clojerl.Set' ->
      'clojerl.Set':'cons'(Coll, Item);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'cons'(Coll, Item);
    'clojerl.SortedSet' ->
      'clojerl.SortedSet':'cons'(Coll, Item);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'cons'(Coll, Item);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'cons'(Coll, Item);
    'clojerl.Vector' ->
      'clojerl.Vector':'cons'(Coll, Item);
    _ ->
      clj_protocol:resolve(?MODULE, 'cons', Coll, Item)
  end.

'empty'(Coll) ->
  case clj_rt:type_module(Coll) of
    'erlang.List' ->
      'erlang.List':'empty'(Coll);
    'erlang.Map' ->
      'erlang.Map':'empty'(Coll);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'empty'(Coll);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'empty'(Coll);
    'clojerl.List' ->
      'clojerl.List':'empty'(Coll);
    'clojerl.Map' ->
      'clojerl.Map':'empty'(Coll);
    'clojerl.Range' ->
      'clojerl.Range':'empty'(Coll);
    'clojerl.Set' ->
      'clojerl.Set':'empty'(Coll);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'empty'(Coll);
    'clojerl.SortedSet' ->
      'clojerl.SortedSet':'empty'(Coll);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'empty'(Coll);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'empty'(Coll);
    'clojerl.Vector' ->
      'clojerl.Vector':'empty'(Coll);
    _ ->
      clj_protocol:resolve(?MODULE, 'empty', Coll)
  end.

?SATISFIES('erlang.List') -> true;
?SATISFIES('erlang.Map') -> true;
?SATISFIES('clojerl.ChunkedCons') -> true;
?SATISFIES('clojerl.LazySeq') -> true;
?SATISFIES('clojerl.List') -> true;
?SATISFIES('clojerl.Map') -> true;
?SATISFIES('clojerl.Range') -> true;
?SATISFIES('clojerl.Set') -> true;
?SATISFIES('clojerl.SortedMap') -> true;
?SATISFIES('clojerl.SortedSet') -> true;
?SATISFIES('clojerl.TupleMap') -> true;
?SATISFIES('clojerl.Vector.ChunkedSeq') -> true;
?SATISFIES('clojerl.Vector') -> true;
?SATISFIES(_) -> false.
