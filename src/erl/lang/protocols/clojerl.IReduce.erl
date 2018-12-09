-module('clojerl.IReduce').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['reduce'/2, 'reduce'/3]).
-export([?SATISFIES/1]).

-callback 'reduce'(any(), any()) -> any().
-callback 'reduce'(any(), any(), any()) -> any().

'reduce'(Coll, Fun) ->
  case clj_rt:type_module(Coll) of
    'erlang.List' ->
      'erlang.List':'reduce'(Coll, Fun);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'reduce'(Coll, Fun);
    'clojerl.Range' ->
      'clojerl.Range':'reduce'(Coll, Fun);
    'clojerl.List' ->
      'clojerl.List':'reduce'(Coll, Fun);
    'clojerl.Vector' ->
      'clojerl.Vector':'reduce'(Coll, Fun);
    'clojerl.Cons' ->
      'clojerl.Cons':'reduce'(Coll, Fun);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'reduce'(Coll, Fun);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'reduce'(Coll, Fun);
    'clojerl.TupleChunk' ->
      'clojerl.TupleChunk':'reduce'(Coll, Fun);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'reduce', Type)
  end.

'reduce'(Coll, Fun, Init) ->
  case clj_rt:type_module(Coll) of
    'erlang.List' ->
      'erlang.List':'reduce'(Coll, Fun, Init);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'reduce'(Coll, Fun, Init);
    'clojerl.Range' ->
      'clojerl.Range':'reduce'(Coll, Fun, Init);
    'clojerl.List' ->
      'clojerl.List':'reduce'(Coll, Fun, Init);
    'clojerl.Vector' ->
      'clojerl.Vector':'reduce'(Coll, Fun, Init);
    'clojerl.Cons' ->
      'clojerl.Cons':'reduce'(Coll, Fun, Init);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'reduce'(Coll, Fun, Init);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'reduce'(Coll, Fun, Init);
    'clojerl.TupleChunk' ->
      'clojerl.TupleChunk':'reduce'(Coll, Fun, Init);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'reduce', Type)
  end.

?SATISFIES('erlang.List') -> true;
?SATISFIES('clojerl.LazySeq') -> true;
?SATISFIES('clojerl.Range') -> true;
?SATISFIES('clojerl.List') -> true;
?SATISFIES('clojerl.Vector') -> true;
?SATISFIES('clojerl.Cons') -> true;
?SATISFIES('clojerl.Vector.ChunkedSeq') -> true;
?SATISFIES('clojerl.ChunkedCons') -> true;
?SATISFIES('clojerl.TupleChunk') -> true;
?SATISFIES(_) -> false.
