-module('clojerl.ISeq').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['first'/1, 'next'/1, 'more'/1, '__satisfies?__'/1]).

-callback 'first'(any()) -> any().
-callback 'next'(any()) -> any().
-callback 'more'(any()) -> any().

'first'(Seq) ->
  case clj_rt:type_module(Seq) of
    'erlang.List' ->
      'erlang.List':'first'(Seq);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'first'(Seq);
    'clojerl.Cons' ->
      'clojerl.Cons':'first'(Seq);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'first'(Seq);
    'clojerl.List' ->
      'clojerl.List':'first'(Seq);
    'clojerl.Range' ->
      'clojerl.Range':'first'(Seq);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'first'(Seq);
    _ ->
      clj_protocol:resolve(?MODULE, 'first', Seq)
  end.

'next'(Seq) ->
  case clj_rt:type_module(Seq) of
    'erlang.List' ->
      'erlang.List':'next'(Seq);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'next'(Seq);
    'clojerl.Cons' ->
      'clojerl.Cons':'next'(Seq);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'next'(Seq);
    'clojerl.List' ->
      'clojerl.List':'next'(Seq);
    'clojerl.Range' ->
      'clojerl.Range':'next'(Seq);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'next'(Seq);
    _ ->
      clj_protocol:resolve(?MODULE, 'next', Seq)
  end.

'more'(Seq) ->
  case clj_rt:type_module(Seq) of
    'erlang.List' ->
      'erlang.List':'more'(Seq);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'more'(Seq);
    'clojerl.Cons' ->
      'clojerl.Cons':'more'(Seq);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'more'(Seq);
    'clojerl.List' ->
      'clojerl.List':'more'(Seq);
    'clojerl.Range' ->
      'clojerl.Range':'more'(Seq);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'more'(Seq);
    _ ->
      clj_protocol:resolve(?MODULE, 'more', Seq)
  end.

?SATISFIES('erlang.List') -> true;
?SATISFIES('clojerl.ChunkedCons') -> true;
?SATISFIES('clojerl.Cons') -> true;
?SATISFIES('clojerl.LazySeq') -> true;
?SATISFIES('clojerl.List') -> true;
?SATISFIES('clojerl.Range') -> true;
?SATISFIES('clojerl.Vector.ChunkedSeq') -> true;
?SATISFIES(_) -> false.
