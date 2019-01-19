-module('clojerl.ISeq').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['first'/1, 'next'/1, 'more'/1]).
-export([?SATISFIES/1]).

-callback 'first'(any()) -> any().
-callback 'next'(any()) -> any().
-callback 'more'(any()) -> any().

'first'(Seq) ->
  case clj_rt:type_module(Seq) of
    'erlang.List' ->
      'erlang.List':'first'(Seq);
    'clojerl.TransducerSeq' ->
      'clojerl.TransducerSeq':'first'(Seq);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'first'(Seq);
    'clojerl.Range' ->
      'clojerl.Range':'first'(Seq);
    'clojerl.Vector.RSeq' ->
      'clojerl.Vector.RSeq':'first'(Seq);
    'clojerl.List' ->
      'clojerl.List':'first'(Seq);
    'clojerl.Cons' ->
      'clojerl.Cons':'first'(Seq);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'first'(Seq);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'first'(Seq);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'first', Type)
  end.

'next'(Seq) ->
  case clj_rt:type_module(Seq) of
    'erlang.List' ->
      'erlang.List':'next'(Seq);
    'clojerl.TransducerSeq' ->
      'clojerl.TransducerSeq':'next'(Seq);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'next'(Seq);
    'clojerl.Range' ->
      'clojerl.Range':'next'(Seq);
    'clojerl.Vector.RSeq' ->
      'clojerl.Vector.RSeq':'next'(Seq);
    'clojerl.List' ->
      'clojerl.List':'next'(Seq);
    'clojerl.Cons' ->
      'clojerl.Cons':'next'(Seq);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'next'(Seq);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'next'(Seq);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'next', Type)
  end.

'more'(Seq) ->
  case clj_rt:type_module(Seq) of
    'erlang.List' ->
      'erlang.List':'more'(Seq);
    'clojerl.TransducerSeq' ->
      'clojerl.TransducerSeq':'more'(Seq);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'more'(Seq);
    'clojerl.Range' ->
      'clojerl.Range':'more'(Seq);
    'clojerl.Vector.RSeq' ->
      'clojerl.Vector.RSeq':'more'(Seq);
    'clojerl.List' ->
      'clojerl.List':'more'(Seq);
    'clojerl.Cons' ->
      'clojerl.Cons':'more'(Seq);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'more'(Seq);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'more'(Seq);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'more', Type)
  end.

?SATISFIES('erlang.List') -> true;
?SATISFIES('clojerl.TransducerSeq') -> true;
?SATISFIES('clojerl.LazySeq') -> true;
?SATISFIES('clojerl.Range') -> true;
?SATISFIES('clojerl.Vector.RSeq') -> true;
?SATISFIES('clojerl.List') -> true;
?SATISFIES('clojerl.Cons') -> true;
?SATISFIES('clojerl.Vector.ChunkedSeq') -> true;
?SATISFIES('clojerl.ChunkedCons') -> true;
?SATISFIES(_) -> false.
