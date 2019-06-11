-module('clojerl.ISeq').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['first'/1, 'next'/1, 'more'/1]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'first'(any()) -> any().
-callback 'next'(any()) -> any().
-callback 'more'(any()) -> any().

'first'(Seq) ->
  case Seq of
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'first'(Seq);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'first'(Seq);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'first'(Seq);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'first'(Seq);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'first'(Seq);
    #{?TYPE := 'clojerl.TransducerSeq'} ->
      'clojerl.TransducerSeq':'first'(Seq);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'first'(Seq);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'first'(Seq);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'first'(Seq);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'first'(Seq);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'first'(Seq);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'first', Seq);
    X_ when is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'first', Seq);
    X_ when is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'first', Seq);
    X_ when is_list(X_) ->
      'erlang.List':'first'(Seq);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'first', Seq);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'first', Seq)
  end.

'next'(Seq) ->
  case Seq of
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'next'(Seq);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'next'(Seq);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'next'(Seq);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'next'(Seq);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'next'(Seq);
    #{?TYPE := 'clojerl.TransducerSeq'} ->
      'clojerl.TransducerSeq':'next'(Seq);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'next'(Seq);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'next'(Seq);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'next'(Seq);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'next'(Seq);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'next'(Seq);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'next', Seq);
    X_ when is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'next', Seq);
    X_ when is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'next', Seq);
    X_ when is_list(X_) ->
      'erlang.List':'next'(Seq);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'next', Seq);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'next', Seq)
  end.

'more'(Seq) ->
  case Seq of
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'more'(Seq);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'more'(Seq);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'more'(Seq);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'more'(Seq);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'more'(Seq);
    #{?TYPE := 'clojerl.TransducerSeq'} ->
      'clojerl.TransducerSeq':'more'(Seq);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'more'(Seq);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'more'(Seq);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'more'(Seq);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'more'(Seq);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'more'(Seq);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'more', Seq);
    X_ when is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'more', Seq);
    X_ when is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'more', Seq);
    X_ when is_list(X_) ->
      'erlang.List':'more'(Seq);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'more', Seq);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'more', Seq)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Iterate'} ->  true;
    #{?TYPE := 'clojerl.Range'} ->  true;
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->  true;
    #{?TYPE := 'clojerl.List'} ->  true;
    #{?TYPE := 'clojerl.LazySeq'} ->  true;
    #{?TYPE := 'clojerl.TransducerSeq'} ->  true;
    #{?TYPE := 'clojerl.Vector.RSeq'} ->  true;
    #{?TYPE := 'clojerl.Cycle'} ->  true;
    #{?TYPE := 'clojerl.Repeat'} ->  true;
    #{?TYPE := 'clojerl.Cons'} ->  true;
    #{?TYPE := 'clojerl.ChunkedCons'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when is_binary(X_) ->  false;
    X_ when is_boolean(X_) ->  false;
    X_ when is_list(X_) ->  true;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.Iterate' -> true;
    'clojerl.Range' -> true;
    'clojerl.Vector.ChunkedSeq' -> true;
    'clojerl.List' -> true;
    'clojerl.LazySeq' -> true;
    'clojerl.TransducerSeq' -> true;
    'clojerl.Vector.RSeq' -> true;
    'clojerl.Cycle' -> true;
    'clojerl.Repeat' -> true;
    'clojerl.Cons' -> true;
    'clojerl.ChunkedCons' -> true;
    'erlang.List' -> true;
    _ -> false
  end.
