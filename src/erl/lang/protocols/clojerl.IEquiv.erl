-module('clojerl.IEquiv').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['equiv'/2]).
-export([?SATISFIES/1]).

-callback 'equiv'(any(), any()) -> any().

'equiv'(X, Y) ->
  case clj_rt:type_module(X) of
    'erlang.Tuple' ->
      'erlang.Tuple':'equiv'(X, Y);
    'erlang.Map' ->
      'erlang.Map':'equiv'(X, Y);
    'erlang.List' ->
      'erlang.List':'equiv'(X, Y);
    'erlang.util.Date' ->
      'erlang.util.Date':'equiv'(X, Y);
    'clojerl.TransducerSeq' ->
      'clojerl.TransducerSeq':'equiv'(X, Y);
    'clojerl.reader.TaggedLiteral' ->
      'clojerl.reader.TaggedLiteral':'equiv'(X, Y);
    'clojerl.ProcessVal' ->
      'clojerl.ProcessVal':'equiv'(X, Y);
    'clojerl.Var' ->
      'clojerl.Var':'equiv'(X, Y);
    'clojerl.reader.ReaderConditional' ->
      'clojerl.reader.ReaderConditional':'equiv'(X, Y);
    'clojerl.Atom' ->
      'clojerl.Atom':'equiv'(X, Y);
    'clojerl.Symbol' ->
      'clojerl.Symbol':'equiv'(X, Y);
    'clojerl.IllegalAccessError' ->
      'clojerl.IllegalAccessError':'equiv'(X, Y);
    'clojerl.AssertionError' ->
      'clojerl.AssertionError':'equiv'(X, Y);
    'clojerl.Error' ->
      'clojerl.Error':'equiv'(X, Y);
    'clojerl.BadArgumentError' ->
      'clojerl.BadArgumentError':'equiv'(X, Y);
    'clojerl.ArityError' ->
      'clojerl.ArityError':'equiv'(X, Y);
    'clojerl.IOError' ->
      'clojerl.IOError':'equiv'(X, Y);
    'clojerl.ExceptionInfo' ->
      'clojerl.ExceptionInfo':'equiv'(X, Y);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'equiv'(X, Y);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'equiv'(X, Y);
    'clojerl.Range' ->
      'clojerl.Range':'equiv'(X, Y);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'equiv'(X, Y);
    'clojerl.Vector.RSeq' ->
      'clojerl.Vector.RSeq':'equiv'(X, Y);
    'clojerl.List' ->
      'clojerl.List':'equiv'(X, Y);
    'clojerl.Vector' ->
      'clojerl.Vector':'equiv'(X, Y);
    'clojerl.Map' ->
      'clojerl.Map':'equiv'(X, Y);
    'clojerl.Cons' ->
      'clojerl.Cons':'equiv'(X, Y);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'equiv'(X, Y);
    'clojerl.Set' ->
      'clojerl.Set':'equiv'(X, Y);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'equiv'(X, Y);
    'clojerl.SortedSet' ->
      'clojerl.SortedSet':'equiv'(X, Y);
    'clojerl.TupleChunk' ->
      'clojerl.TupleChunk':'equiv'(X, Y);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'equiv', Type)
  end.

?SATISFIES('erlang.Tuple') -> true;
?SATISFIES('erlang.Map') -> true;
?SATISFIES('erlang.List') -> true;
?SATISFIES('erlang.util.Date') -> true;
?SATISFIES('clojerl.TransducerSeq') -> true;
?SATISFIES('clojerl.reader.TaggedLiteral') -> true;
?SATISFIES('clojerl.ProcessVal') -> true;
?SATISFIES('clojerl.Var') -> true;
?SATISFIES('clojerl.reader.ReaderConditional') -> true;
?SATISFIES('clojerl.Atom') -> true;
?SATISFIES('clojerl.Symbol') -> true;
?SATISFIES('clojerl.IllegalAccessError') -> true;
?SATISFIES('clojerl.AssertionError') -> true;
?SATISFIES('clojerl.Error') -> true;
?SATISFIES('clojerl.BadArgumentError') -> true;
?SATISFIES('clojerl.ArityError') -> true;
?SATISFIES('clojerl.IOError') -> true;
?SATISFIES('clojerl.ExceptionInfo') -> true;
?SATISFIES('clojerl.LazySeq') -> true;
?SATISFIES('clojerl.SortedMap') -> true;
?SATISFIES('clojerl.Range') -> true;
?SATISFIES('clojerl.TupleMap') -> true;
?SATISFIES('clojerl.Vector.RSeq') -> true;
?SATISFIES('clojerl.List') -> true;
?SATISFIES('clojerl.Vector') -> true;
?SATISFIES('clojerl.Map') -> true;
?SATISFIES('clojerl.Cons') -> true;
?SATISFIES('clojerl.Vector.ChunkedSeq') -> true;
?SATISFIES('clojerl.Set') -> true;
?SATISFIES('clojerl.ChunkedCons') -> true;
?SATISFIES('clojerl.SortedSet') -> true;
?SATISFIES('clojerl.TupleChunk') -> true;
?SATISFIES(_) -> false.
