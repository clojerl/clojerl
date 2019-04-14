-module('clojerl.IHash').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['hash'/1]).
-export([?SATISFIES/1]).

-callback 'hash'(any()) -> any().

'hash'(X) ->
  case clj_rt:type_module(X) of
    'erlang.Tuple' ->
      'erlang.Tuple':'hash'(X);
    'erlang.Map' ->
      'erlang.Map':'hash'(X);
    'erlang.Fn' ->
      'erlang.Fn':'hash'(X);
    'erlang.util.UUID' ->
      'erlang.util.UUID':'hash'(X);
    'erlang.Reference' ->
      'erlang.Reference':'hash'(X);
    'erlang.List' ->
      'erlang.List':'hash'(X);
    'erlang.Process' ->
      'erlang.Process':'hash'(X);
    'erlang.util.Regex' ->
      'erlang.util.Regex':'hash'(X);
    'erlang.Type' ->
      'erlang.Type':'hash'(X);
    'erlang.Port' ->
      'erlang.Port':'hash'(X);
    'erlang.util.Date' ->
      'erlang.util.Date':'hash'(X);
    'clojerl.Reduced' ->
      'clojerl.Reduced':'hash'(X);
    'clojerl.ProcessVal' ->
      'clojerl.ProcessVal':'hash'(X);
    'clojerl.BitString' ->
      'clojerl.BitString':'hash'(X);
    'clojerl.Var' ->
      'clojerl.Var':'hash'(X);
    'clojerl.Integer' ->
      'clojerl.Integer':'hash'(X);
    'clojerl.Keyword' ->
      'clojerl.Keyword':'hash'(X);
    'clojerl.Boolean' ->
      'clojerl.Boolean':'hash'(X);
    'clojerl.Delay' ->
      'clojerl.Delay':'hash'(X);
    'clojerl.reader.ReaderConditional' ->
      'clojerl.reader.ReaderConditional':'hash'(X);
    'clojerl.Float' ->
      'clojerl.Float':'hash'(X);
    'clojerl.Atom' ->
      'clojerl.Atom':'hash'(X);
    'clojerl.String' ->
      'clojerl.String':'hash'(X);
    'clojerl.Symbol' ->
      'clojerl.Symbol':'hash'(X);
    'clojerl.IllegalAccessError' ->
      'clojerl.IllegalAccessError':'hash'(X);
    'clojerl.AssertionError' ->
      'clojerl.AssertionError':'hash'(X);
    'clojerl.Error' ->
      'clojerl.Error':'hash'(X);
    'clojerl.BadArgumentError' ->
      'clojerl.BadArgumentError':'hash'(X);
    'clojerl.ArityError' ->
      'clojerl.ArityError':'hash'(X);
    'clojerl.IOError' ->
      'clojerl.IOError':'hash'(X);
    'clojerl.ExceptionInfo' ->
      'clojerl.ExceptionInfo':'hash'(X);
    'clojerl.Namespace' ->
      'clojerl.Namespace':'hash'(X);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'hash'(X);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'hash'(X);
    'clojerl.Range' ->
      'clojerl.Range':'hash'(X);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'hash'(X);
    'clojerl.Vector.RSeq' ->
      'clojerl.Vector.RSeq':'hash'(X);
    'clojerl.List' ->
      'clojerl.List':'hash'(X);
    'clojerl.Vector' ->
      'clojerl.Vector':'hash'(X);
    'clojerl.Map' ->
      'clojerl.Map':'hash'(X);
    'clojerl.Cons' ->
      'clojerl.Cons':'hash'(X);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'hash'(X);
    'clojerl.Set' ->
      'clojerl.Set':'hash'(X);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'hash'(X);
    'clojerl.SortedSet' ->
      'clojerl.SortedSet':'hash'(X);
    'clojerl.TupleChunk' ->
      'clojerl.TupleChunk':'hash'(X);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'hash', Type)
  end.

?SATISFIES('erlang.Tuple') -> true;
?SATISFIES('erlang.Map') -> true;
?SATISFIES('erlang.Fn') -> true;
?SATISFIES('erlang.util.UUID') -> true;
?SATISFIES('erlang.Reference') -> true;
?SATISFIES('erlang.List') -> true;
?SATISFIES('erlang.Process') -> true;
?SATISFIES('erlang.util.Regex') -> true;
?SATISFIES('erlang.Type') -> true;
?SATISFIES('erlang.Port') -> true;
?SATISFIES('erlang.util.Date') -> true;
?SATISFIES('clojerl.Reduced') -> true;
?SATISFIES('clojerl.ProcessVal') -> true;
?SATISFIES('clojerl.BitString') -> true;
?SATISFIES('clojerl.Var') -> true;
?SATISFIES('clojerl.Integer') -> true;
?SATISFIES('clojerl.Keyword') -> true;
?SATISFIES('clojerl.Boolean') -> true;
?SATISFIES('clojerl.Delay') -> true;
?SATISFIES('clojerl.reader.ReaderConditional') -> true;
?SATISFIES('clojerl.Float') -> true;
?SATISFIES('clojerl.Atom') -> true;
?SATISFIES('clojerl.String') -> true;
?SATISFIES('clojerl.Symbol') -> true;
?SATISFIES('clojerl.IllegalAccessError') -> true;
?SATISFIES('clojerl.AssertionError') -> true;
?SATISFIES('clojerl.Error') -> true;
?SATISFIES('clojerl.BadArgumentError') -> true;
?SATISFIES('clojerl.ArityError') -> true;
?SATISFIES('clojerl.IOError') -> true;
?SATISFIES('clojerl.ExceptionInfo') -> true;
?SATISFIES('clojerl.Namespace') -> true;
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
