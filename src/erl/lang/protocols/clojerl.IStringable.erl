-module('clojerl.IStringable').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['str'/1]).
-export([?SATISFIES/1]).

-callback 'str'(any()) -> any().

'str'(X) ->
  case clj_rt:type_module(X) of
    'erlang.Tuple' ->
      'erlang.Tuple':'str'(X);
    'erlang.Map' ->
      'erlang.Map':'str'(X);
    'erlang.Fn' ->
      'erlang.Fn':'str'(X);
    'erlang.io.PushbackReader' ->
      'erlang.io.PushbackReader':'str'(X);
    'erlang.util.UUID' ->
      'erlang.util.UUID':'str'(X);
    'erlang.io.StringReader' ->
      'erlang.io.StringReader':'str'(X);
    'erlang.Reference' ->
      'erlang.Reference':'str'(X);
    'erlang.io.StringWriter' ->
      'erlang.io.StringWriter':'str'(X);
    'erlang.List' ->
      'erlang.List':'str'(X);
    'erlang.Process' ->
      'erlang.Process':'str'(X);
    'erlang.util.Regex' ->
      'erlang.util.Regex':'str'(X);
    'erlang.Type' ->
      'erlang.Type':'str'(X);
    'erlang.Port' ->
      'erlang.Port':'str'(X);
    'erlang.util.Date' ->
      'erlang.util.Date':'str'(X);
    'erlang.io.File' ->
      'erlang.io.File':'str'(X);
    'clojerl.Reduced' ->
      'clojerl.Reduced':'str'(X);
    'clojerl.TransducerSeq' ->
      'clojerl.TransducerSeq':'str'(X);
    'clojerl.reader.TaggedLiteral' ->
      'clojerl.reader.TaggedLiteral':'str'(X);
    'clojerl.ProcessVal' ->
      'clojerl.ProcessVal':'str'(X);
    'clojerl.BitString' ->
      'clojerl.BitString':'str'(X);
    'clojerl.Var' ->
      'clojerl.Var':'str'(X);
    'clojerl.Integer' ->
      'clojerl.Integer':'str'(X);
    'clojerl.Keyword' ->
      'clojerl.Keyword':'str'(X);
    'clojerl.Boolean' ->
      'clojerl.Boolean':'str'(X);
    'clojerl.Delay' ->
      'clojerl.Delay':'str'(X);
    'clojerl.reader.ReaderConditional' ->
      'clojerl.reader.ReaderConditional':'str'(X);
    'clojerl.Float' ->
      'clojerl.Float':'str'(X);
    'clojerl.Atom' ->
      'clojerl.Atom':'str'(X);
    'clojerl.String' ->
      'clojerl.String':'str'(X);
    'clojerl.Symbol' ->
      'clojerl.Symbol':'str'(X);
    'clojerl.IllegalAccessError' ->
      'clojerl.IllegalAccessError':'str'(X);
    'clojerl.AssertionError' ->
      'clojerl.AssertionError':'str'(X);
    'clojerl.Error' ->
      'clojerl.Error':'str'(X);
    'clojerl.BadArgumentError' ->
      'clojerl.BadArgumentError':'str'(X);
    'clojerl.ArityError' ->
      'clojerl.ArityError':'str'(X);
    'clojerl.IOError' ->
      'clojerl.IOError':'str'(X);
    'clojerl.ExceptionInfo' ->
      'clojerl.ExceptionInfo':'str'(X);
    'clojerl.Namespace' ->
      'clojerl.Namespace':'str'(X);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'str'(X);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'str'(X);
    'clojerl.Range' ->
      'clojerl.Range':'str'(X);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'str'(X);
    'clojerl.Vector.RSeq' ->
      'clojerl.Vector.RSeq':'str'(X);
    'clojerl.List' ->
      'clojerl.List':'str'(X);
    'clojerl.Vector' ->
      'clojerl.Vector':'str'(X);
    'clojerl.Map' ->
      'clojerl.Map':'str'(X);
    'clojerl.Cons' ->
      'clojerl.Cons':'str'(X);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'str'(X);
    'clojerl.Set' ->
      'clojerl.Set':'str'(X);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'str'(X);
    'clojerl.SortedSet' ->
      'clojerl.SortedSet':'str'(X);
    'clojerl.Fn' ->
      'clojerl.Fn':'str'(X);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'str', Type)
  end.

?SATISFIES('erlang.Tuple') -> true;
?SATISFIES('erlang.Map') -> true;
?SATISFIES('erlang.Fn') -> true;
?SATISFIES('erlang.io.PushbackReader') -> true;
?SATISFIES('erlang.util.UUID') -> true;
?SATISFIES('erlang.io.StringReader') -> true;
?SATISFIES('erlang.Reference') -> true;
?SATISFIES('erlang.io.StringWriter') -> true;
?SATISFIES('erlang.List') -> true;
?SATISFIES('erlang.Process') -> true;
?SATISFIES('erlang.util.Regex') -> true;
?SATISFIES('erlang.Type') -> true;
?SATISFIES('erlang.Port') -> true;
?SATISFIES('erlang.util.Date') -> true;
?SATISFIES('erlang.io.File') -> true;
?SATISFIES('clojerl.Reduced') -> true;
?SATISFIES('clojerl.TransducerSeq') -> true;
?SATISFIES('clojerl.reader.TaggedLiteral') -> true;
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
?SATISFIES('clojerl.Fn') -> true;
?SATISFIES(_) -> false.
