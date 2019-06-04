-module('clojerl.IEquiv').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['equiv'/2]).
-export([?SATISFIES/1]).

-callback 'equiv'(any(), any()) -> any().

'equiv'(X, Y) ->
  case X of
    #{?TYPE := 'erlang.util.Date'} ->
      'erlang.util.Date':'equiv'(X, Y);
    #{?TYPE := 'clojerl.TransducerSeq'} ->
      'clojerl.TransducerSeq':'equiv'(X, Y);
    #{?TYPE := 'clojerl.reader.TaggedLiteral'} ->
      'clojerl.reader.TaggedLiteral':'equiv'(X, Y);
    #{?TYPE := 'clojerl.ProcessVal'} ->
      'clojerl.ProcessVal':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Delay'} ->
      'clojerl.Delay':'equiv'(X, Y);
    #{?TYPE := 'clojerl.reader.ReaderConditional'} ->
      'clojerl.reader.ReaderConditional':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Atom'} ->
      'clojerl.Atom':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Symbol'} ->
      'clojerl.Symbol':'equiv'(X, Y);
    #{?TYPE := 'clojerl.IllegalAccessError'} ->
      'clojerl.IllegalAccessError':'equiv'(X, Y);
    #{?TYPE := 'clojerl.AssertionError'} ->
      'clojerl.AssertionError':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Error'} ->
      'clojerl.Error':'equiv'(X, Y);
    #{?TYPE := 'clojerl.BadArgumentError'} ->
      'clojerl.BadArgumentError':'equiv'(X, Y);
    #{?TYPE := 'clojerl.ArityError'} ->
      'clojerl.ArityError':'equiv'(X, Y);
    #{?TYPE := 'clojerl.IOError'} ->
      'clojerl.IOError':'equiv'(X, Y);
    #{?TYPE := 'clojerl.ExceptionInfo'} ->
      'clojerl.ExceptionInfo':'equiv'(X, Y);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'equiv'(X, Y);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'equiv'(X, Y);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'equiv'(X, Y);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'equiv'(X, Y);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'equiv'(X, Y);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'equiv'(X, Y);
    #{?TYPE := 'clojerl.TupleChunk'} ->
      'clojerl.TupleChunk':'equiv'(X, Y);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'equiv', X);
    ZZZ when is_list(ZZZ) ->
      'erlang.List':'equiv'(X, Y);
    ZZZ when is_map(ZZZ) ->
      'erlang.Map':'equiv'(X, Y);
    ZZZ when is_tuple(ZZZ) ->
      'erlang.Tuple':'equiv'(X, Y);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'equiv', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'erlang.util.Date'} -> true;
    #{?TYPE := 'clojerl.TransducerSeq'} -> true;
    #{?TYPE := 'clojerl.reader.TaggedLiteral'} -> true;
    #{?TYPE := 'clojerl.ProcessVal'} -> true;
    #{?TYPE := 'clojerl.Var'} -> true;
    #{?TYPE := 'clojerl.Delay'} -> true;
    #{?TYPE := 'clojerl.reader.ReaderConditional'} -> true;
    #{?TYPE := 'clojerl.Atom'} -> true;
    #{?TYPE := 'clojerl.Symbol'} -> true;
    #{?TYPE := 'clojerl.IllegalAccessError'} -> true;
    #{?TYPE := 'clojerl.AssertionError'} -> true;
    #{?TYPE := 'clojerl.Error'} -> true;
    #{?TYPE := 'clojerl.BadArgumentError'} -> true;
    #{?TYPE := 'clojerl.ArityError'} -> true;
    #{?TYPE := 'clojerl.IOError'} -> true;
    #{?TYPE := 'clojerl.ExceptionInfo'} -> true;
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
    #{?TYPE := 'clojerl.TupleChunk'} -> true;
    #{?TYPE := _} -> false;
    ZZZ when is_list(ZZZ) -> true;
    ZZZ when is_map(ZZZ) -> true;
    ZZZ when is_tuple(ZZZ) -> true;
    _ -> false
  end.
