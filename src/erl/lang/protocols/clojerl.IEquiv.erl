%%% Code generate by scripts/generate-protocols
-module('clojerl.IEquiv').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['equiv'/2]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'equiv'(any(), any()) -> any().
-optional_callbacks(['equiv'/2]).

'equiv'(X, Y) ->
  case X of
    #{?TYPE := 'clojerl.Agent'} ->
      'clojerl.Agent':'equiv'(X, Y);
    #{?TYPE := 'clojerl.ArityError'} ->
      'clojerl.ArityError':'equiv'(X, Y);
    #{?TYPE := 'clojerl.AssertionError'} ->
      'clojerl.AssertionError':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Atom'} ->
      'clojerl.Atom':'equiv'(X, Y);
    #{?TYPE := 'clojerl.BadArgumentError'} ->
      'clojerl.BadArgumentError':'equiv'(X, Y);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Delay'} ->
      'clojerl.Delay':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Error'} ->
      'clojerl.Error':'equiv'(X, Y);
    #{?TYPE := 'clojerl.ExceptionInfo'} ->
      'clojerl.ExceptionInfo':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Future'} ->
      'clojerl.Future':'equiv'(X, Y);
    #{?TYPE := 'clojerl.IOError'} ->
      'clojerl.IOError':'equiv'(X, Y);
    #{?TYPE := 'clojerl.IllegalAccessError'} ->
      'clojerl.IllegalAccessError':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'equiv'(X, Y);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'equiv'(X, Y);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'equiv'(X, Y);
    #{?TYPE := 'clojerl.ProcessVal'} ->
      'clojerl.ProcessVal':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Promise'} ->
      'clojerl.Promise':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'equiv'(X, Y);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'equiv'(X, Y);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'equiv'(X, Y);
    #{?TYPE := 'clojerl.StringSeq'} ->
      'clojerl.StringSeq':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Subvec'} ->
      'clojerl.Subvec':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Symbol'} ->
      'clojerl.Symbol':'equiv'(X, Y);
    #{?TYPE := 'clojerl.TransducerSeq'} ->
      'clojerl.TransducerSeq':'equiv'(X, Y);
    #{?TYPE := 'clojerl.TupleChunk'} ->
      'clojerl.TupleChunk':'equiv'(X, Y);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'equiv'(X, Y);
    #{?TYPE := 'clojerl.Vector.Seq'} ->
      'clojerl.Vector.Seq':'equiv'(X, Y);
    #{?TYPE := 'clojerl.reader.ReaderConditional'} ->
      'clojerl.reader.ReaderConditional':'equiv'(X, Y);
    #{?TYPE := 'clojerl.reader.TaggedLiteral'} ->
      'clojerl.reader.TaggedLiteral':'equiv'(X, Y);
    #{?TYPE := 'erlang.util.Date'} ->
      'erlang.util.Date':'equiv'(X, Y);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'equiv', X);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'equiv', X);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'equiv', X);
    X_ when erlang:is_list(X_) ->
      'erlang.List':'equiv'(X, Y);
    X_ when erlang:is_map(X_) ->
      'erlang.Map':'equiv'(X, Y);
    X_ when erlang:is_tuple(X_) ->
      'erlang.Tuple':'equiv'(X, Y);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'equiv', X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'equiv', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Agent'} ->  true;
    #{?TYPE := 'clojerl.ArityError'} ->  true;
    #{?TYPE := 'clojerl.AssertionError'} ->  true;
    #{?TYPE := 'clojerl.Atom'} ->  true;
    #{?TYPE := 'clojerl.BadArgumentError'} ->  true;
    #{?TYPE := 'clojerl.ChunkedCons'} ->  true;
    #{?TYPE := 'clojerl.Cons'} ->  true;
    #{?TYPE := 'clojerl.Cycle'} ->  true;
    #{?TYPE := 'clojerl.Delay'} ->  true;
    #{?TYPE := 'clojerl.Error'} ->  true;
    #{?TYPE := 'clojerl.ExceptionInfo'} ->  true;
    #{?TYPE := 'clojerl.Future'} ->  true;
    #{?TYPE := 'clojerl.IOError'} ->  true;
    #{?TYPE := 'clojerl.IllegalAccessError'} ->  true;
    #{?TYPE := 'clojerl.Iterate'} ->  true;
    #{?TYPE := 'clojerl.LazySeq'} ->  true;
    #{?TYPE := 'clojerl.List'} ->  true;
    #{?TYPE := 'clojerl.Map'} ->  true;
    #{?TYPE := 'clojerl.ProcessVal'} ->  true;
    #{?TYPE := 'clojerl.Promise'} ->  true;
    #{?TYPE := 'clojerl.Range'} ->  true;
    #{?TYPE := 'clojerl.Repeat'} ->  true;
    #{?TYPE := 'clojerl.Set'} ->  true;
    #{?TYPE := 'clojerl.SortedMap'} ->  true;
    #{?TYPE := 'clojerl.SortedSet'} ->  true;
    #{?TYPE := 'clojerl.StringSeq'} ->  true;
    #{?TYPE := 'clojerl.Subvec'} ->  true;
    #{?TYPE := 'clojerl.Symbol'} ->  true;
    #{?TYPE := 'clojerl.TransducerSeq'} ->  true;
    #{?TYPE := 'clojerl.TupleChunk'} ->  true;
    #{?TYPE := 'clojerl.TupleMap'} ->  true;
    #{?TYPE := 'clojerl.Var'} ->  true;
    #{?TYPE := 'clojerl.Vector'} ->  true;
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->  true;
    #{?TYPE := 'clojerl.Vector.RSeq'} ->  true;
    #{?TYPE := 'clojerl.Vector.Seq'} ->  true;
    #{?TYPE := 'clojerl.reader.ReaderConditional'} ->  true;
    #{?TYPE := 'clojerl.reader.TaggedLiteral'} ->  true;
    #{?TYPE := 'erlang.util.Date'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    X_ when erlang:is_list(X_) ->  true;
    X_ when erlang:is_map(X_) ->  true;
    X_ when erlang:is_tuple(X_) ->  true;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.Agent' -> true;
    'clojerl.ArityError' -> true;
    'clojerl.AssertionError' -> true;
    'clojerl.Atom' -> true;
    'clojerl.BadArgumentError' -> true;
    'clojerl.ChunkedCons' -> true;
    'clojerl.Cons' -> true;
    'clojerl.Cycle' -> true;
    'clojerl.Delay' -> true;
    'clojerl.Error' -> true;
    'clojerl.ExceptionInfo' -> true;
    'clojerl.Future' -> true;
    'clojerl.IOError' -> true;
    'clojerl.IllegalAccessError' -> true;
    'clojerl.Iterate' -> true;
    'clojerl.LazySeq' -> true;
    'clojerl.List' -> true;
    'clojerl.Map' -> true;
    'clojerl.ProcessVal' -> true;
    'clojerl.Promise' -> true;
    'clojerl.Range' -> true;
    'clojerl.Repeat' -> true;
    'clojerl.Set' -> true;
    'clojerl.SortedMap' -> true;
    'clojerl.SortedSet' -> true;
    'clojerl.StringSeq' -> true;
    'clojerl.Subvec' -> true;
    'clojerl.Symbol' -> true;
    'clojerl.TransducerSeq' -> true;
    'clojerl.TupleChunk' -> true;
    'clojerl.TupleMap' -> true;
    'clojerl.Var' -> true;
    'clojerl.Vector' -> true;
    'clojerl.Vector.ChunkedSeq' -> true;
    'clojerl.Vector.RSeq' -> true;
    'clojerl.Vector.Seq' -> true;
    'clojerl.reader.ReaderConditional' -> true;
    'clojerl.reader.TaggedLiteral' -> true;
    'erlang.util.Date' -> true;
    'erlang.List' -> true;
    'erlang.Map' -> true;
    'erlang.Tuple' -> true;
    _ -> false
  end.
