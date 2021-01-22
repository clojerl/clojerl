%%% Code generate by scripts/generate-protocols
-module('clojerl.IHash').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['hash'/1]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'hash'(any()) -> any().
-optional_callbacks(['hash'/1]).

'hash'(X) ->
  case X of
    #{?TYPE := 'clojerl.Agent'} ->
      'clojerl.Agent':'hash'(X);
    #{?TYPE := 'clojerl.ArityError'} ->
      'clojerl.ArityError':'hash'(X);
    #{?TYPE := 'clojerl.AssertionError'} ->
      'clojerl.AssertionError':'hash'(X);
    #{?TYPE := 'clojerl.Atom'} ->
      'clojerl.Atom':'hash'(X);
    #{?TYPE := 'clojerl.BadArgumentError'} ->
      'clojerl.BadArgumentError':'hash'(X);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'hash'(X);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'hash'(X);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'hash'(X);
    #{?TYPE := 'clojerl.Delay'} ->
      'clojerl.Delay':'hash'(X);
    #{?TYPE := 'clojerl.Error'} ->
      'clojerl.Error':'hash'(X);
    #{?TYPE := 'clojerl.ExceptionInfo'} ->
      'clojerl.ExceptionInfo':'hash'(X);
    #{?TYPE := 'clojerl.Fn'} ->
      'clojerl.Fn':'hash'(X);
    #{?TYPE := 'clojerl.Future'} ->
      'clojerl.Future':'hash'(X);
    #{?TYPE := 'clojerl.IOError'} ->
      'clojerl.IOError':'hash'(X);
    #{?TYPE := 'clojerl.IllegalAccessError'} ->
      'clojerl.IllegalAccessError':'hash'(X);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'hash'(X);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'hash'(X);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'hash'(X);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'hash'(X);
    #{?TYPE := 'clojerl.Namespace'} ->
      'clojerl.Namespace':'hash'(X);
    #{?TYPE := 'clojerl.ProcessVal'} ->
      'clojerl.ProcessVal':'hash'(X);
    #{?TYPE := 'clojerl.Promise'} ->
      'clojerl.Promise':'hash'(X);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'hash'(X);
    #{?TYPE := 'clojerl.Reduced'} ->
      'clojerl.Reduced':'hash'(X);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'hash'(X);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'hash'(X);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'hash'(X);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'hash'(X);
    #{?TYPE := 'clojerl.StringSeq'} ->
      'clojerl.StringSeq':'hash'(X);
    #{?TYPE := 'clojerl.Subvec'} ->
      'clojerl.Subvec':'hash'(X);
    #{?TYPE := 'clojerl.Symbol'} ->
      'clojerl.Symbol':'hash'(X);
    #{?TYPE := 'clojerl.TupleChunk'} ->
      'clojerl.TupleChunk':'hash'(X);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'hash'(X);
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'hash'(X);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'hash'(X);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'hash'(X);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'hash'(X);
    #{?TYPE := 'clojerl.Vector.Seq'} ->
      'clojerl.Vector.Seq':'hash'(X);
    #{?TYPE := 'clojerl.reader.ReaderConditional'} ->
      'clojerl.reader.ReaderConditional':'hash'(X);
    #{?TYPE := 'erlang.Type'} ->
      'erlang.Type':'hash'(X);
    #{?TYPE := 'erlang.util.Date'} ->
      'erlang.util.Date':'hash'(X);
    #{?TYPE := 'erlang.util.Regex'} ->
      'erlang.util.Regex':'hash'(X);
    #{?TYPE := 'erlang.util.UUID'} ->
      'erlang.util.UUID':'hash'(X);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'hash', X);
    X_ when erlang:is_binary(X_) ->
      'clojerl.String':'hash'(X);
    X_ when erlang:is_bitstring(X_) ->
      'clojerl.BitString':'hash'(X);
    X_ when erlang:is_integer(X_) ->
      'clojerl.Integer':'hash'(X);
    X_ when erlang:is_float(X_) ->
      'clojerl.Float':'hash'(X);
    X_ when erlang:is_boolean(X_) ->
      'clojerl.Boolean':'hash'(X);
    X_ when erlang:is_list(X_) ->
      'erlang.List':'hash'(X);
    X_ when erlang:is_map(X_) ->
      'erlang.Map':'hash'(X);
    X_ when erlang:is_tuple(X_) ->
      'erlang.Tuple':'hash'(X);
    X_ when erlang:is_function(X_) ->
      'erlang.Fn':'hash'(X);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'hash', X);
    X_ when erlang:is_atom(X_) ->
      'clojerl.Keyword':'hash'(X);
    X_ when erlang:is_port(X_) ->
      'erlang.Port':'hash'(X);
    X_ when erlang:is_pid(X_) ->
      'erlang.Process':'hash'(X);
    X_ when erlang:is_reference(X_) ->
      'erlang.Reference':'hash'(X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'hash', X)
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
    #{?TYPE := 'clojerl.Fn'} ->  true;
    #{?TYPE := 'clojerl.Future'} ->  true;
    #{?TYPE := 'clojerl.IOError'} ->  true;
    #{?TYPE := 'clojerl.IllegalAccessError'} ->  true;
    #{?TYPE := 'clojerl.Iterate'} ->  true;
    #{?TYPE := 'clojerl.LazySeq'} ->  true;
    #{?TYPE := 'clojerl.List'} ->  true;
    #{?TYPE := 'clojerl.Map'} ->  true;
    #{?TYPE := 'clojerl.Namespace'} ->  true;
    #{?TYPE := 'clojerl.ProcessVal'} ->  true;
    #{?TYPE := 'clojerl.Promise'} ->  true;
    #{?TYPE := 'clojerl.Range'} ->  true;
    #{?TYPE := 'clojerl.Reduced'} ->  true;
    #{?TYPE := 'clojerl.Repeat'} ->  true;
    #{?TYPE := 'clojerl.Set'} ->  true;
    #{?TYPE := 'clojerl.SortedMap'} ->  true;
    #{?TYPE := 'clojerl.SortedSet'} ->  true;
    #{?TYPE := 'clojerl.StringSeq'} ->  true;
    #{?TYPE := 'clojerl.Subvec'} ->  true;
    #{?TYPE := 'clojerl.Symbol'} ->  true;
    #{?TYPE := 'clojerl.TupleChunk'} ->  true;
    #{?TYPE := 'clojerl.TupleMap'} ->  true;
    #{?TYPE := 'clojerl.Var'} ->  true;
    #{?TYPE := 'clojerl.Vector'} ->  true;
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->  true;
    #{?TYPE := 'clojerl.Vector.RSeq'} ->  true;
    #{?TYPE := 'clojerl.Vector.Seq'} ->  true;
    #{?TYPE := 'clojerl.reader.ReaderConditional'} ->  true;
    #{?TYPE := 'erlang.Type'} ->  true;
    #{?TYPE := 'erlang.util.Date'} ->  true;
    #{?TYPE := 'erlang.util.Regex'} ->  true;
    #{?TYPE := 'erlang.util.UUID'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  true;
    X_ when erlang:is_bitstring(X_) ->  true;
    X_ when erlang:is_integer(X_) ->  true;
    X_ when erlang:is_float(X_) ->  true;
    X_ when erlang:is_boolean(X_) ->  true;
    X_ when erlang:is_list(X_) ->  true;
    X_ when erlang:is_map(X_) ->  true;
    X_ when erlang:is_tuple(X_) ->  true;
    X_ when erlang:is_function(X_) ->  true;
    ?NIL ->  false;
    X_ when erlang:is_atom(X_) ->  true;
    X_ when erlang:is_port(X_) ->  true;
    X_ when erlang:is_pid(X_) ->  true;
    X_ when erlang:is_reference(X_) ->  true;
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
    'clojerl.Fn' -> true;
    'clojerl.Future' -> true;
    'clojerl.IOError' -> true;
    'clojerl.IllegalAccessError' -> true;
    'clojerl.Iterate' -> true;
    'clojerl.LazySeq' -> true;
    'clojerl.List' -> true;
    'clojerl.Map' -> true;
    'clojerl.Namespace' -> true;
    'clojerl.ProcessVal' -> true;
    'clojerl.Promise' -> true;
    'clojerl.Range' -> true;
    'clojerl.Reduced' -> true;
    'clojerl.Repeat' -> true;
    'clojerl.Set' -> true;
    'clojerl.SortedMap' -> true;
    'clojerl.SortedSet' -> true;
    'clojerl.StringSeq' -> true;
    'clojerl.Subvec' -> true;
    'clojerl.Symbol' -> true;
    'clojerl.TupleChunk' -> true;
    'clojerl.TupleMap' -> true;
    'clojerl.Var' -> true;
    'clojerl.Vector' -> true;
    'clojerl.Vector.ChunkedSeq' -> true;
    'clojerl.Vector.RSeq' -> true;
    'clojerl.Vector.Seq' -> true;
    'clojerl.reader.ReaderConditional' -> true;
    'erlang.Type' -> true;
    'erlang.util.Date' -> true;
    'erlang.util.Regex' -> true;
    'erlang.util.UUID' -> true;
    'clojerl.String' -> true;
    'clojerl.BitString' -> true;
    'clojerl.Integer' -> true;
    'clojerl.Float' -> true;
    'clojerl.Boolean' -> true;
    'erlang.List' -> true;
    'erlang.Map' -> true;
    'erlang.Tuple' -> true;
    'erlang.Fn' -> true;
    'clojerl.Keyword' -> true;
    'erlang.Port' -> true;
    'erlang.Process' -> true;
    'erlang.Reference' -> true;
    _ -> false
  end.
