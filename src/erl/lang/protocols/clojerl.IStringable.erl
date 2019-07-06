-module('clojerl.IStringable').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['str'/1]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'str'(any()) -> any().

'str'(X) ->
  case X of
    #{?TYPE := 'clojerl.IOError'} ->
      'clojerl.IOError':'str'(X);
    #{?TYPE := 'clojerl.Fn'} ->
      'clojerl.Fn':'str'(X);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'str'(X);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'str'(X);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'str'(X);
    #{?TYPE := 'erlang.io.PushbackReader'} ->
      'erlang.io.PushbackReader':'str'(X);
    #{?TYPE := 'clojerl.Delay'} ->
      'clojerl.Delay':'str'(X);
    #{?TYPE := 'erlang.util.Date'} ->
      'erlang.util.Date':'str'(X);
    #{?TYPE := 'clojerl.ArityError'} ->
      'clojerl.ArityError':'str'(X);
    #{?TYPE := 'clojerl.reader.TaggedLiteral'} ->
      'clojerl.reader.TaggedLiteral':'str'(X);
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'str'(X);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'str'(X);
    #{?TYPE := 'erlang.io.StringReader'} ->
      'erlang.io.StringReader':'str'(X);
    #{?TYPE := 'clojerl.reader.ReaderConditional'} ->
      'clojerl.reader.ReaderConditional':'str'(X);
    #{?TYPE := 'clojerl.Namespace'} ->
      'clojerl.Namespace':'str'(X);
    #{?TYPE := 'clojerl.BadArgumentError'} ->
      'clojerl.BadArgumentError':'str'(X);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'str'(X);
    #{?TYPE := 'erlang.io.StringWriter'} ->
      'erlang.io.StringWriter':'str'(X);
    #{?TYPE := 'clojerl.ExceptionInfo'} ->
      'clojerl.ExceptionInfo':'str'(X);
    #{?TYPE := 'clojerl.Error'} ->
      'clojerl.Error':'str'(X);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'str'(X);
    #{?TYPE := 'clojerl.Symbol'} ->
      'clojerl.Symbol':'str'(X);
    #{?TYPE := 'clojerl.TransducerSeq'} ->
      'clojerl.TransducerSeq':'str'(X);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'str'(X);
    #{?TYPE := 'clojerl.Reduced'} ->
      'clojerl.Reduced':'str'(X);
    #{?TYPE := 'clojerl.IllegalAccessError'} ->
      'clojerl.IllegalAccessError':'str'(X);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'str'(X);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'str'(X);
    #{?TYPE := 'erlang.util.Regex'} ->
      'erlang.util.Regex':'str'(X);
    #{?TYPE := 'clojerl.ProcessVal'} ->
      'clojerl.ProcessVal':'str'(X);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'str'(X);
    #{?TYPE := 'clojerl.AssertionError'} ->
      'clojerl.AssertionError':'str'(X);
    #{?TYPE := 'clojerl.StringSeq'} ->
      'clojerl.StringSeq':'str'(X);
    #{?TYPE := 'erlang.util.UUID'} ->
      'erlang.util.UUID':'str'(X);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'str'(X);
    #{?TYPE := 'clojerl.Atom'} ->
      'clojerl.Atom':'str'(X);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'str'(X);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'str'(X);
    #{?TYPE := 'erlang.io.File'} ->
      'erlang.io.File':'str'(X);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'str'(X);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'str'(X);
    #{?TYPE := 'erlang.Type'} ->
      'erlang.Type':'str'(X);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'str'(X);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'str', X);
    X_ when erlang:is_binary(X_) ->
      'clojerl.String':'str'(X);
    X_ when erlang:is_bitstring(X_) ->
      'clojerl.BitString':'str'(X);
    X_ when erlang:is_integer(X_) ->
      'clojerl.Integer':'str'(X);
    X_ when erlang:is_float(X_) ->
      'clojerl.Float':'str'(X);
    X_ when erlang:is_boolean(X_) ->
      'clojerl.Boolean':'str'(X);
    X_ when erlang:is_list(X_) ->
      'erlang.List':'str'(X);
    X_ when erlang:is_map(X_) ->
      'erlang.Map':'str'(X);
    X_ when erlang:is_tuple(X_) ->
      'erlang.Tuple':'str'(X);
    X_ when erlang:is_function(X_) ->
      'erlang.Fn':'str'(X);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'str', X);
    X_ when erlang:is_atom(X_) ->
      'clojerl.Keyword':'str'(X);
    X_ when erlang:is_port(X_) ->
      'erlang.Port':'str'(X);
    X_ when erlang:is_pid(X_) ->
      'erlang.Process':'str'(X);
    X_ when erlang:is_reference(X_) ->
      'erlang.Reference':'str'(X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'str', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.IOError'} ->  true;
    #{?TYPE := 'clojerl.Fn'} ->  true;
    #{?TYPE := 'clojerl.List'} ->  true;
    #{?TYPE := 'clojerl.LazySeq'} ->  true;
    #{?TYPE := 'clojerl.Range'} ->  true;
    #{?TYPE := 'erlang.io.PushbackReader'} ->  true;
    #{?TYPE := 'clojerl.Delay'} ->  true;
    #{?TYPE := 'erlang.util.Date'} ->  true;
    #{?TYPE := 'clojerl.ArityError'} ->  true;
    #{?TYPE := 'clojerl.reader.TaggedLiteral'} ->  true;
    #{?TYPE := 'clojerl.Var'} ->  true;
    #{?TYPE := 'clojerl.ChunkedCons'} ->  true;
    #{?TYPE := 'erlang.io.StringReader'} ->  true;
    #{?TYPE := 'clojerl.reader.ReaderConditional'} ->  true;
    #{?TYPE := 'clojerl.Namespace'} ->  true;
    #{?TYPE := 'clojerl.BadArgumentError'} ->  true;
    #{?TYPE := 'clojerl.Set'} ->  true;
    #{?TYPE := 'erlang.io.StringWriter'} ->  true;
    #{?TYPE := 'clojerl.ExceptionInfo'} ->  true;
    #{?TYPE := 'clojerl.Error'} ->  true;
    #{?TYPE := 'clojerl.Cons'} ->  true;
    #{?TYPE := 'clojerl.Symbol'} ->  true;
    #{?TYPE := 'clojerl.TransducerSeq'} ->  true;
    #{?TYPE := 'clojerl.Vector.RSeq'} ->  true;
    #{?TYPE := 'clojerl.Reduced'} ->  true;
    #{?TYPE := 'clojerl.IllegalAccessError'} ->  true;
    #{?TYPE := 'clojerl.Iterate'} ->  true;
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->  true;
    #{?TYPE := 'erlang.util.Regex'} ->  true;
    #{?TYPE := 'clojerl.ProcessVal'} ->  true;
    #{?TYPE := 'clojerl.TupleMap'} ->  true;
    #{?TYPE := 'clojerl.AssertionError'} ->  true;
    #{?TYPE := 'clojerl.StringSeq'} ->  true;
    #{?TYPE := 'erlang.util.UUID'} ->  true;
    #{?TYPE := 'clojerl.Repeat'} ->  true;
    #{?TYPE := 'clojerl.Atom'} ->  true;
    #{?TYPE := 'clojerl.Vector'} ->  true;
    #{?TYPE := 'clojerl.Cycle'} ->  true;
    #{?TYPE := 'erlang.io.File'} ->  true;
    #{?TYPE := 'clojerl.SortedMap'} ->  true;
    #{?TYPE := 'clojerl.SortedSet'} ->  true;
    #{?TYPE := 'erlang.Type'} ->  true;
    #{?TYPE := 'clojerl.Map'} ->  true;
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
    'clojerl.IOError' -> true;
    'clojerl.Fn' -> true;
    'clojerl.List' -> true;
    'clojerl.LazySeq' -> true;
    'clojerl.Range' -> true;
    'erlang.io.PushbackReader' -> true;
    'clojerl.Delay' -> true;
    'erlang.util.Date' -> true;
    'clojerl.ArityError' -> true;
    'clojerl.reader.TaggedLiteral' -> true;
    'clojerl.Var' -> true;
    'clojerl.ChunkedCons' -> true;
    'erlang.io.StringReader' -> true;
    'clojerl.reader.ReaderConditional' -> true;
    'clojerl.Namespace' -> true;
    'clojerl.BadArgumentError' -> true;
    'clojerl.Set' -> true;
    'erlang.io.StringWriter' -> true;
    'clojerl.ExceptionInfo' -> true;
    'clojerl.Error' -> true;
    'clojerl.Cons' -> true;
    'clojerl.Symbol' -> true;
    'clojerl.TransducerSeq' -> true;
    'clojerl.Vector.RSeq' -> true;
    'clojerl.Reduced' -> true;
    'clojerl.IllegalAccessError' -> true;
    'clojerl.Iterate' -> true;
    'clojerl.Vector.ChunkedSeq' -> true;
    'erlang.util.Regex' -> true;
    'clojerl.ProcessVal' -> true;
    'clojerl.TupleMap' -> true;
    'clojerl.AssertionError' -> true;
    'clojerl.StringSeq' -> true;
    'erlang.util.UUID' -> true;
    'clojerl.Repeat' -> true;
    'clojerl.Atom' -> true;
    'clojerl.Vector' -> true;
    'clojerl.Cycle' -> true;
    'erlang.io.File' -> true;
    'clojerl.SortedMap' -> true;
    'clojerl.SortedSet' -> true;
    'erlang.Type' -> true;
    'clojerl.Map' -> true;
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
