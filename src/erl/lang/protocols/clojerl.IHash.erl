-module('clojerl.IHash').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['hash'/1]).
-export([?SATISFIES/1]).

-callback 'hash'(any()) -> any().

'hash'(X) ->
  case X of
    #{?TYPE := 'erlang.util.UUID'} ->
      'erlang.util.UUID':'hash'(X);
    #{?TYPE := 'erlang.util.Regex'} ->
      'erlang.util.Regex':'hash'(X);
    #{?TYPE := 'erlang.Type'} ->
      'erlang.Type':'hash'(X);
    #{?TYPE := 'erlang.util.Date'} ->
      'erlang.util.Date':'hash'(X);
    #{?TYPE := 'clojerl.Reduced'} ->
      'clojerl.Reduced':'hash'(X);
    #{?TYPE := 'clojerl.ProcessVal'} ->
      'clojerl.ProcessVal':'hash'(X);
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'hash'(X);
    #{?TYPE := 'clojerl.Delay'} ->
      'clojerl.Delay':'hash'(X);
    #{?TYPE := 'clojerl.reader.ReaderConditional'} ->
      'clojerl.reader.ReaderConditional':'hash'(X);
    #{?TYPE := 'clojerl.Atom'} ->
      'clojerl.Atom':'hash'(X);
    #{?TYPE := 'clojerl.Symbol'} ->
      'clojerl.Symbol':'hash'(X);
    #{?TYPE := 'clojerl.IllegalAccessError'} ->
      'clojerl.IllegalAccessError':'hash'(X);
    #{?TYPE := 'clojerl.AssertionError'} ->
      'clojerl.AssertionError':'hash'(X);
    #{?TYPE := 'clojerl.Error'} ->
      'clojerl.Error':'hash'(X);
    #{?TYPE := 'clojerl.BadArgumentError'} ->
      'clojerl.BadArgumentError':'hash'(X);
    #{?TYPE := 'clojerl.ArityError'} ->
      'clojerl.ArityError':'hash'(X);
    #{?TYPE := 'clojerl.IOError'} ->
      'clojerl.IOError':'hash'(X);
    #{?TYPE := 'clojerl.ExceptionInfo'} ->
      'clojerl.ExceptionInfo':'hash'(X);
    #{?TYPE := 'clojerl.Namespace'} ->
      'clojerl.Namespace':'hash'(X);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'hash'(X);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'hash'(X);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'hash'(X);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'hash'(X);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'hash'(X);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'hash'(X);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'hash'(X);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'hash'(X);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'hash'(X);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'hash'(X);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'hash'(X);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'hash'(X);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'hash'(X);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'hash'(X);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'hash'(X);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'hash'(X);
    #{?TYPE := 'clojerl.TupleChunk'} ->
      'clojerl.TupleChunk':'hash'(X);
    #{?TYPE := 'clojerl.Fn'} ->
      'clojerl.Fn':'hash'(X);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'hash', X);
    ZZZ when is_binary(ZZZ) ->
      'clojerl.String':'hash'(X);
    ZZZ when is_bitstring(ZZZ) ->
      'clojerl.BitString':'hash'(X);
    ZZZ when is_integer(ZZZ) ->
      'clojerl.Integer':'hash'(X);
    ZZZ when is_float(ZZZ) ->
      'clojerl.Float':'hash'(X);
    ZZZ when is_boolean(ZZZ) ->
      'clojerl.Boolean':'hash'(X);
    ZZZ when is_list(ZZZ) ->
      'erlang.List':'hash'(X);
    ZZZ when is_map(ZZZ) ->
      'erlang.Map':'hash'(X);
    ZZZ when is_tuple(ZZZ) ->
      'erlang.Tuple':'hash'(X);
    ZZZ when is_function(ZZZ) ->
      'erlang.Fn':'hash'(X);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'hash', X);
    ZZZ when is_atom(ZZZ) ->
      'clojerl.Keyword':'hash'(X);
    ZZZ when is_port(ZZZ) ->
      'erlang.Port':'hash'(X);
    ZZZ when is_pid(ZZZ) ->
      'erlang.Process':'hash'(X);
    ZZZ when is_reference(ZZZ) ->
      'erlang.Reference':'hash'(X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'hash', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'erlang.util.UUID'} -> true;
    #{?TYPE := 'erlang.util.Regex'} -> true;
    #{?TYPE := 'erlang.Type'} -> true;
    #{?TYPE := 'erlang.util.Date'} -> true;
    #{?TYPE := 'clojerl.Reduced'} -> true;
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
    #{?TYPE := 'clojerl.Namespace'} -> true;
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
    #{?TYPE := 'clojerl.Fn'} -> true;
    #{?TYPE := _} -> false;
    ZZZ when is_binary(ZZZ) -> true;
    ZZZ when is_bitstring(ZZZ) -> true;
    ZZZ when is_integer(ZZZ) -> true;
    ZZZ when is_float(ZZZ) -> true;
    ZZZ when is_boolean(ZZZ) -> true;
    ZZZ when is_list(ZZZ) -> true;
    ZZZ when is_map(ZZZ) -> true;
    ZZZ when is_tuple(ZZZ) -> true;
    ZZZ when is_function(ZZZ) -> true;
    ?NIL -> false;
    ZZZ when is_atom(ZZZ) -> true;
    ZZZ when is_port(ZZZ) -> true;
    ZZZ when is_pid(ZZZ) -> true;
    ZZZ when is_reference(ZZZ) -> true;
    _ -> false
  end.
