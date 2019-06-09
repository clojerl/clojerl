-module('clojerl.IStringable').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['str'/1]).
-export([?SATISFIES/1]).

-callback 'str'(any()) -> any().

'str'(X) ->
  case X of
    #{?TYPE := 'erlang.io.PushbackReader'} ->
      'erlang.io.PushbackReader':'str'(X);
    #{?TYPE := 'erlang.util.UUID'} ->
      'erlang.util.UUID':'str'(X);
    #{?TYPE := 'erlang.io.StringReader'} ->
      'erlang.io.StringReader':'str'(X);
    #{?TYPE := 'erlang.io.StringWriter'} ->
      'erlang.io.StringWriter':'str'(X);
    #{?TYPE := 'erlang.util.Regex'} ->
      'erlang.util.Regex':'str'(X);
    #{?TYPE := 'erlang.Type'} ->
      'erlang.Type':'str'(X);
    #{?TYPE := 'erlang.util.Date'} ->
      'erlang.util.Date':'str'(X);
    #{?TYPE := 'erlang.io.File'} ->
      'erlang.io.File':'str'(X);
    #{?TYPE := 'clojerl.Reduced'} ->
      'clojerl.Reduced':'str'(X);
    #{?TYPE := 'clojerl.TransducerSeq'} ->
      'clojerl.TransducerSeq':'str'(X);
    #{?TYPE := 'clojerl.reader.TaggedLiteral'} ->
      'clojerl.reader.TaggedLiteral':'str'(X);
    #{?TYPE := 'clojerl.ProcessVal'} ->
      'clojerl.ProcessVal':'str'(X);
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'str'(X);
    #{?TYPE := 'clojerl.Delay'} ->
      'clojerl.Delay':'str'(X);
    #{?TYPE := 'clojerl.reader.ReaderConditional'} ->
      'clojerl.reader.ReaderConditional':'str'(X);
    #{?TYPE := 'clojerl.Atom'} ->
      'clojerl.Atom':'str'(X);
    #{?TYPE := 'clojerl.Symbol'} ->
      'clojerl.Symbol':'str'(X);
    #{?TYPE := 'clojerl.IllegalAccessError'} ->
      'clojerl.IllegalAccessError':'str'(X);
    #{?TYPE := 'clojerl.AssertionError'} ->
      'clojerl.AssertionError':'str'(X);
    #{?TYPE := 'clojerl.Error'} ->
      'clojerl.Error':'str'(X);
    #{?TYPE := 'clojerl.BadArgumentError'} ->
      'clojerl.BadArgumentError':'str'(X);
    #{?TYPE := 'clojerl.ArityError'} ->
      'clojerl.ArityError':'str'(X);
    #{?TYPE := 'clojerl.IOError'} ->
      'clojerl.IOError':'str'(X);
    #{?TYPE := 'clojerl.ExceptionInfo'} ->
      'clojerl.ExceptionInfo':'str'(X);
    #{?TYPE := 'clojerl.Namespace'} ->
      'clojerl.Namespace':'str'(X);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'str'(X);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'str'(X);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'str'(X);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'str'(X);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'str'(X);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'str'(X);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'str'(X);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'str'(X);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'str'(X);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'str'(X);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'str'(X);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'str'(X);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'str'(X);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'str'(X);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'str'(X);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'str'(X);
    #{?TYPE := 'clojerl.Fn'} ->
      'clojerl.Fn':'str'(X);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'str', X);
    ZZZ when is_binary(ZZZ) ->
      'clojerl.String':'str'(X);
    ZZZ when is_bitstring(ZZZ) ->
      'clojerl.BitString':'str'(X);
    ZZZ when is_integer(ZZZ) ->
      'clojerl.Integer':'str'(X);
    ZZZ when is_float(ZZZ) ->
      'clojerl.Float':'str'(X);
    ZZZ when is_boolean(ZZZ) ->
      'clojerl.Boolean':'str'(X);
    ZZZ when is_list(ZZZ) ->
      'erlang.List':'str'(X);
    ZZZ when is_map(ZZZ) ->
      'erlang.Map':'str'(X);
    ZZZ when is_tuple(ZZZ) ->
      'erlang.Tuple':'str'(X);
    ZZZ when is_function(ZZZ) ->
      'erlang.Fn':'str'(X);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'str', X);
    ZZZ when is_atom(ZZZ) ->
      'clojerl.Keyword':'str'(X);
    ZZZ when is_port(ZZZ) ->
      'erlang.Port':'str'(X);
    ZZZ when is_pid(ZZZ) ->
      'erlang.Process':'str'(X);
    ZZZ when is_reference(ZZZ) ->
      'erlang.Reference':'str'(X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'str', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'erlang.io.PushbackReader'} -> true;
    #{?TYPE := 'erlang.util.UUID'} -> true;
    #{?TYPE := 'erlang.io.StringReader'} -> true;
    #{?TYPE := 'erlang.io.StringWriter'} -> true;
    #{?TYPE := 'erlang.util.Regex'} -> true;
    #{?TYPE := 'erlang.Type'} -> true;
    #{?TYPE := 'erlang.util.Date'} -> true;
    #{?TYPE := 'erlang.io.File'} -> true;
    #{?TYPE := 'clojerl.Reduced'} -> true;
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
