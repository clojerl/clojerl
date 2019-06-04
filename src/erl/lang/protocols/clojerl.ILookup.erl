-module('clojerl.ILookup').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['get'/2, 'get'/3]).
-export([?SATISFIES/1]).

-callback 'get'(any(), any()) -> any().
-callback 'get'(any(), any(), any()) -> any().

'get'(X, Key) ->
  case X of
    #{?TYPE := 'clojerl.reader.TaggedLiteral'} ->
      'clojerl.reader.TaggedLiteral':'get'(X, Key);
    #{?TYPE := 'clojerl.reader.ReaderConditional'} ->
      'clojerl.reader.ReaderConditional':'get'(X, Key);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'get'(X, Key);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'get'(X, Key);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'get'(X, Key);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'get'(X, Key);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'get'(X, Key);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'get'(X, Key);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'get', X);
    ZZZ when is_list(ZZZ) ->
      'erlang.List':'get'(X, Key);
    ZZZ when is_map(ZZZ) ->
      'erlang.Map':'get'(X, Key);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'get', X)
  end.

'get'(X, Key, NotFound) ->
  case X of
    #{?TYPE := 'clojerl.reader.TaggedLiteral'} ->
      'clojerl.reader.TaggedLiteral':'get'(X, Key, NotFound);
    #{?TYPE := 'clojerl.reader.ReaderConditional'} ->
      'clojerl.reader.ReaderConditional':'get'(X, Key, NotFound);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'get'(X, Key, NotFound);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'get'(X, Key, NotFound);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'get'(X, Key, NotFound);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'get'(X, Key, NotFound);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'get'(X, Key, NotFound);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'get'(X, Key, NotFound);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'get', X);
    ZZZ when is_list(ZZZ) ->
      'erlang.List':'get'(X, Key, NotFound);
    ZZZ when is_map(ZZZ) ->
      'erlang.Map':'get'(X, Key, NotFound);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'get', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.reader.TaggedLiteral'} -> true;
    #{?TYPE := 'clojerl.reader.ReaderConditional'} -> true;
    #{?TYPE := 'clojerl.SortedMap'} -> true;
    #{?TYPE := 'clojerl.TupleMap'} -> true;
    #{?TYPE := 'clojerl.Vector'} -> true;
    #{?TYPE := 'clojerl.Map'} -> true;
    #{?TYPE := 'clojerl.Set'} -> true;
    #{?TYPE := 'clojerl.SortedSet'} -> true;
    #{?TYPE := _} -> false;
    ZZZ when is_list(ZZZ) -> true;
    ZZZ when is_map(ZZZ) -> true;
    _ -> false
  end.
