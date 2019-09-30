-module('clojerl.IMeta').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['meta'/1, 'with_meta'/2]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'meta'(any()) -> any().
-callback 'with_meta'(any(), any()) -> any().
-optional_callbacks(['meta'/1, 'with_meta'/2]).

'meta'(X) ->
  case X of
    #{?TYPE := 'clojerl.Atom'} ->
      'clojerl.Atom':'meta'(X);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'meta'(X);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'meta'(X);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'meta'(X);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'meta'(X);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'meta'(X);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'meta'(X);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'meta'(X);
    #{?TYPE := 'clojerl.Namespace'} ->
      'clojerl.Namespace':'meta'(X);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'meta'(X);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'meta'(X);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'meta'(X);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'meta'(X);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'meta'(X);
    #{?TYPE := 'clojerl.Symbol'} ->
      'clojerl.Symbol':'meta'(X);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'meta'(X);
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'meta'(X);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'meta'(X);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'meta'(X);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'meta'(X);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'meta', X);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'meta', X);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'meta', X);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'meta', X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'meta', X)
  end.

'with_meta'(X, Meta) ->
  case X of
    #{?TYPE := 'clojerl.Atom'} ->
      'clojerl.Atom':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.Namespace'} ->
      'clojerl.Namespace':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.Symbol'} ->
      'clojerl.Symbol':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'with_meta'(X, Meta);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'with_meta'(X, Meta);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'with_meta', X);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'with_meta', X);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'with_meta', X);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'with_meta', X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'with_meta', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Atom'} ->  true;
    #{?TYPE := 'clojerl.ChunkedCons'} ->  true;
    #{?TYPE := 'clojerl.Cons'} ->  true;
    #{?TYPE := 'clojerl.Cycle'} ->  true;
    #{?TYPE := 'clojerl.Iterate'} ->  true;
    #{?TYPE := 'clojerl.LazySeq'} ->  true;
    #{?TYPE := 'clojerl.List'} ->  true;
    #{?TYPE := 'clojerl.Map'} ->  true;
    #{?TYPE := 'clojerl.Namespace'} ->  true;
    #{?TYPE := 'clojerl.Range'} ->  true;
    #{?TYPE := 'clojerl.Repeat'} ->  true;
    #{?TYPE := 'clojerl.Set'} ->  true;
    #{?TYPE := 'clojerl.SortedMap'} ->  true;
    #{?TYPE := 'clojerl.SortedSet'} ->  true;
    #{?TYPE := 'clojerl.Symbol'} ->  true;
    #{?TYPE := 'clojerl.TupleMap'} ->  true;
    #{?TYPE := 'clojerl.Var'} ->  true;
    #{?TYPE := 'clojerl.Vector'} ->  true;
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->  true;
    #{?TYPE := 'clojerl.Vector.RSeq'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.Atom' -> true;
    'clojerl.ChunkedCons' -> true;
    'clojerl.Cons' -> true;
    'clojerl.Cycle' -> true;
    'clojerl.Iterate' -> true;
    'clojerl.LazySeq' -> true;
    'clojerl.List' -> true;
    'clojerl.Map' -> true;
    'clojerl.Namespace' -> true;
    'clojerl.Range' -> true;
    'clojerl.Repeat' -> true;
    'clojerl.Set' -> true;
    'clojerl.SortedMap' -> true;
    'clojerl.SortedSet' -> true;
    'clojerl.Symbol' -> true;
    'clojerl.TupleMap' -> true;
    'clojerl.Var' -> true;
    'clojerl.Vector' -> true;
    'clojerl.Vector.ChunkedSeq' -> true;
    'clojerl.Vector.RSeq' -> true;
    _ -> false
  end.
