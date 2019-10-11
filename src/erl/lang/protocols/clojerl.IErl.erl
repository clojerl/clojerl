-module('clojerl.IErl').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['->erl'/2]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback '->erl'(any(), any()) -> any().
-optional_callbacks(['->erl'/2]).

'->erl'(X, Recursive) ->
  case X of
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'->erl'(X, Recursive);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'->erl'(X, Recursive);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'->erl'(X, Recursive);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'->erl'(X, Recursive);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'->erl'(X, Recursive);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'->erl'(X, Recursive);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'->erl'(X, Recursive);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'->erl'(X, Recursive);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'->erl'(X, Recursive);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'->erl'(X, Recursive);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, '->erl', X);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, '->erl', X);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, '->erl', X);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, '->erl', X);
    _ ->
      clj_protocol:not_implemented(?MODULE, '->erl', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.ChunkedCons'} ->  true;
    #{?TYPE := 'clojerl.Cons'} ->  true;
    #{?TYPE := 'clojerl.LazySeq'} ->  true;
    #{?TYPE := 'clojerl.List'} ->  true;
    #{?TYPE := 'clojerl.Map'} ->  true;
    #{?TYPE := 'clojerl.Range'} ->  true;
    #{?TYPE := 'clojerl.TupleMap'} ->  true;
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
    'clojerl.ChunkedCons' -> true;
    'clojerl.Cons' -> true;
    'clojerl.LazySeq' -> true;
    'clojerl.List' -> true;
    'clojerl.Map' -> true;
    'clojerl.Range' -> true;
    'clojerl.TupleMap' -> true;
    'clojerl.Vector' -> true;
    'clojerl.Vector.ChunkedSeq' -> true;
    'clojerl.Vector.RSeq' -> true;
    _ -> false
  end.
