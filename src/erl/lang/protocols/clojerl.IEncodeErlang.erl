%%% Code generate by scripts/generate-protocols
-module('clojerl.IEncodeErlang').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['clj->erl'/2]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'clj->erl'(any(), any()) -> any().
-optional_callbacks(['clj->erl'/2]).

'clj->erl'(X, Recursive) ->
  case X of
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'clj->erl'(X, Recursive);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'clj->erl'(X, Recursive);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'clj->erl'(X, Recursive);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'clj->erl'(X, Recursive);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'clj->erl'(X, Recursive);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'clj->erl'(X, Recursive);
    #{?TYPE := 'clojerl.Subvec'} ->
      'clojerl.Subvec':'clj->erl'(X, Recursive);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'clj->erl'(X, Recursive);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'clj->erl'(X, Recursive);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'clj->erl'(X, Recursive);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'clj->erl'(X, Recursive);
    #{?TYPE := 'clojerl.Vector.Seq'} ->
      'clojerl.Vector.Seq':'clj->erl'(X, Recursive);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'clj->erl', X);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'clj->erl', X);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'clj->erl', X);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'clj->erl', X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'clj->erl', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.ChunkedCons'} ->  true;
    #{?TYPE := 'clojerl.Cons'} ->  true;
    #{?TYPE := 'clojerl.LazySeq'} ->  true;
    #{?TYPE := 'clojerl.List'} ->  true;
    #{?TYPE := 'clojerl.Map'} ->  true;
    #{?TYPE := 'clojerl.Range'} ->  true;
    #{?TYPE := 'clojerl.Subvec'} ->  true;
    #{?TYPE := 'clojerl.TupleMap'} ->  true;
    #{?TYPE := 'clojerl.Vector'} ->  true;
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->  true;
    #{?TYPE := 'clojerl.Vector.RSeq'} ->  true;
    #{?TYPE := 'clojerl.Vector.Seq'} ->  true;
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
    'clojerl.Subvec' -> true;
    'clojerl.TupleMap' -> true;
    'clojerl.Vector' -> true;
    'clojerl.Vector.ChunkedSeq' -> true;
    'clojerl.Vector.RSeq' -> true;
    'clojerl.Vector.Seq' -> true;
    _ -> false
  end.
