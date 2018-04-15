-module('clojerl.IErl').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['->erl'/2]).
-export([?SATISFIES/1]).

-callback '->erl'(any(), any()) -> any().

'->erl'(X, Recursive) ->
  case clj_rt:type_module(X) of
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'->erl'(X, Recursive);
    'clojerl.Cons' ->
      'clojerl.Cons':'->erl'(X, Recursive);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'->erl'(X, Recursive);
    'clojerl.List' ->
      'clojerl.List':'->erl'(X, Recursive);
    'clojerl.Map' ->
      'clojerl.Map':'->erl'(X, Recursive);
    'clojerl.Range' ->
      'clojerl.Range':'->erl'(X, Recursive);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'->erl'(X, Recursive);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'->erl'(X, Recursive);
    'clojerl.Vector' ->
      'clojerl.Vector':'->erl'(X, Recursive);
    _ ->
      clj_protocol:resolve(?MODULE, '->erl', X, Recursive)
  end.

?SATISFIES('clojerl.ChunkedCons') -> true;
?SATISFIES('clojerl.Cons') -> true;
?SATISFIES('clojerl.LazySeq') -> true;
?SATISFIES('clojerl.List') -> true;
?SATISFIES('clojerl.Map') -> true;
?SATISFIES('clojerl.Range') -> true;
?SATISFIES('clojerl.TupleMap') -> true;
?SATISFIES('clojerl.Vector.ChunkedSeq') -> true;
?SATISFIES('clojerl.Vector') -> true;
?SATISFIES(_) -> false.
