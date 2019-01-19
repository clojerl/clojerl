-module('clojerl.ISeqable').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['seq'/1, 'to_list'/1]).
-export([?SATISFIES/1]).

-callback 'seq'(any()) -> any().
-callback 'to_list'(any()) -> any().

'seq'(X) ->
  case clj_rt:type_module(X) of
    'erlang.Tuple' ->
      'erlang.Tuple':'seq'(X);
    'erlang.Map' ->
      'erlang.Map':'seq'(X);
    'erlang.List' ->
      'erlang.List':'seq'(X);
    'clojerl.TransducerSeq' ->
      'clojerl.TransducerSeq':'seq'(X);
    'clojerl.BitString' ->
      'clojerl.BitString':'seq'(X);
    'clojerl.String' ->
      'clojerl.String':'seq'(X);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'seq'(X);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'seq'(X);
    'clojerl.Range' ->
      'clojerl.Range':'seq'(X);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'seq'(X);
    'clojerl.Vector.RSeq' ->
      'clojerl.Vector.RSeq':'seq'(X);
    'clojerl.List' ->
      'clojerl.List':'seq'(X);
    'clojerl.Vector' ->
      'clojerl.Vector':'seq'(X);
    'clojerl.Map' ->
      'clojerl.Map':'seq'(X);
    'clojerl.Cons' ->
      'clojerl.Cons':'seq'(X);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'seq'(X);
    'clojerl.Set' ->
      'clojerl.Set':'seq'(X);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'seq'(X);
    'clojerl.SortedSet' ->
      'clojerl.SortedSet':'seq'(X);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'seq', Type)
  end.

'to_list'(X) ->
  case clj_rt:type_module(X) of
    'erlang.Tuple' ->
      'erlang.Tuple':'to_list'(X);
    'erlang.Map' ->
      'erlang.Map':'to_list'(X);
    'erlang.List' ->
      'erlang.List':'to_list'(X);
    'clojerl.TransducerSeq' ->
      'clojerl.TransducerSeq':'to_list'(X);
    'clojerl.BitString' ->
      'clojerl.BitString':'to_list'(X);
    'clojerl.String' ->
      'clojerl.String':'to_list'(X);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'to_list'(X);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'to_list'(X);
    'clojerl.Range' ->
      'clojerl.Range':'to_list'(X);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'to_list'(X);
    'clojerl.Vector.RSeq' ->
      'clojerl.Vector.RSeq':'to_list'(X);
    'clojerl.List' ->
      'clojerl.List':'to_list'(X);
    'clojerl.Vector' ->
      'clojerl.Vector':'to_list'(X);
    'clojerl.Map' ->
      'clojerl.Map':'to_list'(X);
    'clojerl.Cons' ->
      'clojerl.Cons':'to_list'(X);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'to_list'(X);
    'clojerl.Set' ->
      'clojerl.Set':'to_list'(X);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'to_list'(X);
    'clojerl.SortedSet' ->
      'clojerl.SortedSet':'to_list'(X);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'to_list', Type)
  end.

?SATISFIES('erlang.Tuple') -> true;
?SATISFIES('erlang.Map') -> true;
?SATISFIES('erlang.List') -> true;
?SATISFIES('clojerl.TransducerSeq') -> true;
?SATISFIES('clojerl.BitString') -> true;
?SATISFIES('clojerl.String') -> true;
?SATISFIES('clojerl.LazySeq') -> true;
?SATISFIES('clojerl.SortedMap') -> true;
?SATISFIES('clojerl.Range') -> true;
?SATISFIES('clojerl.TupleMap') -> true;
?SATISFIES('clojerl.Vector.RSeq') -> true;
?SATISFIES('clojerl.List') -> true;
?SATISFIES('clojerl.Vector') -> true;
?SATISFIES('clojerl.Map') -> true;
?SATISFIES('clojerl.Cons') -> true;
?SATISFIES('clojerl.Vector.ChunkedSeq') -> true;
?SATISFIES('clojerl.Set') -> true;
?SATISFIES('clojerl.ChunkedCons') -> true;
?SATISFIES('clojerl.SortedSet') -> true;
?SATISFIES(_) -> false.
