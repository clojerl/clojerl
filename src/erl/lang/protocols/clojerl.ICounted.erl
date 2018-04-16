-module('clojerl.ICounted').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['count'/1]).
-export([?SATISFIES/1]).

-callback 'count'(any()) -> any().

'count'(Seq) ->
  case clj_rt:type_module(Seq) of
    'erlang.io.StringWriter' ->
      'erlang.io.StringWriter':'count'(Seq);
    'erlang.List' ->
      'erlang.List':'count'(Seq);
    'erlang.Map' ->
      'erlang.Map':'count'(Seq);
    'erlang.Tuple' ->
      'erlang.Tuple':'count'(Seq);
    'clojerl.BitString' ->
      'clojerl.BitString':'count'(Seq);
    'clojerl.String' ->
      'clojerl.String':'count'(Seq);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'count'(Seq);
    'clojerl.Cons' ->
      'clojerl.Cons':'count'(Seq);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'count'(Seq);
    'clojerl.List' ->
      'clojerl.List':'count'(Seq);
    'clojerl.Map' ->
      'clojerl.Map':'count'(Seq);
    'clojerl.Range' ->
      'clojerl.Range':'count'(Seq);
    'clojerl.Set' ->
      'clojerl.Set':'count'(Seq);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'count'(Seq);
    'clojerl.SortedSet' ->
      'clojerl.SortedSet':'count'(Seq);
    'clojerl.TupleChunk' ->
      'clojerl.TupleChunk':'count'(Seq);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'count'(Seq);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'count'(Seq);
    'clojerl.Vector' ->
      'clojerl.Vector':'count'(Seq);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'count', Type)
  end.

?SATISFIES('erlang.io.StringWriter') -> true;
?SATISFIES('erlang.List') -> true;
?SATISFIES('erlang.Map') -> true;
?SATISFIES('erlang.Tuple') -> true;
?SATISFIES('clojerl.BitString') -> true;
?SATISFIES('clojerl.String') -> true;
?SATISFIES('clojerl.ChunkedCons') -> true;
?SATISFIES('clojerl.Cons') -> true;
?SATISFIES('clojerl.LazySeq') -> true;
?SATISFIES('clojerl.List') -> true;
?SATISFIES('clojerl.Map') -> true;
?SATISFIES('clojerl.Range') -> true;
?SATISFIES('clojerl.Set') -> true;
?SATISFIES('clojerl.SortedMap') -> true;
?SATISFIES('clojerl.SortedSet') -> true;
?SATISFIES('clojerl.TupleChunk') -> true;
?SATISFIES('clojerl.TupleMap') -> true;
?SATISFIES('clojerl.Vector.ChunkedSeq') -> true;
?SATISFIES('clojerl.Vector') -> true;
?SATISFIES(_) -> false.
