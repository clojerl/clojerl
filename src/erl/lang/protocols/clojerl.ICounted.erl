-module('clojerl.ICounted').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['count'/1]).
-export([?SATISFIES/1]).

-callback 'count'(any()) -> any().

'count'(Seq) ->
  case clj_rt:type_module(Seq) of
    'erlang.Tuple' ->
      'erlang.Tuple':'count'(Seq);
    'erlang.Map' ->
      'erlang.Map':'count'(Seq);
    'erlang.io.StringWriter' ->
      'erlang.io.StringWriter':'count'(Seq);
    'erlang.List' ->
      'erlang.List':'count'(Seq);
    'clojerl.BitString' ->
      'clojerl.BitString':'count'(Seq);
    'clojerl.String' ->
      'clojerl.String':'count'(Seq);
    'clojerl.LazySeq' ->
      'clojerl.LazySeq':'count'(Seq);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'count'(Seq);
    'clojerl.Range' ->
      'clojerl.Range':'count'(Seq);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'count'(Seq);
    'clojerl.Vector.RSeq' ->
      'clojerl.Vector.RSeq':'count'(Seq);
    'clojerl.List' ->
      'clojerl.List':'count'(Seq);
    'clojerl.Vector' ->
      'clojerl.Vector':'count'(Seq);
    'clojerl.Map' ->
      'clojerl.Map':'count'(Seq);
    'clojerl.Cons' ->
      'clojerl.Cons':'count'(Seq);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'count'(Seq);
    'clojerl.Set' ->
      'clojerl.Set':'count'(Seq);
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'count'(Seq);
    'clojerl.SortedSet' ->
      'clojerl.SortedSet':'count'(Seq);
    'clojerl.TupleChunk' ->
      'clojerl.TupleChunk':'count'(Seq);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'count', Type)
  end.

?SATISFIES('erlang.Tuple') -> true;
?SATISFIES('erlang.Map') -> true;
?SATISFIES('erlang.io.StringWriter') -> true;
?SATISFIES('erlang.List') -> true;
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
?SATISFIES('clojerl.TupleChunk') -> true;
?SATISFIES(_) -> false.
