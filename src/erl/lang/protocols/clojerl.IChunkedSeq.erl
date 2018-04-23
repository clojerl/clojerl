-module('clojerl.IChunkedSeq').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['chunked_first'/1, 'chunked_next'/1, 'chunked_more'/1]).
-export([?SATISFIES/1]).

-callback 'chunked_first'(any()) -> any().
-callback 'chunked_next'(any()) -> any().
-callback 'chunked_more'(any()) -> any().

'chunked_first'(Seq) ->
  case clj_rt:type_module(Seq) of
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'chunked_first'(Seq);
    'clojerl.Range' ->
      'clojerl.Range':'chunked_first'(Seq);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'chunked_first'(Seq);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'chunked_first', Type)
  end.

'chunked_next'(Seq) ->
  case clj_rt:type_module(Seq) of
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'chunked_next'(Seq);
    'clojerl.Range' ->
      'clojerl.Range':'chunked_next'(Seq);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'chunked_next'(Seq);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'chunked_next', Type)
  end.

'chunked_more'(Seq) ->
  case clj_rt:type_module(Seq) of
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'chunked_more'(Seq);
    'clojerl.Range' ->
      'clojerl.Range':'chunked_more'(Seq);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'chunked_more'(Seq);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'chunked_more', Type)
  end.

?SATISFIES('clojerl.ChunkedCons') -> true;
?SATISFIES('clojerl.Range') -> true;
?SATISFIES('clojerl.Vector.ChunkedSeq') -> true;
?SATISFIES(_) -> false.
