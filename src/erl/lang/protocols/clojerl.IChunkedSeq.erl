-module('clojerl.IChunkedSeq').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['chunked_first'/1, 'chunked_next'/1, 'chunked_more'/1, '__satisfies?__'/1]).

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
    _ ->
      clj_protocol:resolve(?MODULE, 'chunked_first', Seq)
  end.

'chunked_next'(Seq) ->
  case clj_rt:type_module(Seq) of
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'chunked_next'(Seq);
    'clojerl.Range' ->
      'clojerl.Range':'chunked_next'(Seq);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'chunked_next'(Seq);
    _ ->
      clj_protocol:resolve(?MODULE, 'chunked_next', Seq)
  end.

'chunked_more'(Seq) ->
  case clj_rt:type_module(Seq) of
    'clojerl.ChunkedCons' ->
      'clojerl.ChunkedCons':'chunked_more'(Seq);
    'clojerl.Range' ->
      'clojerl.Range':'chunked_more'(Seq);
    'clojerl.Vector.ChunkedSeq' ->
      'clojerl.Vector.ChunkedSeq':'chunked_more'(Seq);
    _ ->
      clj_protocol:resolve(?MODULE, 'chunked_more', Seq)
  end.

?SATISFIES('clojerl.ChunkedCons') -> true;
?SATISFIES('clojerl.Range') -> true;
?SATISFIES('clojerl.Vector.ChunkedSeq') -> true;
?SATISFIES(_) -> false.
