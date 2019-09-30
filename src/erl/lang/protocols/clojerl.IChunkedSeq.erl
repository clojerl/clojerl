-module('clojerl.IChunkedSeq').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['chunked_first'/1, 'chunked_next'/1, 'chunked_more'/1]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'chunked_first'(any()) -> any().
-callback 'chunked_next'(any()) -> any().
-callback 'chunked_more'(any()) -> any().
-optional_callbacks(['chunked_first'/1, 'chunked_next'/1, 'chunked_more'/1]).

'chunked_first'(Seq) ->
  case Seq of
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'chunked_first'(Seq);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'chunked_first'(Seq);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'chunked_first'(Seq);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'chunked_first', Seq);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'chunked_first', Seq);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'chunked_first', Seq);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'chunked_first', Seq);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'chunked_first', Seq)
  end.

'chunked_next'(Seq) ->
  case Seq of
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'chunked_next'(Seq);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'chunked_next'(Seq);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'chunked_next'(Seq);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'chunked_next', Seq);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'chunked_next', Seq);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'chunked_next', Seq);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'chunked_next', Seq);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'chunked_next', Seq)
  end.

'chunked_more'(Seq) ->
  case Seq of
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'chunked_more'(Seq);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'chunked_more'(Seq);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'chunked_more'(Seq);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'chunked_more', Seq);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'chunked_more', Seq);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'chunked_more', Seq);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'chunked_more', Seq);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'chunked_more', Seq)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.ChunkedCons'} ->  true;
    #{?TYPE := 'clojerl.Range'} ->  true;
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.ChunkedCons' -> true;
    'clojerl.Range' -> true;
    'clojerl.Vector.ChunkedSeq' -> true;
    _ -> false
  end.
