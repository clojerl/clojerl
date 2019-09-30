-module('clojerl.IReduce').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['reduce'/2, 'reduce'/3]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'reduce'(any(), any()) -> any().
-callback 'reduce'(any(), any(), any()) -> any().
-optional_callbacks(['reduce'/2, 'reduce'/3]).

'reduce'(Coll, Fun) ->
  case Coll of
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'reduce'(Coll, Fun);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'reduce'(Coll, Fun);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'reduce'(Coll, Fun);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'reduce'(Coll, Fun);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'reduce'(Coll, Fun);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'reduce'(Coll, Fun);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'reduce'(Coll, Fun);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'reduce'(Coll, Fun);
    #{?TYPE := 'clojerl.TupleChunk'} ->
      'clojerl.TupleChunk':'reduce'(Coll, Fun);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'reduce'(Coll, Fun);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'reduce'(Coll, Fun);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'reduce', Coll);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'reduce', Coll);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'reduce', Coll);
    X_ when erlang:is_list(X_) ->
      'erlang.List':'reduce'(Coll, Fun);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'reduce', Coll);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'reduce', Coll)
  end.

'reduce'(Coll, Fun, Init) ->
  case Coll of
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'reduce'(Coll, Fun, Init);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'reduce'(Coll, Fun, Init);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'reduce'(Coll, Fun, Init);
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'reduce'(Coll, Fun, Init);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'reduce'(Coll, Fun, Init);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'reduce'(Coll, Fun, Init);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'reduce'(Coll, Fun, Init);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'reduce'(Coll, Fun, Init);
    #{?TYPE := 'clojerl.TupleChunk'} ->
      'clojerl.TupleChunk':'reduce'(Coll, Fun, Init);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'reduce'(Coll, Fun, Init);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'reduce'(Coll, Fun, Init);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'reduce', Coll);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'reduce', Coll);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'reduce', Coll);
    X_ when erlang:is_list(X_) ->
      'erlang.List':'reduce'(Coll, Fun, Init);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'reduce', Coll);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'reduce', Coll)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.ChunkedCons'} ->  true;
    #{?TYPE := 'clojerl.Cons'} ->  true;
    #{?TYPE := 'clojerl.Cycle'} ->  true;
    #{?TYPE := 'clojerl.Iterate'} ->  true;
    #{?TYPE := 'clojerl.LazySeq'} ->  true;
    #{?TYPE := 'clojerl.List'} ->  true;
    #{?TYPE := 'clojerl.Range'} ->  true;
    #{?TYPE := 'clojerl.Repeat'} ->  true;
    #{?TYPE := 'clojerl.TupleChunk'} ->  true;
    #{?TYPE := 'clojerl.Vector'} ->  true;
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    X_ when erlang:is_list(X_) ->  true;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.ChunkedCons' -> true;
    'clojerl.Cons' -> true;
    'clojerl.Cycle' -> true;
    'clojerl.Iterate' -> true;
    'clojerl.LazySeq' -> true;
    'clojerl.List' -> true;
    'clojerl.Range' -> true;
    'clojerl.Repeat' -> true;
    'clojerl.TupleChunk' -> true;
    'clojerl.Vector' -> true;
    'clojerl.Vector.ChunkedSeq' -> true;
    'erlang.List' -> true;
    _ -> false
  end.
