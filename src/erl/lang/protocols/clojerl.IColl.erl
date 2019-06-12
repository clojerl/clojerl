-module('clojerl.IColl').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['cons'/2, 'empty'/1]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'cons'(any(), any()) -> any().
-callback 'empty'(any()) -> any().

'cons'(Coll, Item) ->
  case Coll of
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'cons'(Coll, Item);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'cons'(Coll, Item);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'cons', Coll);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'cons', Coll);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'cons', Coll);
    X_ when erlang:is_list(X_) ->
      'erlang.List':'cons'(Coll, Item);
    X_ when erlang:is_map(X_) ->
      'erlang.Map':'cons'(Coll, Item);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'cons', Coll);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'cons', Coll)
  end.

'empty'(Coll) ->
  case Coll of
    #{?TYPE := 'clojerl.Iterate'} ->
      'clojerl.Iterate':'empty'(Coll);
    #{?TYPE := 'clojerl.Range'} ->
      'clojerl.Range':'empty'(Coll);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'empty'(Coll);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'empty'(Coll);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'empty'(Coll);
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->
      'clojerl.Vector.ChunkedSeq':'empty'(Coll);
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'empty'(Coll);
    #{?TYPE := 'clojerl.LazySeq'} ->
      'clojerl.LazySeq':'empty'(Coll);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'empty'(Coll);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'empty'(Coll);
    #{?TYPE := 'clojerl.Vector.RSeq'} ->
      'clojerl.Vector.RSeq':'empty'(Coll);
    #{?TYPE := 'clojerl.Cycle'} ->
      'clojerl.Cycle':'empty'(Coll);
    #{?TYPE := 'clojerl.Repeat'} ->
      'clojerl.Repeat':'empty'(Coll);
    #{?TYPE := 'clojerl.Cons'} ->
      'clojerl.Cons':'empty'(Coll);
    #{?TYPE := 'clojerl.ChunkedCons'} ->
      'clojerl.ChunkedCons':'empty'(Coll);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'empty'(Coll);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'empty', Coll);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'empty', Coll);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'empty', Coll);
    X_ when erlang:is_list(X_) ->
      'erlang.List':'empty'(Coll);
    X_ when erlang:is_map(X_) ->
      'erlang.Map':'empty'(Coll);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'empty', Coll);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'empty', Coll)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Iterate'} ->  true;
    #{?TYPE := 'clojerl.Range'} ->  true;
    #{?TYPE := 'clojerl.SortedMap'} ->  true;
    #{?TYPE := 'clojerl.Set'} ->  true;
    #{?TYPE := 'clojerl.Map'} ->  true;
    #{?TYPE := 'clojerl.Vector.ChunkedSeq'} ->  true;
    #{?TYPE := 'clojerl.List'} ->  true;
    #{?TYPE := 'clojerl.LazySeq'} ->  true;
    #{?TYPE := 'clojerl.Vector'} ->  true;
    #{?TYPE := 'clojerl.SortedSet'} ->  true;
    #{?TYPE := 'clojerl.Vector.RSeq'} ->  true;
    #{?TYPE := 'clojerl.Cycle'} ->  true;
    #{?TYPE := 'clojerl.Repeat'} ->  true;
    #{?TYPE := 'clojerl.Cons'} ->  true;
    #{?TYPE := 'clojerl.ChunkedCons'} ->  true;
    #{?TYPE := 'clojerl.TupleMap'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    X_ when erlang:is_list(X_) ->  true;
    X_ when erlang:is_map(X_) ->  true;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.Iterate' -> true;
    'clojerl.Range' -> true;
    'clojerl.SortedMap' -> true;
    'clojerl.Set' -> true;
    'clojerl.Map' -> true;
    'clojerl.Vector.ChunkedSeq' -> true;
    'clojerl.List' -> true;
    'clojerl.LazySeq' -> true;
    'clojerl.Vector' -> true;
    'clojerl.SortedSet' -> true;
    'clojerl.Vector.RSeq' -> true;
    'clojerl.Cycle' -> true;
    'clojerl.Repeat' -> true;
    'clojerl.Cons' -> true;
    'clojerl.ChunkedCons' -> true;
    'clojerl.TupleMap' -> true;
    'erlang.List' -> true;
    'erlang.Map' -> true;
    _ -> false
  end.
