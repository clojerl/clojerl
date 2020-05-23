-module('clojerl.IKVReduce').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['kv-reduce'/3]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'kv-reduce'(any(), any(), any()) -> any().
-optional_callbacks(['kv-reduce'/3]).

'kv-reduce'(Coll, Fun, Init) ->
  case Coll of
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'kv-reduce'(Coll, Fun, Init);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'kv-reduce'(Coll, Fun, Init);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'kv-reduce'(Coll, Fun, Init);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'kv-reduce', Coll);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'kv-reduce', Coll);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'kv-reduce', Coll);
    X_ when erlang:is_map(X_) ->
      'erlang.Map':'kv-reduce'(Coll, Fun, Init);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'kv-reduce', Coll);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'kv-reduce', Coll)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Map'} ->  true;
    #{?TYPE := 'clojerl.SortedMap'} ->  true;
    #{?TYPE := 'clojerl.TupleMap'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    X_ when erlang:is_map(X_) ->  true;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.Map' -> true;
    'clojerl.SortedMap' -> true;
    'clojerl.TupleMap' -> true;
    'erlang.Map' -> true;
    _ -> false
  end.
