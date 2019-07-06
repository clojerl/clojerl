-module('clojerl.IChunk').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['drop_first'/1]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'drop_first'(any()) -> any().

'drop_first'(Chunk) ->
  case Chunk of
    #{?TYPE := 'clojerl.TupleChunk'} ->
      'clojerl.TupleChunk':'drop_first'(Chunk);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'drop_first', Chunk);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'drop_first', Chunk);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'drop_first', Chunk);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'drop_first', Chunk);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'drop_first', Chunk)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.TupleChunk'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.TupleChunk' -> true;
    _ -> false
  end.
