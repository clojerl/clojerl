-module('clojerl.IChunk').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['drop_first'/1]).
-export([?SATISFIES/1]).

-callback 'drop_first'(any()) -> any().

'drop_first'(Chunk) ->
  case Chunk of
    #{?TYPE := 'clojerl.TupleChunk'} ->
      'clojerl.TupleChunk':'drop_first'(Chunk);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'drop_first', Chunk);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'drop_first', Chunk);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'drop_first', Chunk)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.TupleChunk'} -> true;
    #{?TYPE := _} -> false;
    ?NIL -> false;
    _ -> false
  end.
