-module('clojerl.IChunk').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['drop_first'/1, '__satisfies?__'/1]).

-callback 'drop_first'(any()) -> any().

'drop_first'(Chunk) ->
  case clj_rt:type_module(Chunk) of
    'clojerl.TupleChunk' ->
      'clojerl.TupleChunk':'drop_first'(Chunk);
    _ ->
      clj_protocol:resolve(?MODULE, 'drop_first', Chunk)
  end.

?SATISFIES('clojerl.TupleChunk') -> true;
?SATISFIES(_) -> false.
