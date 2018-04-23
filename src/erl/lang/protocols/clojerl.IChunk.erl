-module('clojerl.IChunk').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['drop_first'/1]).
-export([?SATISFIES/1]).

-callback 'drop_first'(any()) -> any().

'drop_first'(Chunk) ->
  case clj_rt:type_module(Chunk) of
    'clojerl.TupleChunk' ->
      'clojerl.TupleChunk':'drop_first'(Chunk);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'drop_first', Type)
  end.

?SATISFIES('clojerl.TupleChunk') -> true;
?SATISFIES(_) -> false.
