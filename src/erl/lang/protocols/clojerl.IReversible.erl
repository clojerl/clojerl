-module('clojerl.IReversible').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['rseq'/1, '__satisfies?__'/1]).

-callback 'rseq'(any()) -> any().

'rseq'(Seq) ->
  case clj_rt:type_module(Seq) of
    _ ->
      clj_protocol:resolve(?MODULE, 'rseq', Seq)
  end.

?SATISFIES(_) -> false.
