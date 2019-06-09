-module('clojerl.IReversible').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['rseq'/1]).
-export([?SATISFIES/1]).

-callback 'rseq'(any()) -> any().

'rseq'(Seq) ->
  case Seq of
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'rseq'(Seq);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'rseq', Seq);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'rseq', Seq);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'rseq', Seq)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Vector'} -> true;
    #{?TYPE := _} -> false;
    ?NIL -> false;
    _ -> false
  end.
