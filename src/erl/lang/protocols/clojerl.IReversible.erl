-module('clojerl.IReversible').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['rseq'/1]).
-export([?SATISFIES/1]).

-callback 'rseq'(any()) -> any().

-spec 'rseq'(any()) -> no_return().
'rseq'(Seq) ->
  case clj_rt:type_module(Seq) of
    Type ->
      clj_protocol:not_implemented(?MODULE, 'rseq', Type)
  end.

?SATISFIES(_) -> false.
