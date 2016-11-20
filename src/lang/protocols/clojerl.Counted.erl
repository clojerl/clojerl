-module('clojerl.Counted').

-clojure(true).
-protocol(true).

-export([count/1]).

-type type() :: any().

-callback count(Counted :: type()) -> integer().

-spec count(type()) -> type().
count(Seq) ->
  clj_protocol:resolve(?MODULE, count, Seq).
