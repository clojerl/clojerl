-module('clojerl.IErl').

-clojure(true).
-protocol(true).

-export(['->erl'/2]).

-type type() :: any().

-callback '->erl'(type(), boolean()) -> any().

-spec '->erl'(type(), boolean()) -> any().
'->erl'(X, Recursive) ->
  clj_protocol:resolve(?MODULE, '->erl', X, Recursive).
