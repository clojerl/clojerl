-module('clojerl.IReduceInit').

-clojure(true).
-protocol(true).

-export([reduce/3]).

-type type() :: any().

-callback reduce(type(), any(), any()) -> any().

-spec reduce(type(), any(), any()) -> any().
reduce(Coll, Fun, Val) ->
  'clojerl.protocol':resolve(?MODULE, reduce, [Coll, Fun, Val]).
