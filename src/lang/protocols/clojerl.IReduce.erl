-module('clojerl.IReduce').

-clojure(true).
-protocol(true).

-export([reduce/2]).

-type type() :: any().

-callback reduce(type(), any()) -> any().

-spec reduce(type(), any()) -> any().
reduce(Coll, Fun) ->
  'clojerl.protocol':resolve(?MODULE, reduce, [Coll, Fun]).
