-module('clojerl.IFn').

-clojure(true).
-protocol(true).

-export([invoke/2]).

-type type() ::  any().

-callback invoke(type(), any()) -> any().

-spec invoke(type(), any()) -> any().
invoke(Fn, Args) ->
  'clojerl.protocol':resolve(?MODULE, invoke, [Fn, Args]).
