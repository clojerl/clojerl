-module('clojerl.IFn').

-clojure(true).
-protocol(true).

-export([apply/2]).

-type type() ::  any().

-callback apply(type(), any()) -> any().

-spec apply(type(), any()) -> any().
apply(Fn, Args) ->
  'clojerl.protocol':resolve(?MODULE, apply, Fn, Args).
