-module('clojerl.IDeref').

-clojure(true).
-protocol(true).

-export([deref/1]).

-type type() ::  any().

-callback deref(any()) -> any().

-spec deref(type()) -> any().
deref(Ref) ->
  'clojerl.protocol':resolve(?MODULE, deref, Ref).
