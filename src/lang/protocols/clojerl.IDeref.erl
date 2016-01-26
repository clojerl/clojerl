-module('clojerl.IDeref').

-export([deref/1]).

-type type() ::  any().

-callback 'clojerl.IDeref.deref'(any()) -> any().

-spec deref(type()) -> any().
deref(Ref) ->
  'clojerl.protocol':resolve(?MODULE, deref, [Ref]).
