-module('clojerl.IFn').

-export([invoke/2]).

-type type() ::  any().

-callback 'clojerl.IFn.invoke'(type(), any()) -> any().

-spec invoke(type(), any()) -> any().
invoke(Fn, Args) ->
  'clojerl.protocol':resolve(?MODULE, invoke, [Fn, Args]).
