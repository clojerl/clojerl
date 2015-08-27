-module('clojerl.IMeta').

-export([meta/1]).

-type type() ::  any().

-callback meta(any()) -> any().

-spec meta(type()) -> any().
meta(X) ->
  'clojerl.protocol':resolve(?MODULE, meta, [X]).
