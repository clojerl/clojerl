-module('clojerl.Counted').

-export([count/1]).

-type type() :: any().

-callback 'clojerl.Counted.count'(Counted :: type()) -> integer().

-spec count(type()) -> type().
count(Seq) ->
  'clojerl.protocol':resolve(?MODULE, count, [Seq]).
