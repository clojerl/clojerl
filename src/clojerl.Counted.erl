-module('clojerl.Counted').

-export([count/1]).

-type type() :: any().

-callback count(Counted :: type()) -> integer().

-spec count(type()) -> type().
count(Seq) ->
  'clojerl.protocol':resolve('Counted', count, [Seq]).
