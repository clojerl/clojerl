-module('clojerl.IStack').

-clojure(true).
-protocol(true).

-export([peek/1, pop/1]).

-type type() :: any().

-callback peek(Stack :: type()) -> any().
-callback pop(Stack :: type()) -> any().

-spec peek(type()) -> any().
peek(Stack) ->
  'clojerl.protocol':resolve(?MODULE, peek, Stack).

-spec pop(type()) -> any().
pop(Stack) ->
  'clojerl.protocol':resolve(?MODULE, pop, Stack).
