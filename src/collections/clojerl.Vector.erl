-module('clojerl.Vector').

-export([
         new/1
        ]).

-type type() :: {?MODULE, array:array()}.

-spec new(list()) -> type().
new(Items) ->
  {?MODULE, array:from_list(Items)}.
