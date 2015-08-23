-module('clojerl.Set').

-export([
         new/1
        ]).

-type type() :: {?MODULE, gb_sets:set()}.

-spec new(list()) -> type().
new(Values) ->
  {?MODULE, gb_sets:from_list(Values)}.
