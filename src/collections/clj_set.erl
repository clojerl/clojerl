-module(clj_set).

-export([
         new/1
        ]).

-include("include/clj_types.hrl").

-spec new(list()) -> set().
new(Values) ->
  {set, gb_sets:from_list(Values)}.
