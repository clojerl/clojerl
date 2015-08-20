-module(clj_vector).

-export([
         new/1,
         first/1
        ]).

-include("include/clj_types.hrl").

-spec new(list()) -> vector().
new(Items) ->
  {vector, array:from_list(Items)}.

-spec first(vector()) -> any().
first({vector, Array}) ->
  array:get(0, Array).
