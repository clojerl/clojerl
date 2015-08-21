-module(clj_list).

-export([
         new/1,
         first/1
        ]).

-include("include/clj_types.hrl").

-spec new(list()) -> 'list*'().
new(Items) ->
  {list, array:from_list(Items)}.

-spec first(vector()) -> any().
first({list, Array}) ->
  array:get(0, Array).
