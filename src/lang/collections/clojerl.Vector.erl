-module('clojerl.Vector').

-export([
         new/1,
         to_list/1
        ]).

-type type() :: {?MODULE, array:array()}.

-spec new(list()) -> type().
new(Items) ->
  {?MODULE, array:from_list(Items)}.

-spec to_list(type()) -> list().
to_list({_, Items}) ->
  array:to_list(Items).
