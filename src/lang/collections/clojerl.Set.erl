-module('clojerl.Set').

-export([
         new/1,
         to_list/1
        ]).

-type type() :: {?MODULE, gb_sets:set()}.

-spec new(list()) -> type().
new(Values) when is_list(Values) ->
  {?MODULE, gb_sets:from_list(Values), #{}}.

-spec to_list(type()) -> list().
to_list({_, Set, _}) ->
  gb_sets:to_list(Set).
