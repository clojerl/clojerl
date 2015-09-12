-module('clojerl.Set.clojerl.Stringable').

-behaviour('clojerl.Stringable').

-export([str/1]).

str({'clojerl.Set', Set}) ->
  Items = lists:map(fun clj_core:str/1, gb_sets:to_list(Set)),
  Strs = clj_utils:binary_join(Items, <<", ">>),
  <<"#{", Strs/binary, "}">>.
