-module('clojerl.Vector.clojerl.Stringable').

-behaviour('clojerl.Stringable').

-export([str/1]).

str({'clojerl.Vector', Array}) ->
  Items = lists:map(fun clj_core:str/1, array:to_list(Array)),
  Strs = clj_utils:binary_join(Items, <<", ">>),
  <<"[", Strs/binary, "]">>.
