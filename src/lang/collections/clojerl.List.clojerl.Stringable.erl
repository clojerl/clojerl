-module('clojerl.List.clojerl.Stringable').

-behaviour('clojerl.Stringable').

-export([str/1]).

str({'clojerl.List', [], _}) ->
  <<"()">>;
str({'clojerl.List', Items, _}) ->
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs = clj_utils:binary_join(ItemsStrs, <<" ">>),
  <<"(", Strs/binary, ")">>.
