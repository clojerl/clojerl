-module('clojerl.erlang.List.clojerl.Stringable').

-behaviour('clojerl.Stringable').

-export([str/1]).

str([]) ->
  <<"()">>;
str(Items) ->
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs = clj_utils:binary_join(ItemsStrs, <<" ">>),
  <<"(", Strs/binary, ")">>.
