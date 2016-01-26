-module('clojerl.erlang.Tuple').

-behaviour('clojerl.Stringable').

-export(['clojerl.Stringable.str'/1]).

'clojerl.Stringable.str'(Tuple) when is_tuple(Tuple) ->
  Items = tuple_to_list(Tuple),
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs = clj_utils:binary_join(ItemsStrs, <<", ">>),
  <<"#[", Strs/binary, "]">>.
