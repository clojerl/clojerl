-module('clojerl.erlang.Map').

-behaviour('clojerl.Stringable').

-export(['clojerl.Stringable.str'/1]).

'clojerl.Stringable.str'(Map) when is_map(Map) ->
  StrFun = fun(Key) ->
               KeyStr = clj_core:str(Key),
               ValStr = clj_core:str(maps:get(Key, Map)),

               clj_utils:binary_join([KeyStr, ValStr], <<" ">>)
           end,
  KeyValueStrs = lists:map(StrFun, maps:keys(Map)),
  Strs = clj_utils:binary_join(KeyValueStrs, <<", ">>),
  <<"{", Strs/binary, "}">>.
