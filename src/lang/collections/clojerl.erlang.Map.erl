-module('clojerl.erlang.Map').

-behavior('clojerl.Stringable').
-behavior('clojerl.Seqable').
-behavior('clojerl.IColl').

-export(['clojerl.Stringable.str'/1]).
-export(['clojerl.Seqable.seq'/1]).
-export([ 'clojerl.IColl.count'/1
        , 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        , 'clojerl.IColl.equiv'/2
        ]).

'clojerl.Stringable.str'(Map) when is_map(Map) ->
  StrFun = fun(Key) ->
               KeyStr = clj_core:str(Key),
               ValStr = clj_core:str(maps:get(Key, Map)),

               clj_utils:binary_join([KeyStr, ValStr], <<" ">>)
           end,
  KeyValueStrs = lists:map(StrFun, maps:keys(Map)),
  Strs = clj_utils:binary_join(KeyValueStrs, <<", ">>),
  <<"{", Strs/binary, "}">>.

'clojerl.Seqable.seq'(Map) when is_map(Map) ->
  FoldFun = fun(K, V, List) ->
                [clj_core:vector([K, V]) | List]
            end,
  case maps:fold(FoldFun, [], Map) of
    [] -> undefined;
    X -> X
  end.

'clojerl.IColl.count'(Map) -> maps:size(Map).

'clojerl.IColl.cons'(Map, X) ->
  case clj_core:seq(X) of
    [K, V] ->
      Map#{K => V};
    _ ->
      throw(<<"Can't conj something that is not"
              " a key/value pair to a map.">>)
  end.

'clojerl.IColl.empty'(_) -> #{}.

'clojerl.IColl.equiv'(X, X) -> true;
'clojerl.IColl.equiv'(_, _) -> false.
