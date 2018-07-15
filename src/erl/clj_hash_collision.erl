-module(clj_hash_collision).

-include("clojerl.hrl").

-export([ get_entry/3
        , create_entry/4
        , without_entry/3
        ]).

-spec get_entry(map(), integer(), any()) -> ?NIL | {any(), any()}.
get_entry(Map, Hash, Key) ->
  case maps:get(Hash, Map, ?NIL) of
    ?NIL -> ?NIL;
    {K, V} -> {K, V};
    KVs -> find_entry(KVs, Key)
  end.

-spec find_entry([{any(), any()}], any()) -> ?NIL | {any(), any()}.
find_entry([], _) ->
  ?NIL;
find_entry([{K, V} | Rest], Key) ->
  case clj_rt:equiv(K, Key) of
    true  -> {K, V};
    false -> find_entry(Rest, Key)
  end.

-spec create_entry(map(), integer(), any(), any()) ->
  {any(), any()} | [{any(), any()}].
create_entry(Map, Hash, Key, Value) ->
  case maps:get(Hash, Map, ?NIL) of
    ?NIL -> {Key, Value};
    {K, V} ->
      case clj_rt:equiv(Key, K) of
        true  -> {K, Value};
        false -> [{K, V}, {Key, Value}]
      end;
    KVs ->
      assoc_entry(KVs, Key, Value, [])
  end.

-spec assoc_entry([{any(), any()}], any(), any(), [{any(), any()}]) ->
  [{any(), any()}].
assoc_entry([], Key, Value, Acc) ->
  [{Key, Value} | Acc];
assoc_entry([{K, V} | Rest], Key, Value, Acc) ->
  case clj_rt:equiv(K, Key) of
    true  -> Acc ++ [{K, Value} | Rest];
    false -> assoc_entry(Rest, Key, Value, [{K, V} | Acc])
  end.

-spec without_entry(map(), integer(), any()) -> map().
without_entry(Map, Hash, Key) ->
  case maps:get(Hash, Map, ?NIL) of
    ?NIL   -> Map;
    {_, _} -> maps:remove(Hash, Map);
    KVs0   ->
      KVs1 = remove_entry(KVs0, Key, []),
      Map#{Hash => KVs1}
  end.

-spec remove_entry([{any(), any()}], integer(), any()) -> [{any(), any()}].
remove_entry([], _Key, Acc) ->
  Acc;
remove_entry([{K, V} | Rest], Key, Acc) ->
  case clj_rt:equiv(K, Key) of
    true  -> Acc ++ Rest;
    false -> remove_entry(Rest, Key, [{K, V} | Acc])
  end.
