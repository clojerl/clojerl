-module(clj_hash_collision).

-include("clojerl.hrl").

-export([ get_entry/3
        , create_entry/4
        , without_entry/3
        , equiv/2
        ]).

-type entry()    :: {any(), any()} | [{any(), any()}].
-type mappings() :: #{integer() => entry()}.

-spec get_entry(mappings(), integer(), any()) -> ?NIL | {any(), any()}.
get_entry(Map, Hash, Key) ->
  case maps:get(Hash, Map, ?NIL) of
    ?NIL -> ?NIL;
    {Key, V} -> {Key, V};
    KVs when is_list(KVs) -> find_entry(KVs, Key);
    _ -> ?NIL
  end.

-spec create_entry(map(), integer(), any(), any()) ->
  {0 | 1, {any(), any()} | [{any(), any()}]}.
create_entry(Map, Hash, Key, Value) ->
  case maps:get(Hash, Map, ?NIL) of
    ?NIL -> {1, {Key, Value}};
    {K, V} ->
      case clj_rt:equiv(Key, K) of
        true  -> {0, {K, Value}};
        false -> {1, [{K, V}, {Key, Value}]}
      end;
    KVs ->
      assoc_entry(KVs, Key, Value, [])
  end.

-spec without_entry(map(), integer(), any()) -> {-1 | 0, map()}.
without_entry(Map, Hash, Key) ->
  case maps:get(Hash, Map, ?NIL) of
    ?NIL   -> {0, Map};
    {_, _} -> {-1, maps:remove(Hash, Map)};
    KVs0   ->
      case remove_entry(KVs0, Key, []) of
        {Diff, []}   -> {Diff, maps:remove(Hash, Map)};
        {Diff, KVs1} -> {Diff, Map#{Hash => KVs1}}
      end
  end.

-spec equiv(map(), map()) -> boolean().
equiv(MapX, MapY) ->
  do_equiv(maps:to_list(MapX), MapY).

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec find_entry([{any(), any()}], any()) -> ?NIL | {any(), any()}.
find_entry([], _) ->
  ?NIL;
find_entry([{K, V} | Rest], Key) ->
  case clj_rt:equiv(K, Key) of
    true  -> {K, V};
    false -> find_entry(Rest, Key)
  end.

-spec assoc_entry([{any(), any()}], any(), any(), [{any(), any()}]) ->
  {0 | 1, [{any(), any()}]}.
assoc_entry([], Key, Value, Acc) ->
  {1, [{Key, Value} | Acc]};
assoc_entry([{K, V} | Rest], Key, Value, Acc) ->
  case clj_rt:equiv(K, Key) of
    true  -> {0, Acc ++ [{K, Value} | Rest]};
    false -> assoc_entry(Rest, Key, Value, [{K, V} | Acc])
  end.

-spec remove_entry([{any(), any()}], integer(), any()) ->
  {-1 | 0, [{any(), any()}]}.
remove_entry([], _Key, Acc) ->
  {0, Acc};
remove_entry([{K, V} | Rest], Key, Acc) ->
  case clj_rt:equiv(K, Key) of
    true  -> {-1, Acc ++ Rest};
    false -> remove_entry(Rest, Key, [{K, V} | Acc])
  end.

-spec do_equiv([entry()], mappings()) -> boolean().
do_equiv([], _) ->
  true;
do_equiv([{Hash, {K, V}} | Rest], MapSet) ->
  case get_entry(MapSet, Hash, K) of
    ?NIL    -> false;
    {_, V1} -> clj_rt:equiv(V, V1) andalso do_equiv(Rest, MapSet)
  end;
do_equiv([{Hash, KVs} | Rest], MapSet) ->
  case do_equiv_values(KVs, Hash, MapSet) of
    false -> false;
    _     -> do_equiv(Rest, MapSet)
  end.

-spec do_equiv_values([entry()], integer(), mappings()) -> boolean().
do_equiv_values([], _Hash, _MapSet) ->
  true;
do_equiv_values([{K, V} | Rest], Hash, MapSet) ->
  case get_entry(MapSet, Hash, K) of
    ?NIL    -> false;
    {_, V1} -> clj_rt:equiv(V, V1) andalso do_equiv_values(Rest, Hash, MapSet)
  end.
