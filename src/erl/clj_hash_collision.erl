%% @doc Hash collision handling for data structures.
%%
%% Implements utility functions for dealing with the collision of
%% hashes in data structures such as Clojure maps and sets.
-module(clj_hash_collision).

-include("clojerl.hrl").

-export([ get_entry/3
        , create_entry/4
        , without_entry/3
        , equiv/2
        ]).

-type entry()   :: {any(), any()} | [{any(), any()}].
-type mapping() :: #{integer() => entry()}.

-export_type([entry/0, mapping/0]).

%% @doc Gets the entry for `Key' in `Map'.
%%
%% Returns `undefined' when the entry is not found.
-spec get_entry(mapping(), integer(), any()) -> ?NIL | {any(), any()}.
get_entry(Map, Hash, Key) ->
  case Map of
    #{Hash := {Key, V}} -> {Key, V};
    #{Hash := KVs} when is_list(KVs) ->
      find_entry(KVs, Key);
    #{Hash := {K, V}} ->
      case clj_rt:equiv(Key, K) of
        true -> {K, V};
        false -> ?NIL
      end;
    _ -> ?NIL
  end.

%% @doc Create a new entry.
%%
%% Returns a tuple with two elements. The first is either `0' (the
%% value for the provided key already existed) or `1' (a new value was
%% added to the entry). The second is the created entry.
-spec create_entry(mapping(), integer(), any(), any()) ->
  {0 | 1, entry()}.
create_entry(Map, Hash, Key, Value) ->
  case Map of
    #{Hash := {K, V}} ->
      case clj_rt:equiv(Key, K) of
        true  -> {0, {K, Value}};
        false -> {1, [{K, V}, {Key, Value}]}
      end;
    #{Hash := KVs} ->
      assoc_entry(KVs, Key, Value, []);
    _ -> {1, {Key, Value}}
  end.

%% @doc Removes the entry for `Key'.
%%
%% Returns a tuple with two elements. The first is either `-1' (the
%% `Key' was removed) or `0' (the `Key' was not found). The second is
%% the updated mapping.
-spec without_entry(mapping(), integer(), any()) -> {-1 | 0, mapping()}.
without_entry(Mapping, Hash, Key) ->
  case Mapping of
    #{Hash := {_, _}} -> {-1, maps:remove(Hash, Mapping)};
    #{Hash := KVs0}   ->
      case remove_entry(KVs0, Key, []) of
        {Diff, []}   -> {Diff, maps:remove(Hash, Mapping)};
        {Diff, KVs1} -> {Diff, Mapping#{Hash => KVs1}}
      end;
    _ -> {0, Mapping}
  end.

%% @doc Checks if the two mappings are equivalent.
-spec equiv(mapping(), mapping()) -> boolean().
equiv(MappingX, MappingY) ->
  do_equiv(maps:to_list(MappingX), MappingY).

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

-spec do_equiv([entry()], mapping()) -> boolean().
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

-spec do_equiv_values([entry()], integer(), mapping()) -> boolean().
do_equiv_values([], _Hash, _MapSet) ->
  true;
do_equiv_values([{K, V} | Rest], Hash, MapSet) ->
  case get_entry(MapSet, Hash, K) of
    ?NIL    -> false;
    {_, V1} -> clj_rt:equiv(V, V1) andalso do_equiv_values(Rest, Hash, MapSet)
  end.
