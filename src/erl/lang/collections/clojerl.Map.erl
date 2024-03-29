-module('clojerl.Map').

-compile({no_auto_import, [{apply, 2}]}).

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behavior('clojerl.IAssociative').
-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IEncodeErlang').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.IKVReduce').
-behavior('clojerl.ILookup').
-behavior('clojerl.IMap').
-behavior('clojerl.IMeta').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

-export([ ?CONSTRUCTOR/1
        , ?CONSTRUCTOR/2
        ]).
-export([fold/8]).

-export([ contains_key/2
        , entry_at/2
        , assoc/3
        ]).
-export([count/1]).
-export([ cons/2
        , empty/1
        ]).
-export([equiv/2]).
-export(['clj->erl'/2]).
-export([apply/2]).
-export([hash/1]).
-export(['kv-reduce'/3]).
-export([ get/2
        , get/3
        ]).
-export([ keys/1
        , vals/1
        , without/2
        ]).
-export([ meta/1
        , with_meta/2
        ]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

-import( clj_hash_collision
       , [ get_entry/3
         , create_entry/4
         , without_entry/3
         ]
       ).

-type entry()    :: clj_hash_collision:entry().
-type mappings() :: #{integer() => entry()}.

-export_type([type/0]).
-type type() :: #{ ?TYPE => ?M
                 , map   => mappings()
                 , count => non_neg_integer()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(KeyValues) when is_list(KeyValues) ->
  ?CONSTRUCTOR(KeyValues, false);
?CONSTRUCTOR(KeyValues) ->
  ?CONSTRUCTOR(clj_rt:to_list(KeyValues)).

-spec ?CONSTRUCTOR(list(), boolean()) -> type().
?CONSTRUCTOR(KeyValues, FailDuplicates) when is_list(KeyValues) ->
  KeyValuePairs = build_key_values([], KeyValues),
  {Count, Map, _} = lists:foldl( fun build_mappings/2
                               , {0, #{}, FailDuplicates}
                               , KeyValuePairs),
  #{ ?TYPE => ?M
   , map   => Map
   , meta  => ?NIL
   , count => Count
   }.

%% @private
-spec build_key_values(list(), list()) -> [{any(), any()}].
build_key_values(KeyValues, []) ->
  lists:reverse(KeyValues);
build_key_values(KeyValues, [K, V | Items]) ->
  build_key_values([{K, V} | KeyValues], Items).

%% @private
-spec build_mappings({any(), any()}, {integer(), mappings(), boolean()}) ->
  {integer(), mappings(), boolean()}.
build_mappings({Key, Value}, {Count, Map, FailDuplicates}) ->
  Hash = clj_rt:hash(Key),
  {Diff, Entry} = create_entry(Map, Hash, Key, Value),
  ?ERROR_WHEN( FailDuplicates andalso Diff == 0
             , [<<"Duplicate key: ">>, Key]
             ),
  {Count + Diff, Map#{Hash => Entry}, FailDuplicates}.

-spec fold(type(), integer(), any(), any(), any(), any(), any(), any()) ->
  any().
fold( #{?TYPE := ?M, map := Map} = M
    , N
    , Combine
    , Reduce
    , Invoke
    , Task
    , Fork
    , Join
    ) ->
  F = fun() ->
          Init = clj_rt:apply(Combine, []),
          case count(M) of
            0 -> Init;
            _ ->
              Entries = maps:values(Map),
              Partitions = partition_kvs(Entries, N),
              Tasks = [ create_task(KVs, Combine, Reduce, Task, Fork, Join)
                        || KVs <- Partitions
                      ],
              Result = fold_tasks( Tasks, length(Partitions), Combine
                                 , Reduce, Task, Fork, Join
                                 ),
              clj_rt:apply(Combine, [Init, Result])
          end
      end,
  clj_rt:apply(Invoke, [F]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IAssociative

contains_key(#{?TYPE := ?M, map := Map}, Key) ->
  ?NIL /= get_entry(Map, clj_rt:hash(Key), Key).

entry_at(#{?TYPE := ?M, map := Map}, Key) ->
  Hash = clj_rt:hash(Key),
  case get_entry(Map, Hash, Key) of
    ?NIL -> ?NIL;
    {K, V} -> 'clojerl.Vector':?CONSTRUCTOR([K, V])
  end.

assoc(#{?TYPE := ?M, map := Map, count := Count} = M, Key, Value) ->
  Hash = clj_rt:hash(Key),
  {Diff, Entry} = create_entry(Map, Hash, Key, Value),
  M#{map => Map#{Hash => Entry}, count => Count + Diff}.

%% clojerl.ICounted

count(#{?TYPE := ?M, count := Count}) -> Count.

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, count := Count, map := MapX}
     , #{?TYPE := ?M, count := Count, map := MapY}
     ) ->
  clj_hash_collision:equiv(MapX, MapY);
equiv(#{?TYPE := ?M, map := Map, count := Count}, Y) ->
  case clj_rt:'map?'(Y) of
    true  ->
      TypeModule   = clj_rt:type_module(Y),
      KeyHashFun   = fun(X) -> {X, clj_rt:hash(X)} end,
      KeyHashPairs = lists:map( KeyHashFun
                              , clj_rt:to_list(TypeModule:keys(Y))
                              ),
      Fun = fun({Key, Hash}) ->
                Entry = get_entry(Map, Hash, Key),
                Entry /= ?NIL andalso
                  clj_rt:equiv(Entry, {Key, TypeModule:get(Y, Key)})
            end,
      Count =:= TypeModule:count(Y)
        andalso lists:all(Fun, KeyHashPairs);
    false -> false
  end.

%% clojerl.IEncodeErlang

'clj->erl'(#{?TYPE := ?M, map := Map}, Recursive) ->
  ErlMapFun = fun
                (_, {Key0, Val0}, MapAcc) when Recursive ->
                  Key1 = clj_rt:'clj->erl'(Key0, Recursive),
                  Val1 = clj_rt:'clj->erl'(Val0, Recursive),
                  MapAcc#{Key1 => Val1};
                (_, {Key0, Val0}, MapAcc) ->
                  MapAcc#{Key0 => Val0};
                (_, KVs0, MapAcc) when Recursive ->
                  KVs1 = [ { clj_rt:'clj->erl'(K, Recursive)
                           , clj_rt:'clj->erl'(V, Recursive)
                           }
                           || {K, V} <- KVs0
                         ],
                  maps:merge(MapAcc, maps:from_list(KVs1));
                (_, KVs, MapAcc) ->
                  maps:merge(MapAcc, maps:from_list(KVs))
              end,
  maps:fold(ErlMapFun, #{}, Map).

%% clojerl.IFn

apply(#{?TYPE := ?M} = M, [Key]) ->
  apply(M, [Key, ?NIL]);
apply(#{?TYPE := ?M, map := Map}, [Key, NotFound]) ->
  Hash = clj_rt:hash(Key),
  case get_entry(Map, Hash, Key) of
    ?NIL -> NotFound;
    {_, Val} -> Val
  end;
apply(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for map, got: ", CountBin/binary>>).

%% clojerl.IColl

cons(#{?TYPE := ?M} = M, ?NIL) ->
  M;
cons(#{?TYPE := ?M} = M, X) ->
  IsVector = clj_rt:'vector?'(X),
  IsMap    = clj_rt:'map?'(X),
  case clj_rt:to_list(X) of
    [K, V] when IsVector ->
      assoc(M, K, V);
    KVs when IsMap ->
      Fun = fun(KV, Acc) ->
                [K, V] = clj_rt:to_list(KV),
                assoc(Acc, K, V)
            end,
      lists:foldl(Fun, M, KVs);
    _ ->
      ?ERROR([ <<"Can't conj something that is not a key/value pair or "
               "another map to a map, got: ">>
             , X
             ])
  end.

empty(_) -> ?CONSTRUCTOR([]).

%% clojerl.IHash

hash(#{?TYPE := ?M} = Map) ->
  clj_murmur3:unordered(Map).

%% clojerl.IKVReduce

'kv-reduce'(#{?TYPE := ?M, map := Map}, Fun, Init) ->
  Ref = make_ref(),
  ListFold =
    fun({K, V}, Acc0) ->
        Acc = 'clojerl.IFn':apply(Fun, [Acc0, K, V]),
        case 'clojerl.Reduced':is_reduced(Acc) of
          true  -> throw({reduced, Ref, Acc});
          false -> Acc
        end
    end,
  MapFold =
    fun
      (_, {K, V}, Acc) -> ListFold({K, V}, Acc);
      (_, KVs, Acc)    -> lists:foldl(ListFold, Acc, KVs)
    end,
  try maps:fold(MapFold, Init, Map)
  catch throw:{reduced, Ref, Acc} ->
      'clojerl.Reduced':deref(Acc)
  end.

%% clojerl.ILookup

get(#{?TYPE := ?M} = Map, Key) ->
  get(Map, Key, ?NIL).

get(#{?TYPE := ?M, map := Map}, Key, NotFound) ->
  Hash = clj_rt:hash(Key),
  case get_entry(Map, Hash, Key) of
    ?NIL -> NotFound;
    {_, Val} -> Val
  end.

%% clojerl.IMap

keys(#{?TYPE := ?M, map := Map}) ->
  case maps:size(Map) of
    0 -> ?NIL;
    _ -> maps:fold(fun keys_fold/3, [], Map)
  end.

keys_fold(_, {K, _}, Keys) ->
  [K | Keys];
keys_fold(_, KVs, Keys) ->
  [K || {K, _} <- KVs] ++ Keys.

vals(#{?TYPE := ?M, map := Map}) ->
  case maps:size(Map) of
    0 -> ?NIL;
    _ -> maps:fold(fun vals_fold/3, [], Map)
  end.

vals_fold(_, {_, V}, Vals) ->
  [V | Vals];
vals_fold(_, KVs, Vals) ->
  [V || {_, V} <- KVs] ++ Vals.

without(#{?TYPE := ?M, map := Map0, count := Count} = M, Key) ->
  Hash = clj_rt:hash(Key),
  {Diff, Map1} = without_entry(Map0, Hash, Key),
  M#{map => Map1, count => Count + Diff}.

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = M, Metadata) ->
  M#{meta => Metadata}.

%% clojerl.ISeqable

seq(#{?TYPE := ?M} = Map) ->
  case to_list(Map) of
    [] -> ?NIL;
    X -> X
  end.

to_list(#{?TYPE := ?M, map := Map}) ->
  maps:fold(fun to_list_fold/3, [], Map).

to_list_fold(_Hash, {K, V}, List) ->
  ['clojerl.Vector':?CONSTRUCTOR([K, V]) | List];
to_list_fold(_Hash, KVs, List) ->
  ['clojerl.Vector':?CONSTRUCTOR([K, V]) || {K, V} <- KVs] ++ List.

%% clojerl.IStringable

str(#{?TYPE := ?M} = M) ->
  clj_rt:print_str(M).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% Helper functions for fold/8 used by clojure.core.reducers

-spec partition_kvs([entry()], integer()) -> [[{any(), any()}]].
partition_kvs(Entries, N) ->
  {_, Partitions} = partition_kvs(Entries, N, N, [], []),
  Partitions.

-spec partition_kvs( [entry()], integer(), integer()
                   , [{any(), any()}], [[{any(), any()}]]
                   ) ->
  {integer(), [[{any(), any()}]]}.
partition_kvs([], _MaxN, N, [], Acc) ->
  {N, Acc};
partition_kvs([], _MaxN, N, Current, Acc) ->
  {N, [Current | Acc]};
partition_kvs(Entries, MaxN, 0, Current, Acc0) ->
  Acc = [Current | Acc0],
  partition_kvs(Entries, MaxN, MaxN, [], Acc);
partition_kvs([{_, _} = KV | Rest], MaxN, N, Current, Acc) ->
  partition_kvs(Rest, MaxN, N - 1, [KV | Current], Acc);
partition_kvs([KVs | Rest], MaxN, N0, Current0, Acc0) ->
  %% Entries where there was a hash conflict
  %% will contains a list of {K, V}.
  case partition_kvs(KVs, MaxN, N0, Current0, Acc0) of
    {0, Acc} ->
      partition_kvs(KVs, MaxN, MaxN, [], Acc);
    {N, [Current | Acc]} ->
      partition_kvs(Rest, MaxN, N, Current, Acc)
  end.

-spec create_task([entry()], any(), any(), any(), any(), any()) ->
  any().
create_task(KVs, Combine, Reduce, Task, Fork, Join) ->
  fun() ->
      fold_kvs(KVs, Combine, Reduce, Task, Fork, Join)
  end.

-spec fold_tasks([function()], integer(), any(), any(), any(), any(), any()) ->
  any().
fold_tasks([], _Count, Combine, _Reduce, _Task, _Fork, _Join) ->
  clj_rt:apply(Combine, []);
fold_tasks([Task], _Count, _Combine, _Reduce, _Task, _Fork, _Join) ->
  Task();
fold_tasks(Tasks, Count, Combine, Reduce, Task, Fork, Join) ->
  HalfCount = Count div 2,
  {Tasks1, Tasks2} = lists:split(HalfCount, Tasks),
  ForkedFun =
    fun() ->
        fold_tasks(Tasks2, Count - HalfCount, Combine, Reduce, Task, Fork, Join)
    end,
  Forked = clj_rt:apply(Fork, [clj_rt:apply(Task, [ForkedFun])]),
  Result = fold_tasks(Tasks1, HalfCount, Combine, Reduce, Task, Fork, Join),
  ResultJoin = clj_rt:apply(Join, [Forked]),
  clj_rt:apply(Combine, [Result, ResultJoin]).

-spec fold_kvs([entry()], any(), any(), any(), any(), any()) ->
  any().
fold_kvs(KVs, Combine, Reduce, _Task, _Fork, _Join) ->
  Fun = fun ({K, V}, Acc) ->
            clj_rt:apply(Reduce, [Acc, K, V])
        end,
  Init = clj_rt:apply(Combine, []),
  lists:foldl(Fun, Init, KVs).
