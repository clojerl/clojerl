-module('clojerl.TupleMap').

-compile({no_auto_import, [{apply, 2}]}).

-include("clojerl.hrl").

-behavior('clojerl.Associative').
-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.ILookup').
-behavior('clojerl.IMap').
-behavior('clojerl.IMeta').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([ ?CONSTRUCTOR/1
        , create_with_assoc/1
        , create_with_check/1
        , to_erl_map/1
        ]).

-export([ contains_key/2
        , entry_at/2
        , assoc/3
        ]).
-export([count/1]).
-export([ cons/2
        , empty/1
        ]).
-export([equiv/2]).
-export([apply/2]).
-export([hash/1]).
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

-type mappings() :: {map(), map()}.
-type type()     :: #?TYPE{data :: mappings()}.

-define(HASHTABLE_THRESHOLD, 16).

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(KeyValues) when is_list(KeyValues) ->
  TupleMap = list_to_tuple(KeyValues),
  #?TYPE{name = ?M, data = TupleMap}.

-spec create_with_check(list()) -> type().
create_with_check(KeyValues) ->
  lists:foldl(fun check_duplicate_keys/2, {key, []}, KeyValues),
  ?CONSTRUCTOR(KeyValues).

-spec check_duplicate_keys(any(), {key | value, list()}) ->
  term().
check_duplicate_keys(K, {key, Keys}) ->
  lists:member(K, Keys)
    andalso clj_utils:error([<<"Duplicate key: ">>, K]),
  {value, [K | Keys]};
check_duplicate_keys(_, {value, Keys}) ->
  {key, Keys}.

-spec create_with_assoc(list()) -> type().
create_with_assoc(KeyValues0) ->
  {_, KeyValues1} = lists:foldl(fun create_with_assoc/2, {key, []}, KeyValues0),
  KeyValues2 = lists:foldl(fun build_from_proplist/2, [], KeyValues1),
  ?CONSTRUCTOR(KeyValues2).

-spec create_with_assoc(any(), {key | value, list()}) -> {key | value, list()}.
create_with_assoc(Key, {key, KeyValues}) ->
  {value, [Key | KeyValues]};
create_with_assoc(Value, {value, [Key | KeyValues0]}) ->
  KeyValues1 = try replace_key_value(Key, Value, KeyValues0)
               catch throw:notfound -> [{Key, Value} | KeyValues0]
               end,
  {key, KeyValues1}.

-spec replace_key_value(any(), any(), [{any(), any()}]) ->
  [{any(), any()}].
replace_key_value(_Key, _Value, []) ->
  throw(notfound);
replace_key_value(Key, Value, [{K, V} | KeyValues]) ->
  case clj_rt:equiv(K, Key) of
    true ->
      [{K, Value} | KeyValues];
    false ->
      [{K, V} | replace_key_value(Key, Value, KeyValues)]
  end.

-spec build_from_proplist(any(), list()) -> list().
build_from_proplist({Key, Value}, Acc) ->
  [Key, Value | Acc].

-spec to_erl_map(type()) -> map().
to_erl_map(#?TYPE{name = ?M, data = TupleMap}) ->
  ErlMapFun = fun
                (Key, {none, MapAcc}) ->
                  {{Key}, MapAcc};
                (Value, {{Key}, MapAcc}) ->
                  {none, MapAcc#{Key => Value}}
              end,
  {_, Map} = lists:foldl(ErlMapFun, {none, #{}}, tuple_to_list(TupleMap)),
  Map.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.Associative

contains_key(#?TYPE{name = ?M, data = TupleMap}, Key) ->
  index_of(TupleMap, Key) =/= notfound.

entry_at(#?TYPE{name = ?M, data = TupleMap}, Key) ->
  case index_of(TupleMap, Key) of
    notfound ->
      ?NIL;
    Index ->
      KeyFound = erlang:element(Index, TupleMap),
      Val      = erlang:element(Index + 1, TupleMap),
      clj_rt:vector([KeyFound, Val])
  end.

assoc(#?TYPE{name = ?M, data = TupleMap0} = M, Key, Value) ->
  case index_of(TupleMap0, Key) of
    notfound ->
      Count   = trunc(tuple_size(TupleMap0) / 2),
      ListMap = tuple_to_list(TupleMap0),
      case Count + 1 > ?HASHTABLE_THRESHOLD of
        true ->
          clj_rt:hash_map([Key, Value | ListMap]);
        false ->
          TupleMap1 = list_to_tuple([Key, Value | ListMap]),
          M#?TYPE{data = TupleMap1}
        end;
    Index ->
      TupleMap1 = erlang:setelement(Index + 1, TupleMap0, Value),
      M#?TYPE{data = TupleMap1}
  end.

%% clojerl.Counted

count(#?TYPE{name = ?M, data = TupleMap}) ->
  trunc(tuple_size(TupleMap) / 2).

%% clojerl.IEquiv

equiv( #?TYPE{name = ?M, data = TupleMapX}
     , #?TYPE{name = ?M, data = TupleMapY}
     ) ->
  Fun = fun
          Fun([KeyX, ValX | Rest]) ->
            case index_of(TupleMapY, KeyX) of
              notfound -> false;
              Index ->
                ValY = element(Index + 1, TupleMapY),
                case clj_rt:equiv(ValX, ValY) of
                  false -> false;
                  true  -> Fun(Rest)
                end
            end;
          Fun([]) -> true
        end,
  tuple_size(TupleMapX) =:= tuple_size(TupleMapY)
    andalso Fun(tuple_to_list(TupleMapX));
equiv(#?TYPE{name = ?M, data = TupleMap}, Y) ->
  case clj_rt:'map?'(Y) of
    true  ->
      Fun = fun
              Fun([Key, Val | Rest]) ->
                case clj_rt:equiv(Val, clj_rt:get(Y, Key)) of
                  false -> false;
                  true  -> Fun(Rest)
                end;
              Fun([]) -> true
            end,
      trunc(tuple_size(TupleMap) / 2) =:= clj_rt:count(Y)
        andalso Fun(tuple_to_list(TupleMap));
    false -> false
  end.

%% clojerl.IFn

apply(#?TYPE{name = ?M} = M, [Key]) ->
  apply(M, [Key, ?NIL]);
apply(#?TYPE{name = ?M, data = TupleMap}, [Key, NotFound]) ->
  case index_of(TupleMap, Key) of
    notfound -> NotFound;
     Index   -> erlang:element(Index + 1, TupleMap)
  end;
apply(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for map, got: ", CountBin/binary>>).

%% clojerl.IColl

cons(#?TYPE{name = ?M} = Map, ?NIL) ->
  Map;
cons(#?TYPE{name = ?M} = Map, X) ->
  IsVector = clj_rt:'vector?'(X),
  IsMap    = clj_rt:'map?'(X),
  case clj_rt:to_list(X) of
    [K, V] when IsVector ->
      assoc(Map, K, V);
    KVs when IsMap ->
      Fun = fun(KV, Acc) ->
                assoc(Acc, clj_rt:first(KV), clj_rt:second(KV))
            end,
      lists:foldl(Fun, Map, KVs);
    _ ->
      throw(<<"Can't conj something that is not a key/value pair or "
              "another map to a map.">>)
  end.

empty(_) -> ?CONSTRUCTOR([]).

%% clojerl.IHash

hash(#?TYPE{name = ?M} = Map) ->
  clj_murmur3:unordered(Map).

%% clojerl.ILookup

get(#?TYPE{name = ?M} = Map, Key) ->
  get(Map, Key, ?NIL).

get(#?TYPE{name = ?M, data = TupleMap}, Key, NotFound) ->
  case index_of(TupleMap, Key) of
    notfound -> NotFound;
    Index    -> erlang:element(Index + 1, TupleMap)
  end.

%% clojerl.IMap

keys(#?TYPE{name = ?M, data = TupleMap}) ->
  Fun = fun
          Fun([], Acc) ->
            lists:reverse(Acc);
          Fun([K, _ | Rest], Acc) ->
            Fun(Rest, [K | Acc])
        end,
  Fun(tuple_to_list(TupleMap), []).

vals(#?TYPE{name = ?M, data = TupleMap}) ->
  Fun = fun
          Fun([], Acc) ->
            lists:reverse(Acc);
          Fun([_, V | Rest], Acc) ->
            Fun(Rest, [V | Acc])
        end,
  Fun(tuple_to_list(TupleMap), []).

without(#?TYPE{name = ?M, data = TupleMap0} = M, Key) ->
  TupleMap1 = do_without(tuple_to_list(TupleMap0), [], Key),
  M#?TYPE{data = TupleMap1}.

-spec do_without(list(), list(), any()) ->
  tuple().
do_without([], Acc, _Key) ->
  list_to_tuple(lists:reverse(Acc));
do_without([K, V | Rest], Acc, Key) ->
  case clj_rt:equiv(K, Key) of
    true -> do_without(Rest, Acc, Key);
    false -> do_without(Rest, [V, K | Acc], Key)
  end.

%% clojerl.IMeta

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, ?NIL).

with_meta(#?TYPE{name = ?M, info = Info} = Map, Metadata) ->
  Map#?TYPE{info = Info#{meta => Metadata}}.

%% clojerl.Seqable

seq(#?TYPE{name = ?M} = Map) ->
  case to_list(Map) of
    [] -> ?NIL;
    X -> X
  end.

to_list(#?TYPE{name = ?M, data = TupleMap}) ->
  do_to_list(tuple_to_list(TupleMap), []).

do_to_list([], Acc) ->
  lists:reverse(Acc);
do_to_list([K, V | Rest], Acc0) ->
  Acc1 = [clj_rt:vector([K, V]) | Acc0],
  do_to_list(Rest, Acc1).

%% clojerl.Stringable

str(#?TYPE{name = ?M, data = TupleMap}) ->
  do_str(tuple_to_list(TupleMap), []).

do_str([], KeyValueStrs0) ->
  KeyValueStrs1 = lists:reverse(KeyValueStrs0),
  Strs = 'clojerl.String':join(KeyValueStrs1, <<", ">>),
  <<"{", Strs/binary, "}">>;
do_str([K, V | Rest], KeyValueStrs0) ->
  KeyStr = clj_rt:str(K),
  ValStr = clj_rt:str(V),
  KeyValueStr = 'clojerl.String':join([KeyStr, ValStr], <<" ">>),
  do_str(Rest, [KeyValueStr | KeyValueStrs0]).

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec index_of(tuple(), any()) -> pos_integer() | notfound.
index_of(TupleMap, Key) ->
  Fun = fun(Tuple, Index, Acc) ->
            K = element(Index, Tuple),
            case clj_rt:equiv(K, Key) of
              true  -> {return, Index};
              false -> {acc, Acc}
            end
        end,
  fold(Fun, notfound, TupleMap, 2).

fold(Fun, Acc, Tuple, Inc) ->
  Length = tuple_size(Tuple),
  do_fold(Fun, {acc, Acc}, Tuple, 1, Length, Inc).

-type accumulator() :: {return, any()} | {acc, any()}.

-spec do_fold(fun(), accumulator(), tuple(), integer(), integer(), integer()) ->
  any().
do_fold(_Fun, {return, Value}, _Tuple, _Index, _Length, _Inc) ->
  Value;
do_fold(_Fun, {_, Value}, _Tuple, Index, Length, _Inc) when Index > Length->
  Value;
do_fold(Fun, {acc, Value}, Tuple, Index, Length, Inc) ->
  Acc = Fun(Tuple, Index, Value),
  do_fold(Fun, Acc, Tuple, Index + Inc, Length, Inc).
