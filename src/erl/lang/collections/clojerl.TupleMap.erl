-module('clojerl.TupleMap').

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
        , create_with_assoc/1
        , create_with_check/1
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

-type type() :: #{ ?TYPE => ?M
                 , tuple => tuple()
                 , meta  => ?NIL | any()
                 }.

-define(HASHTABLE_THRESHOLD, 16).

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(KeyValues) when is_list(KeyValues) ->
  TupleMap = list_to_tuple(KeyValues),
  #{ ?TYPE => ?M
   , tuple => TupleMap
   , meta  => ?NIL
   }.

-spec create_with_check(list()) -> type().
create_with_check(KeyValues) ->
  lists:foldl(fun check_duplicate_keys/2, {key, []}, KeyValues),
  ?CONSTRUCTOR(KeyValues).

-spec check_duplicate_keys(any(), {key | value, list()}) ->
  term().
check_duplicate_keys(K, {key, Keys}) ->
  lists:member(K, Keys)
    andalso ?ERROR([<<"Duplicate key: ">>, K]),
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

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IAssociative

contains_key(#{?TYPE := ?M, tuple := TupleMap}, Key) ->
  index_of(TupleMap, Key) =/= notfound.

entry_at(#{?TYPE := ?M, tuple := TupleMap}, Key) ->
  case index_of(TupleMap, Key) of
    notfound ->
      ?NIL;
    Index ->
      KeyFound = erlang:element(Index, TupleMap),
      Val      = erlang:element(Index + 1, TupleMap),
      clj_rt:vector([KeyFound, Val])
  end.

assoc(#{?TYPE := ?M, tuple := TupleMap0} = M, Key, Value) ->
  case index_of(TupleMap0, Key) of
    notfound ->
      Count   = trunc(tuple_size(TupleMap0) / 2),
      ListMap = tuple_to_list(TupleMap0),
      case Count + 1 > ?HASHTABLE_THRESHOLD of
        true ->
          clj_rt:hash_map([Key, Value | ListMap]);
        false ->
          TupleMap1 = list_to_tuple([Key, Value | ListMap]),
          M#{tuple => TupleMap1}
        end;
    Index ->
      TupleMap1 = erlang:setelement(Index + 1, TupleMap0, Value),
      M#{tuple => TupleMap1}
  end.

%% clojerl.ICounted

count(#{?TYPE := ?M, tuple := TupleMap}) ->
  trunc(tuple_size(TupleMap) / 2).

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, tuple := TupleMapX}
     , #{?TYPE := ?M, tuple := TupleMapY}
     ) ->
  Fun = fun
          Fun([KeyX, ValX | Rest]) ->
            case index_of(TupleMapY, KeyX) of
              notfound -> false;
              Index ->
                ValY = element(Index + 1, TupleMapY),
                clj_rt:equiv(ValX, ValY) andalso Fun(Rest)
            end;
          Fun([]) -> true
        end,
  tuple_size(TupleMapX) =:= tuple_size(TupleMapY)
    andalso Fun(tuple_to_list(TupleMapX));
equiv(#{?TYPE := ?M, tuple := TupleMap}, Y) ->
  case clj_rt:'map?'(Y) of
    true  ->
      TypeModule = clj_rt:type_module(Y),
      Fun = fun
              Fun([Key, Val | Rest]) ->
                clj_rt:equiv(Val, TypeModule:get(Y, Key)) andalso Fun(Rest);
              Fun([]) -> true
            end,
      trunc(tuple_size(TupleMap) / 2) =:= TypeModule:count(Y)
        andalso Fun(tuple_to_list(TupleMap));
    false -> false
  end.

%% clojerl.IEncodeErlang

-spec 'clj->erl'(type(), boolean()) -> map().
'clj->erl'(#{?TYPE := ?M, tuple := TupleMap}, Recursive) ->
  ErlMapFun = fun
                (Key, {none, MapAcc}) ->
                  {{Key}, MapAcc};
                (Val0, {{Key0}, MapAcc}) when Recursive ->
                  Key1 = clj_rt:'clj->erl'(Key0, Recursive),
                  Val1 = clj_rt:'clj->erl'(Val0, Recursive),
                  {none, MapAcc#{Key1 => Val1}};
                (Val0, {{Key0}, MapAcc}) ->
                  {none, MapAcc#{Key0 => Val0}}
              end,
  {_, Map} = lists:foldl(ErlMapFun, {none, #{}}, tuple_to_list(TupleMap)),
  Map.

%% clojerl.IFn

apply(#{?TYPE := ?M} = M, [Key]) ->
  apply(M, [Key, ?NIL]);
apply(#{?TYPE := ?M, tuple := TupleMap}, [Key, NotFound]) ->
  case index_of(TupleMap, Key) of
    notfound -> NotFound;
     Index   -> erlang:element(Index + 1, TupleMap)
  end;
apply(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for map, got: ", CountBin/binary>>).

%% clojerl.IColl

cons(#{?TYPE := ?M} = Map, ?NIL) ->
  Map;
cons(#{?TYPE := ?M} = Map, X) ->
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

hash(#{?TYPE := ?M} = Map) ->
  clj_murmur3:unordered(Map).

%% clojerl.IKVReduce

'kv-reduce'(#{?TYPE := ?M, tuple := Tuple}, Fun, Init) ->
  Ref = make_ref(),
  Size = size(Tuple),
  Fold = fun
           Fold(N, Acc) when N > Size ->
             Acc;
           Fold(N, Acc0) ->
             K = erlang:element(N, Tuple),
             V = erlang:element(N + 1, Tuple),
             Acc = 'clojerl.IFn':apply(Fun, [Acc0, K, V]),
             case 'clojerl.Reduced':is_reduced(Acc) of
               true  -> throw({reduced, Ref, Acc});
               false -> Fold(N + 2, Acc)
             end
         end,
  try   Fold(1, Init)
  catch throw:{reduced, Ref, Acc} ->
      'clojerl.Reduced':deref(Acc)
  end.

%% clojerl.ILookup

get(#{?TYPE := ?M} = Map, Key) ->
  get(Map, Key, ?NIL).

get(#{?TYPE := ?M, tuple := TupleMap}, Key, NotFound) ->
  case index_of(TupleMap, Key) of
    notfound -> NotFound;
    Index    -> erlang:element(Index + 1, TupleMap)
  end.

%% clojerl.IMap

keys(#{?TYPE := ?M, tuple := {}}) ->
  ?NIL;
keys(#{?TYPE := ?M, tuple := TupleMap}) ->
  Fun = fun
          Fun([], Acc) ->
            lists:reverse(Acc);
          Fun([K, _ | Rest], Acc) ->
            Fun(Rest, [K | Acc])
        end,
  Fun(tuple_to_list(TupleMap), []).

vals(#{?TYPE := ?M, tuple := {}}) ->
  ?NIL;
vals(#{?TYPE := ?M, tuple := TupleMap}) ->
  Fun = fun
          Fun([], Acc) ->
            lists:reverse(Acc);
          Fun([_, V | Rest], Acc) ->
            Fun(Rest, [V | Acc])
        end,
  Fun(tuple_to_list(TupleMap), []).

without(#{?TYPE := ?M, tuple := TupleMap0} = M, Key) ->
  TupleMap1 = do_without(tuple_to_list(TupleMap0), [], Key),
  M#{tuple => TupleMap1}.

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

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = Map, Metadata) ->
  Map#{meta => Metadata}.

%% clojerl.ISeqable

seq(#{?TYPE := ?M} = Map) ->
  case to_list(Map) of
    [] -> ?NIL;
    X -> X
  end.

to_list(#{?TYPE := ?M, tuple := TupleMap}) ->
  do_to_list(tuple_to_list(TupleMap), []).

do_to_list([], Acc) ->
  lists:reverse(Acc);
do_to_list([K, V | Rest], Acc0) ->
  Acc1 = [clj_rt:vector([K, V]) | Acc0],
  do_to_list(Rest, Acc1).

%% clojerl.IStringable

str(#{?TYPE := ?M} = TupleMap) ->
  clj_rt:print_str(TupleMap).

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
