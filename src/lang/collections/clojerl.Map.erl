-module('clojerl.Map').

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

-export([new/1, to_erl_map/1]).
-export([ contains_key/2
        , entry_at/2
        , assoc/3
        ]).
-export([count/1]).
-export([ cons/2
        , empty/1
        ]).
-export([equiv/2]).
-export([invoke/2]).
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
-export([seq/1]).
-export([str/1]).

-type mappings() :: {map(), map()}.
-type type()     :: #?TYPE{data :: mappings()}.

-spec new(list()) -> type().
new(KeyValues) when is_list(KeyValues) ->
  KeyValuePairs = build_key_values([], KeyValues),
  Mappings = lists:foldl(fun build_mappings/2, {#{}, #{}}, KeyValuePairs),
  #?TYPE{name = ?M, data = Mappings}.

-spec to_erl_map(type()) -> map().
to_erl_map(#?TYPE{name = ?M, data = {Keys, Vals}}) ->
  ErlMapFun = fun({Hash, Key}, MapAcc) ->
                  MapAcc#{Key => maps:get(Hash, Vals)}
              end,
  lists:foldl(ErlMapFun, #{}, maps:to_list(Keys)).

%% @private
-spec build_key_values(list(), list()) -> [{any(), any()}].
build_key_values(KeyValues, []) ->
  lists:reverse(KeyValues);
build_key_values(KeyValues, [K, V | Items]) ->
  build_key_values([{K, V} | KeyValues], Items).

%% @prvivate
-spec build_mappings({any(), any()}, mappings()) -> mappings().
build_mappings({K, V}, {Keys, Values}) ->
  KHash = 'clojerl.IHash':hash(K),
  {Keys#{KHash => K}, Values#{KHash => V}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.Associative

contains_key(#?TYPE{name = ?M, data = {Keys, _}}, Key) ->
  maps:is_key('clojerl.IHash':hash(Key), Keys).

entry_at(#?TYPE{name = ?M, data = {Keys, Vals}}, Key) ->
  Hash = 'clojerl.IHash':hash(Key),
  case maps:is_key(Hash, Keys) of
    true ->
      KeyFound = maps:get(Hash, Keys),
      Val      = maps:get(Hash, Vals),
      clj_core:vector([KeyFound, Val]);
    false -> undefined
  end.

assoc(#?TYPE{ name = ?M
                                  , data = {Keys, Vals}
                                  } = M
                           , Key
                           , Value) ->
  Hash = 'clojerl.IHash':hash(Key),
  M#?TYPE{data = {Keys#{Hash => Key}, Vals#{Hash => Value}}}.

%% clojerl.Counted

count(#?TYPE{name = ?M, data = {Keys, _}}) ->
  maps:size(Keys).

%% clojerl.IEquiv

equiv( #?TYPE{name = ?M, data = {KeysX, ValsX}}
                      , #?TYPE{name = ?M, data = {KeysY, ValsY}}
                      ) ->
  clj_core:equiv(KeysX, KeysY) andalso clj_core:equiv(ValsX, ValsY);
equiv(#?TYPE{name = ?M, data = {Keys, Vals}}, Y) ->
  case clj_core:'map?'(Y) of
    true  ->
      KeyHashFun = fun(X) -> {X, 'clojerl.IHash':hash(X)} end,
      KeyHashPairs = lists:map(KeyHashFun, clj_core:keys(Y)),
      Fun = fun({Key, Hash}) ->
                maps:is_key(Hash, Keys) andalso
                  clj_core:equiv(maps:get(Hash, Vals), clj_core:get(Y, Key))
            end,
      maps:size(Keys) == clj_core:count(Y)
        andalso lists:all(Fun, KeyHashPairs);
    false -> false
  end.

%% clojerl.IFn

invoke(#?TYPE{name = ?M} = M, [Key]) ->
  invoke(M, [Key, undefined]);
invoke(#?TYPE{name = ?M, data = {_, Vals}}, [Key, NotFound]) ->
  Hash = 'clojerl.IHash':hash(Key),
  maps:get(Hash, Vals, NotFound);
invoke(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for map, got: ", CountBin/binary>>).

%% clojerl.IColl

cons(#?TYPE{name = ?M} = Map, undefined) ->
  Map;
cons(#?TYPE{name = ?M, data = {Keys, Vals}} = Map, X) ->
  IsVector = clj_core:'vector?'(X),
  IsMap    = clj_core:'map?'(X),
  case clj_core:seq_to_list(X) of
    [K, V] when IsVector ->
      Hash = 'clojerl.IHash':hash(K),
      Map#?TYPE{data = {Keys#{Hash => K}, Vals#{Hash => V}}};
    KVs when IsMap ->
      Fun = fun(KV, Acc) ->
                clj_core:assoc(Acc, clj_core:first(KV), clj_core:second(KV))
            end,
      lists:foldl(Fun, Map, KVs);
    _ ->
      throw(<<"Can't conj something that is not a key/value pair or "
              "another map to a map.">>)
  end.

empty(_) -> new([]).

%% clojerl.IHash

hash(#?TYPE{name = ?M} = Map) ->
  clj_murmur3:unordered(Map).

%% clojerl.ILookup

get(#?TYPE{name = ?M} = Map, Key) ->
  get(Map, Key, undefined).

get(#?TYPE{name = ?M, data = {_, Vals}}, Key, NotFound) ->
  Hash = 'clojerl.IHash':hash(Key),
  maps:get(Hash, Vals, NotFound).

%% clojerl.IMap

keys(#?TYPE{name = ?M, data = {Keys, _}}) ->
  maps:values(Keys).

vals(#?TYPE{name = ?M, data = {_, Vals}}) ->
  maps:values(Vals).

without(#?TYPE{name = ?M, data = {Keys, Vals}} = M, Key) ->
  Hash = 'clojerl.IHash':hash(Key),
  M#?TYPE{data = {maps:remove(Hash, Keys), maps:remove(Hash, Vals)}}.

%% clojerl.IMeta

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

with_meta(#?TYPE{name = ?M, info = Info} = Map, Metadata) ->
  Map#?TYPE{info = Info#{meta => Metadata}}.

%% clojerl.Seqable

seq(#?TYPE{name = ?M, data = {Keys, Vals}}) ->
  FoldFun = fun(Hash, K, List) ->
                [clj_core:vector([K, maps:get(Hash, Vals)]) | List]
            end,
  case maps:fold(FoldFun, [], Keys) of
    [] -> undefined;
    X -> X
  end.

%% clojerl.Stringable

str(#?TYPE{name = ?M, data = {Keys, Vals}}) ->
  StrFun = fun(Hash) ->
               KeyStr = clj_core:str(maps:get(Hash, Keys)),
               ValStr = clj_core:str(maps:get(Hash, Vals)),
               'clojerl.String':join([KeyStr, ValStr], <<" ">>)
           end,
  KeyValueStrs = lists:map(StrFun, maps:keys(Keys)),
  Strs = 'clojerl.String':join(KeyValueStrs, <<", ">>),
  <<"{", Strs/binary, "}">>.
