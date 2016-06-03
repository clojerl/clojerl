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

-export([new/1]).
-export([ 'clojerl.Associative.contains_key'/2
        , 'clojerl.Associative.entry_at'/2
        , 'clojerl.Associative.assoc'/3
        ]).
-export(['clojerl.Counted.count'/1]).
-export([ 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        ]).
-export(['clojerl.IEquiv.equiv'/2]).
-export(['clojerl.IFn.invoke'/2]).
-export(['clojerl.IHash.hash'/1]).
-export([ 'clojerl.ILookup.get'/2
        , 'clojerl.ILookup.get'/3
        ]).
-export([ 'clojerl.IMap.keys'/1
        , 'clojerl.IMap.vals'/1
        , 'clojerl.IMap.without'/2
        ]).
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

-type mappings() :: {map(), map()}.
-type type()     :: #?TYPE{data :: mappings()}.

-spec new(list()) -> type().
new(KeyValues) when is_list(KeyValues) ->
  KeyValuePairs = build_key_values([], KeyValues),
  Mappings = lists:foldl(fun build_mappings/2, {#{}, #{}}, KeyValuePairs),
  #?TYPE{name = ?M, data = Mappings}.

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

'clojerl.Associative.contains_key'(#?TYPE{name = ?M, data = {Keys, _}}, Key) ->
  maps:is_key('clojerl.IHash':hash(Key), Keys).

'clojerl.Associative.entry_at'(#?TYPE{name = ?M, data = {Keys, Vals}}, Key) ->
  Hash = 'clojerl.IHash':hash(Key),
  case maps:is_key(Hash, Keys) of
    true ->
      Key = maps:get(Hash, Keys),
      Val = maps:get(Hash, Vals),
      clj_core:vector([Key, Val]);
    false -> undefined
  end.

'clojerl.Associative.assoc'(#?TYPE{ name = ?M
                                  , data = {Keys, Vals}
                                  } = M
                           , Key
                           , Value) ->
  Hash = 'clojerl.IHash':hash(Key),
  M#?TYPE{data = {Keys#{Hash => Key}, Vals#{Hash => Value}}}.

%% clojerl.Counted

'clojerl.Counted.count'(#?TYPE{name = ?M, data = {Keys, _}}) ->
  maps:size(Keys).

%% clojerl.IEquiv

'clojerl.IEquiv.equiv'( #?TYPE{name = ?M, data = {KeysX, ValsX}}
                      , #?TYPE{name = ?M, data = {KeysY, ValsY}}
                      ) ->
  clj_core:equiv(KeysX, KeysY) andalso clj_core:equiv(ValsX, ValsY);
'clojerl.IEquiv.equiv'(#?TYPE{name = ?M, data = {Keys, Vals}}, Y) ->
  case clj_core:'map?'(Y) of
    true  ->
      KeyHashFun = fun(X) -> {X, erlang:phash2(X)} end,
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

'clojerl.IFn.invoke'(#?TYPE{name = ?M} = M, [Key]) ->
  'clojerl.IFn.invoke'(M, [Key, undefined]);
'clojerl.IFn.invoke'(#?TYPE{name = ?M, data = {_, Vals}}, [Key, NotFound]) ->
  Hash = 'clojerl.IHash':hash(Key),
  maps:get(Hash, Vals, NotFound);
'clojerl.IFn.invoke'(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for map, got: ", CountBin/binary>>).

%% clojerl.IColl

'clojerl.IColl.cons'(#?TYPE{name = ?M, data = {Keys, Vals}} = Map, X) ->
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

'clojerl.IColl.empty'(_) -> new([]).

%% clojerl.IHash

'clojerl.IHash.hash'(#?TYPE{name = ?M, data = Map}) ->
  erlang:phash2(Map).

%% clojerl.ILookup

'clojerl.ILookup.get'(#?TYPE{name = ?M} = Map, Key) ->
  'clojerl.ILookup.get'(Map, Key, undefined).

'clojerl.ILookup.get'(#?TYPE{name = ?M, data = {_, Vals}}, Key, NotFound) ->
  Hash = 'clojerl.IHash':hash(Key),
  maps:get(Hash, Vals, NotFound).

%% clojerl.IMap

'clojerl.IMap.keys'(#?TYPE{name = ?M, data = {Keys, _}}) ->
  maps:values(Keys).

'clojerl.IMap.vals'(#?TYPE{name = ?M, data = {_, Vals}}) ->
  maps:values(Vals).

'clojerl.IMap.without'(#?TYPE{name = ?M, data = {Keys, Vals}} = M, Key) ->
  Hash = 'clojerl.IHash':hash(Key),
  M#?TYPE{data = {maps:remove(Hash, Keys), maps:remove(Hash, Vals)}}.

%% clojerl.IMeta

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?TYPE{name = ?M, info = Info} = Map, Metadata) ->
  Map#?TYPE{info = Info#{meta => Metadata}}.

%% clojerl.Seqable

'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = {Keys, Vals}}) ->
  FoldFun = fun(Hash, K, List) ->
                [clj_core:vector([K, maps:get(Hash, Vals)]) | List]
            end,
  case maps:fold(FoldFun, [], Keys) of
    [] -> undefined;
    X -> X
  end.

%% clojerl.Stringable

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = {Keys, Vals}}) ->
  StrFun = fun(Hash) ->
               KeyStr = clj_core:str(maps:get(Hash, Keys)),
               ValStr = clj_core:str(maps:get(Hash, Vals)),
               clj_utils:binary_join([KeyStr, ValStr], <<" ">>)
           end,
  KeyValueStrs = lists:map(StrFun, maps:keys(Keys)),
  Strs = clj_utils:binary_join(KeyValueStrs, <<", ">>),
  <<"{", Strs/binary, "}">>.
