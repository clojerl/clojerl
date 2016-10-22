-module('clojerl.SortedMap').

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

-export([?CONSTRUCTOR/1]).
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

-type type()     :: #?TYPE{data :: orddict:orddict()}.

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(KeyValues) when is_list(KeyValues) ->
  KeyValuePairs = build_key_values([], KeyValues),
  Keys   = lists:foldl(fun fold_key_values/2, #{}, KeyValuePairs),
  Values = rbdict:from_list(KeyValuePairs),
  #?TYPE{name = ?M, data = {Keys, Values}}.

%% @private
-spec build_key_values(list(), list()) -> [{any(), any()}].
build_key_values(KeyValues, []) ->
  lists:reverse(KeyValues);
build_key_values(KeyValues, [K, V | Items]) ->
  build_key_values([{K, V} | KeyValues], Items).

%% @private
-spec fold_key_values({any(), any()}, map()) -> map().
fold_key_values({K, _}, Map) ->
  maps:put('clojerl.IHash':hash(K), K, Map).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.Associative

contains_key(#?TYPE{name = ?M, data = {Keys, _}}, Key) ->
  Hash = 'clojerl.IHash':hash(Key),
  maps:is_key(Hash, Keys).

entry_at(#?TYPE{name = ?M, data = {Keys, Vals}}, Key) ->
  Hash = 'clojerl.IHash':hash(Key),
  case maps:is_key(Hash, Keys) of
    true ->
      FoundKey = maps:get(Hash, Keys),
      Val = rbdict:fetch(FoundKey, Vals),
      clj_core:vector([FoundKey, Val]);
    false -> undefined
  end.

assoc(#?TYPE{name = ?M, data = {Keys, Vals0}} = M, Key, Value) ->
  Hash = 'clojerl.IHash':hash(Key),
  Vals = case maps:get(Hash, Keys, undefined) of
           undefined -> Vals0;
           K -> rbdict:erase(K, Vals0)
         end,
  M#?TYPE{data = {Keys#{Hash => Key}, rbdict:store(Key, Value, Vals)}}.

%% clojerl.Counted

count(#?TYPE{name = ?M, data = {Keys, _}}) ->
  maps:size(Keys).

%% clojerl.IEquiv

equiv( #?TYPE{name = ?M, data = {KeysX, ValsX}}
     , #?TYPE{name = ?M, data = {KeysY, ValsY}}
     ) ->
  maps:size(KeysX) =:= maps:size(KeysY)
    andalso clj_core:equiv(rbdict:to_list(ValsX), rbdict:to_list(ValsY));
equiv(#?TYPE{name = ?M, data = {Keys, Vals}}, Y) ->
  case clj_core:'map?'(Y) of
    true  ->
      KeyHashFun   = fun(X) -> {X, 'clojerl.IHash':hash(X)} end,
      KeyHashPairs = lists:map( KeyHashFun
                              , clj_core:to_list(clj_core:keys(Y))
                              ),
      Fun = fun({Key, Hash}) ->
                maps:is_key(Hash, Keys) andalso
                  clj_core:equiv(rbdict:fetch(Key, Vals), clj_core:get(Y, Key))
            end,
      maps:size(Keys) =:= clj_core:count(Y)
        andalso lists:all(Fun, KeyHashPairs);
    false -> false
  end.

%% clojerl.IFn

apply(#?TYPE{name = ?M} = M, [Key]) ->
  apply(M, [Key, undefined]);
apply(#?TYPE{name = ?M} = M, [Key, NotFound]) ->
  get(M, Key, NotFound);
apply(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  error(<<"Wrong number of args for map, got: ", CountBin/binary>>).

%% clojerl.IColl

cons(#?TYPE{name = ?M} = Map, undefined) ->
  Map;
cons(#?TYPE{name = ?M} = M, X) ->
  IsVector = clj_core:'vector?'(X),
  IsMap    = clj_core:'map?'(X),
  case clj_core:to_list(X) of
    [K, V] when IsVector ->
      assoc(M, K, V);
    KVs when IsMap ->
      Fun = fun(KV, Acc) ->
                assoc(Acc, clj_core:first(KV), clj_core:second(KV))
            end,
      lists:foldl(Fun, M, KVs);
    _ ->
      error(<<"Can't conj something that is not a key/value pair or "
              "another map to a map.">>)
  end.

empty(_) -> ?CONSTRUCTOR([]).

%% clojerl.IHash

hash(#?TYPE{name = ?M, data = {_, Vals}}) ->
  clj_murmur3:unordered(rbdict:to_list(Vals)).

%% clojerl.ILookup

get(#?TYPE{name = ?M} = Map, Key) ->
  get(Map, Key, undefined).

get(#?TYPE{name = ?M, data = {Keys, Vals}}, Key, NotFound) ->
  Hash = 'clojerl.IHash':hash(Key),
  case maps:is_key(Hash, Keys) of
    true  -> rbdict:fetch(maps:get(Hash, Keys), Vals);
    false -> NotFound
  end.

%% clojerl.IMap

keys(#?TYPE{name = ?M, data = {Keys, _}}) ->
  maps:values(Keys).

vals(#?TYPE{name = ?M, data = {_, Vals}}) ->
  [Val || {_, Val} <- rbdict:to_list(Vals)].

without(#?TYPE{name = ?M, data = {Keys, Vals0}} = M, Key) ->
  Hash = 'clojerl.IHash':hash(Key),
  Vals = case maps:is_key(Hash, Keys) of
           true  -> rbdict:erase(maps:get(Hash, Keys), Vals0);
           false -> Vals0
         end,
  M#?TYPE{data = {maps:remove(Hash, Keys), Vals}}.

%% clojerl.IMeta

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

with_meta(#?TYPE{name = ?M, info = Info} = Map, Metadata) ->
  Map#?TYPE{info = Info#{meta => Metadata}}.

%% clojerl.Seqable

seq(#?TYPE{name = ?M} = Map) ->
  case to_list(Map) of
    [] -> undefined;
    X -> X
  end.

to_list(#?TYPE{name = ?M, data = {_, Vals}}) ->
  VectorFun = fun({Key, Val}) ->
                  clj_core:vector([Key, Val])
              end,
  lists:map(VectorFun, rbdict:to_list(Vals)).

%% clojerl.Stringable

str(#?TYPE{name = ?M, data = {_, Vals}}) ->
  StrFun = fun({Key, Value}) ->
               KeyStr = clj_core:str(Key),
               ValStr = clj_core:str(Value),
               'clojerl.String':join([KeyStr, ValStr], <<" ">>)
           end,
  KeyValueStrs = lists:map(StrFun, rbdict:to_list(Vals)),
  Strs = 'clojerl.String':join(KeyValueStrs, <<", ">>),
  <<"{", Strs/binary, "}">>.
