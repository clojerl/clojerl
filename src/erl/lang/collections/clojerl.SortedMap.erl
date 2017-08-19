-module('clojerl.SortedMap').

-compile({no_auto_import, [{apply, 2}]}).

-include("clojerl.hrl").

-behavior('clojerl.IAssociative').
-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.ILookup').
-behavior('clojerl.IMap').
-behavior('clojerl.IMeta').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

-export([ ?CONSTRUCTOR/1
        , ?CONSTRUCTOR/2
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

-type type() :: #{ ?TYPE => ?M
                 , keys  => map()
                 , vals  => any()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR([any()]) -> type().
?CONSTRUCTOR(KeyValues) when is_list(KeyValues) ->
  KeyValuePairs = build_key_values([], KeyValues),
  Keys   = lists:foldl(fun fold_key_values/2, #{}, KeyValuePairs),
  Values = rbdict:from_list(KeyValuePairs),
  #{ ?TYPE => ?M
   , keys  => Keys
   , vals  => Values
   , meta  => ?NIL
   }.

-spec ?CONSTRUCTOR(function(), [any()]) -> type().
?CONSTRUCTOR(Compare, KeyValues) when is_list(KeyValues) ->
  KeyValuePairs = build_key_values([], KeyValues),
  Keys   = lists:foldl(fun fold_key_values/2, #{}, KeyValuePairs),
  Values = rbdict:from_list(Compare, KeyValuePairs),
  #{ ?TYPE => ?M
   , keys  => Keys
   , vals  => Values
   , meta  => ?NIL
   }.

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

%% clojerl.IAssociative

contains_key(#{?TYPE := ?M, keys := Keys}, Key) ->
  Hash = 'clojerl.IHash':hash(Key),
  maps:is_key(Hash, Keys).

entry_at(#{?TYPE := ?M, keys := Keys, vals := Vals}, Key) ->
  Hash = 'clojerl.IHash':hash(Key),
  case maps:is_key(Hash, Keys) of
    true ->
      FoundKey = maps:get(Hash, Keys),
      Val = rbdict:fetch(FoundKey, Vals),
      clj_rt:vector([FoundKey, Val]);
    false -> ?NIL
  end.

assoc(#{?TYPE := ?M, keys := Keys, vals := Vals0} = M, Key, Value) ->
  Hash = 'clojerl.IHash':hash(Key),
  Vals = case maps:get(Hash, Keys, ?NIL) of
           ?NIL -> Vals0;
           K -> rbdict:erase(K, Vals0)
         end,
  M#{ keys => Keys#{Hash => Key}
    , vals => rbdict:store(Key, Value, Vals)
    }.

%% clojerl.ICounted

count(#{?TYPE := ?M, keys := Keys}) ->
  maps:size(Keys).

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, keys := KeysX, vals := ValsX}
     , #{?TYPE := ?M, keys := KeysY, vals := ValsY}
     ) ->
  maps:size(KeysX) =:= maps:size(KeysY)
    andalso clj_rt:equiv(rbdict:to_list(ValsX), rbdict:to_list(ValsY));
equiv(#{?TYPE := ?M, keys := Keys, vals := Vals}, Y) ->
  case clj_rt:'map?'(Y) of
    true  ->
      KeyHashFun   = fun(X) -> {X, 'clojerl.IHash':hash(X)} end,
      KeyHashPairs = lists:map( KeyHashFun
                              , clj_rt:to_list(clj_rt:keys(Y))
                              ),
      Fun = fun({Key, Hash}) ->
                maps:is_key(Hash, Keys) andalso
                  clj_rt:equiv(rbdict:fetch(Key, Vals), clj_rt:get(Y, Key))
            end,
      maps:size(Keys) =:= clj_rt:count(Y)
        andalso lists:all(Fun, KeyHashPairs);
    false -> false
  end.

%% clojerl.IFn

apply(#{?TYPE := ?M} = M, [Key]) ->
  apply(M, [Key, ?NIL]);
apply(#{?TYPE := ?M} = M, [Key, NotFound]) ->
  get(M, Key, NotFound);
apply(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  error(<<"Wrong number of args for map, got: ", CountBin/binary>>).

%% clojerl.IColl

cons(#{?TYPE := ?M} = Map, ?NIL) ->
  Map;
cons(#{?TYPE := ?M} = M, X) ->
  IsVector = clj_rt:'vector?'(X),
  IsMap    = clj_rt:'map?'(X),
  case clj_rt:to_list(X) of
    [K, V] when IsVector ->
      assoc(M, K, V);
    KVs when IsMap ->
      Fun = fun(KV, Acc) ->
                assoc(Acc, clj_rt:first(KV), clj_rt:second(KV))
            end,
      lists:foldl(Fun, M, KVs);
    _ ->
      error(<<"Can't conj something that is not a key/value pair or "
              "another map to a map.">>)
  end.

empty(#{?TYPE := ?M, vals := Vals}) ->
  Compare = rbdict:compare_fun(Vals),
  ?CONSTRUCTOR(Compare, []).

%% clojerl.IHash

hash(#{?TYPE := ?M, vals := Vals}) ->
  clj_murmur3:unordered(rbdict:to_list(Vals)).

%% clojerl.ILookup

get(#{?TYPE := ?M} = Map, Key) ->
  get(Map, Key, ?NIL).

get(#{?TYPE := ?M, keys := Keys, vals := Vals}, Key, NotFound) ->
  Hash = 'clojerl.IHash':hash(Key),
  case maps:is_key(Hash, Keys) of
    true  -> rbdict:fetch(maps:get(Hash, Keys), Vals);
    false -> NotFound
  end.

%% clojerl.IMap

keys(#{?TYPE := ?M, keys := Keys}) ->
  maps:values(Keys).

vals(#{?TYPE := ?M, vals := Vals}) ->
  [Val || {_, Val} <- rbdict:to_list(Vals)].

without(#{?TYPE := ?M, keys := Keys, vals := Vals0} = M, Key) ->
  Hash = 'clojerl.IHash':hash(Key),
  Vals = case maps:is_key(Hash, Keys) of
           true  -> rbdict:erase(maps:get(Hash, Keys), Vals0);
           false -> Vals0
         end,
  M#{ keys => maps:remove(Hash, Keys)
    , vals => Vals
    }.

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

to_list(#{?TYPE := ?M, vals := Vals}) ->
  VectorFun = fun({Key, Val}) ->
                  clj_rt:vector([Key, Val])
              end,
  lists:map(VectorFun, rbdict:to_list(Vals)).

%% clojerl.IStringable

str(#{?TYPE := ?M} = SortedMap) ->
  clj_rt:print(SortedMap).
