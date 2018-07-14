-module('clojerl.Map').

-compile({no_auto_import, [{apply, 2}]}).

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behavior('clojerl.IAssociative').
-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IErl').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.ILookup').
-behavior('clojerl.IMap').
-behavior('clojerl.IMeta').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

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
-export(['->erl'/2]).
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

-type type() :: #{ ?TYPE => ?M
                 , keys  => map()
                 , vals  => map()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(KeyValues) when is_list(KeyValues) ->
  KeyValuePairs = build_key_values([], KeyValues),
  Map = lists:foldl(fun build_mappings/2, #{}, KeyValuePairs),
  #{ ?TYPE => ?M
   , map   => Map
   , meta  => ?NIL
   };
?CONSTRUCTOR(KeyValues) ->
  ?CONSTRUCTOR(clj_rt:to_list(KeyValues)).

%% @private
-spec build_key_values(list(), list()) -> [{any(), any()}].
build_key_values(KeyValues, []) ->
  lists:reverse(KeyValues);
build_key_values(KeyValues, [K, V | Items]) ->
  build_key_values([{K, V} | KeyValues], Items).

%% @private
-spec build_mappings({any(), any()}, mappings()) -> mappings().
build_mappings({K, _} = KV, Map) ->
  KHash = clj_rt:hash(K),
  Map#{KHash => KV}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IAssociative

contains_key(#{?TYPE := ?M, map := Map}, Key) ->
  maps:is_key(clj_rt:hash(Key), Map).

entry_at(#{?TYPE := ?M, map := Map}, Key) ->
  Hash = clj_rt:hash(Key),
  case maps:is_key(Hash, Map) of
    true ->
      {KeyFound, Val} = maps:get(Hash, Map),
      clj_rt:vector([KeyFound, Val]);
    false -> ?NIL
  end.

assoc(#{?TYPE := ?M, map := Map} = M, Key, Value) ->
  Hash = clj_rt:hash(Key),
  {Key1, _} = maps:get(Hash, Map, {Key, Value}),
  M#{map => Map#{Hash => {Key1, Value}}}.

%% clojerl.ICounted

count(#{?TYPE := ?M, map := Map}) ->
  maps:size(Map).

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, map := MapX}
     , #{?TYPE := ?M, map := MapY}
     ) ->
  'erlang.Map':equiv(MapX, MapY);
equiv(#{?TYPE := ?M, map := Map}, Y) ->
  case clj_rt:'map?'(Y) of
    true  ->
      TypeModule   = clj_rt:type_module(Y),
      KeyHashFun   = fun(X) -> {X, clj_rt:hash(X)} end,
      KeyHashPairs = lists:map( KeyHashFun
                              , clj_rt:to_list(TypeModule:keys(Y))
                              ),
      Fun = fun({Key, Hash}) ->
                maps:is_key(Hash, Map) andalso
                  clj_rt:equiv( maps:get(Hash, Map)
                              , {Key, TypeModule:get(Y, Key)}
                              )
            end,
      maps:size(Map) =:= TypeModule:count(Y)
        andalso lists:all(Fun, KeyHashPairs);
    false -> false
  end.

%% clojerl.IErl

'->erl'(#{?TYPE := ?M, map := Map}, Recursive) ->
  ErlMapFun = fun
                ({Key0, Val0}, MapAcc) when Recursive ->
                  Key1 = clj_rt:'->erl'(Key0, Recursive),
                  Val1 = clj_rt:'->erl'(Val0, Recursive),
                  MapAcc#{Key1 => Val1};
                ({Key0, Val0}, MapAcc) ->
                  MapAcc#{Key0 => Val0}
              end,
  lists:foldl(ErlMapFun, #{}, maps:values(Map)).

%% clojerl.IFn

apply(#{?TYPE := ?M} = M, [Key]) ->
  apply(M, [Key, ?NIL]);
apply(#{?TYPE := ?M, map := Map}, [Key, NotFound]) ->
  Hash = clj_rt:hash(Key),
  {_, Val} = maps:get(Hash, Map, {Key, NotFound}),
  Val;
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
    Map when IsMap ->
      Fun = fun(KV, Acc) ->
                [K, V] = clj_rt:to_list(KV),
                assoc(Acc, K, V)
            end,
      lists:foldl(Fun, M, Map);
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

%% clojerl.ILookup

get(#{?TYPE := ?M} = Map, Key) ->
  get(Map, Key, ?NIL).

get(#{?TYPE := ?M, map := Map}, Key, NotFound) ->
  Hash = clj_rt:hash(Key),
  {_, Val} = maps:get(Hash, Map, {Key, NotFound}),
  Val.

%% clojerl.IMap

keys(#{?TYPE := ?M, map := Map}) ->
  [K || {K, _} <- maps:values(Map)].

vals(#{?TYPE := ?M, map := Map}) ->
  [V || {_, V} <- maps:values(Map)].

without(#{?TYPE := ?M, map := Map} = M, Key) ->
  Hash = clj_rt:hash(Key),
  M#{map => maps:remove(Hash, Map)}.

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
  FoldFun = fun(_Hash, {K, V}, List) ->
                ['clojerl.Vector':?CONSTRUCTOR([K, V]) | List]
            end,
  maps:fold(FoldFun, [], Map).

%% clojerl.IStringable

str(#{?TYPE := ?M} = M) ->
  clj_rt:print(M).
