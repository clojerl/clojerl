-module('clojerl.Map').

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

-export([?CONSTRUCTOR/1, to_erl_map/1]).
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

-type type() :: #{ ?TYPE => ?M
                 , keys  => map()
                 , vals  => map()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(KeyValues) when is_list(KeyValues) ->
  KeyValuePairs = build_key_values([], KeyValues),
  {Keys, Vals} = lists:foldl(fun build_mappings/2, {#{}, #{}}, KeyValuePairs),
  #{ ?TYPE => ?M
   , keys  => Keys
   , vals  => Vals
   , meta  => ?NIL
   };
?CONSTRUCTOR(KeyValues) ->
  ?CONSTRUCTOR(clj_rt:to_list(KeyValues)).

-spec to_erl_map(type()) -> map().
to_erl_map(#{?TYPE := ?M, keys := Keys, vals := Vals}) ->
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

%% @private
-spec build_mappings({any(), any()}, mappings()) -> mappings().
build_mappings({K, V}, {Keys, Values}) ->
  KHash = 'clojerl.IHash':hash(K),
  {Keys#{KHash => K}, Values#{KHash => V}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IAssociative

contains_key(#{?TYPE := ?M, keys := Keys}, Key) ->
  maps:is_key('clojerl.IHash':hash(Key), Keys).

entry_at(#{?TYPE := ?M, keys := Keys, vals := Vals}, Key) ->
  Hash = 'clojerl.IHash':hash(Key),
  case maps:is_key(Hash, Keys) of
    true ->
      KeyFound = maps:get(Hash, Keys),
      Val      = maps:get(Hash, Vals),
      clj_rt:vector([KeyFound, Val]);
    false -> ?NIL
  end.

assoc(#{?TYPE := ?M, keys := Keys, vals := Vals} = M, Key, Value) ->
  Hash = 'clojerl.IHash':hash(Key),
  Key1 = case maps:is_key(Hash, Keys) of
           false -> Key;
           true  -> maps:get(Hash, Keys)
         end,
  M#{keys => Keys#{Hash => Key1}, vals => Vals#{Hash => Value}}.

%% clojerl.ICounted

count(#{?TYPE := ?M, keys := Keys}) ->
  maps:size(Keys).

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, keys := KeysX, vals := ValsX}
     , #{?TYPE := ?M, keys := KeysY, vals := ValsY}
     ) ->
  clj_rt:equiv(KeysX, KeysY) andalso clj_rt:equiv(ValsX, ValsY);
equiv(#{?TYPE := ?M, keys := Keys, vals := Vals}, Y) ->
  case clj_rt:'map?'(Y) of
    true  ->
      KeyHashFun   = fun(X) -> {X, 'clojerl.IHash':hash(X)} end,
      KeyHashPairs = lists:map( KeyHashFun
                              , clj_rt:to_list(clj_rt:keys(Y))
                              ),
      Fun = fun({Key, Hash}) ->
                maps:is_key(Hash, Keys) andalso
                  clj_rt:equiv(maps:get(Hash, Vals), clj_rt:get(Y, Key))
            end,
      maps:size(Keys) =:= clj_rt:count(Y)
        andalso lists:all(Fun, KeyHashPairs);
    false -> false
  end.

%% clojerl.IFn

apply(#{?TYPE := ?M} = M, [Key]) ->
  apply(M, [Key, ?NIL]);
apply(#{?TYPE := ?M, vals := Vals}, [Key, NotFound]) ->
  Hash = 'clojerl.IHash':hash(Key),
  maps:get(Hash, Vals, NotFound);
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

%% clojerl.ILookup

get(#{?TYPE := ?M} = Map, Key) ->
  get(Map, Key, ?NIL).

get(#{?TYPE := ?M, vals := Vals}, Key, NotFound) ->
  Hash = 'clojerl.IHash':hash(Key),
  maps:get(Hash, Vals, NotFound).

%% clojerl.IMap

keys(#{?TYPE := ?M, keys := Keys}) ->
  maps:values(Keys).

vals(#{?TYPE := ?M, vals := Vals}) ->
  maps:values(Vals).

without(#{?TYPE := ?M, keys := Keys, vals := Vals} = M, Key) ->
  Hash = 'clojerl.IHash':hash(Key),
  M#{ keys => maps:remove(Hash, Keys)
    , vals => maps:remove(Hash, Vals)
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

to_list(#{?TYPE := ?M, keys := Keys, vals := Vals}) ->
  FoldFun = fun(Hash, K, List) ->
                [clj_rt:vector([K, maps:get(Hash, Vals)]) | List]
            end,
  maps:fold(FoldFun, [], Keys).

%% clojerl.IStringable

str(#{?TYPE := ?M} = Map) ->
  clj_rt:print(Map).
