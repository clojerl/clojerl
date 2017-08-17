-module('erlang.Map').

-include("clojerl.hrl").

-behavior('clojerl.Associative').
-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.ILookup').
-behavior('clojerl.IMap').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([ contains_key/2
        , entry_at/2
        , assoc/3
        ]).
-export([count/1]).
-export([ cons/2
        , empty/1
        ]).
-export([ equiv/2]).
-export([ apply/2]).
-export([ hash/1]).
-export([ get/2
        , get/3
        ]).
-export([ keys/1
        , vals/1
        , without/2
        ]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

contains_key(Map, Key) ->
  maps:is_key(Key, Map).

entry_at(Map, Key) ->
  case maps:is_key(Key, Map) of
    true ->
      Val = maps:get(Key, Map),
      clj_rt:vector([Key, Val]);
    false -> ?NIL
  end.

assoc(Map, Key, Value) ->
  Map#{Key => Value}.

count(Map) -> maps:size(Map).

cons(Map, X) ->
  IsVector = clj_rt:'vector?'(X),
  IsMap    = clj_rt:'map?'(X),
  case clj_rt:to_list(X) of
    [K, V] when IsVector ->
      Map#{K => V};
    KVs when IsMap ->
      Fun = fun(KV, Acc) ->
                assoc(Acc, clj_rt:first(KV), clj_rt:second(KV))
            end,
      lists:foldl(Fun, Map, KVs);
    _ ->
      throw(<<"Can't conj something that is not a key/value pair to a map.">>)
  end.

%% clojerl.IColl

empty(_) -> #{}.

%% clojerl.IEquiv

equiv(X, Y) when is_map(X), is_map(Y) ->
  case maps:size(X) =:= maps:size(Y) of
    false -> false;
    true  ->
      X1 = maps:fold(fun remove_meta/3, #{}, X),
      Y1 = maps:fold(fun remove_meta/3, #{}, Y),

      FunEquiv = fun(K) ->
                     maps:is_key(K, Y1)
                       andalso clj_rt:equiv( maps:get(K, X1)
                                             , maps:get(K, Y1))
                 end,

      lists:all(FunEquiv, maps:keys(X1))
  end;
equiv(X, Y) when is_map(X) ->
  case clj_rt:'map?'(Y) of
    true  ->
      Keys = clj_rt:keys(Y),
      Fun = fun(Key) ->
                maps:is_key(Key, X) andalso
                  clj_rt:equiv(maps:get(Key, X), clj_rt:get(Y, Key))
            end,
      maps:size(X) == clj_rt:count(Y)
        andalso lists:all(Fun, Keys);
    false -> false
  end.

remove_meta(#?TYPE{} = K, V, Acc) ->
  K1 = K#?TYPE{info = #{}},
  Acc#{K1 => V};
remove_meta(K, V, Acc) ->
  Acc#{K => V}.

%% clojerl.IFn

apply(Map, [Key]) ->
  get(Map, Key);
apply(Map, [Key, NotFound]) ->
  get(Map, Key, NotFound);
apply(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for map, got: ", CountBin/binary>>).

hash(Map) ->
  clj_murmur3:unordered(Map).

get(Map, Key) ->
  get(Map, Key, ?NIL).

get(Map, Key, NotFound) ->
  maps:get(Key, Map, NotFound).

keys(Map) ->
  maps:keys(Map).

vals(Map) ->
  maps:values(Map).

without(Map, Key) ->
  maps:remove(Key, Map).

str(Map) when is_map(Map) ->
  clj_rt:print(Map).

seq(Map) when is_map(Map) ->
  FoldFun = fun(K, V, List) ->
                [clj_rt:vector([K, V]) | List]
            end,
  case maps:fold(FoldFun, [], Map) of
    [] -> ?NIL;
    X -> X
  end.

to_list(Map) ->
  FoldFun = fun(K, V, List) ->
                [clj_rt:vector([K, V]) | List]
            end,
  maps:fold(FoldFun, [], Map).
