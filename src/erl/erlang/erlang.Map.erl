-module('erlang.Map').

-include("clojerl.hrl").

-behavior('clojerl.IAssociative').
-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.IKVReduce').
-behavior('clojerl.ILookup').
-behavior('clojerl.IMap').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

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
-export(['kv-reduce'/3]).
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

%% clojerl.IAssociative

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

%% clojerl.ICounted

count(Map) -> maps:size(Map).

%% clojerl.IColl

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

empty(_) -> #{}.

%% clojerl.IEquiv

equiv(X, ?MATCH_TYPE = Y) when is_map(X) ->
  equiv_other(X, Y);
equiv(X, Y) when is_map(X), is_map(Y) ->
  equiv_erl_maps(X, Y);
equiv(X, Y) when is_map(X) ->
  equiv_other(X, Y).

equiv_erl_maps(X, Y) ->
  case maps:size(X) =:= maps:size(Y) of
    false -> false;
    true  ->
      X1 = maps:fold(fun remove_meta/3, #{}, X),
      Y1 = maps:fold(fun remove_meta/3, #{}, Y),

      FunEquiv = fun(K) ->
                     maps:is_key(K, Y1)
                     andalso clj_rt:equiv(maps:get(K, X1), maps:get(K, Y1))
                 end,

      lists:all(FunEquiv, maps:keys(X1))
  end.

equiv_other(X, Y) ->
  case clj_rt:'map?'(Y) andalso maps:size(X) =:= clj_rt:count(Y) of
    true ->
      Keys = maps:keys(X),
      Fun = fun(K) ->
                clj_rt:'contains?'(Y, K)
                andalso clj_rt:equiv(maps:get(K, X), clj_rt:get(Y, K))
            end,
      lists:all(Fun, Keys);
    false -> false
  end.

remove_meta(#{?TYPE := _, meta := _} = K, V, Acc) ->
  K1 = K#{meta => ?NIL},
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

%% clojerl.IHash

hash(Map) ->
  clj_murmur3:unordered(Map).

%% clojerl.IKVReduce

'kv-reduce'(Map, Fun, Init) ->
  Ref = make_ref(),
  F = fun(K, V, Acc0) ->
          Acc = clj_rt:apply(Fun, [Acc0, K, V]),
          case 'clojerl.Reduced':is_reduced(Acc) of
            true  -> throw({reduced, Ref, Acc});
            false -> Acc
          end
      end,
  try maps:fold(F, Init, Map)
  catch throw:{reduced, Ref, Acc} ->
      'clojerl.Reduced':deref(Acc)
  end.

%% clojerl.ILookup

get(Map, Key) ->
  get(Map, Key, ?NIL).

get(Map, Key, NotFound) ->
  maps:get(Key, Map, NotFound).

%% clojerl.IMap

keys(Map) ->
  case maps:size(Map) of
    0 -> ?NIL;
    _ -> maps:keys(Map)
  end.

vals(Map) ->
  case maps:size(Map) of
    0 -> ?NIL;
    _ -> maps:values(Map)
  end.

without(Map, Key) ->
  maps:remove(Key, Map).

%% clojerl.IStringable

str(Map) when is_map(Map) ->
  clj_rt:print_str(Map).

%% clojerl.ISeqable

seq(Map) when is_map(Map) ->
case maps:to_list(Map) of
    [] -> ?NIL;
    X  -> X
  end.

to_list(Map) ->
  FoldFun = fun(K, V, List) ->
                [clj_rt:vector([K, V]) | List]
            end,
  maps:fold(FoldFun, [], Map).
