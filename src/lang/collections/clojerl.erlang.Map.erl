-module('clojerl.erlang.Map').

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
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Associative.contains_key'(Map, Key) ->
  maps:is_key(Key, Map).

'clojerl.Associative.entry_at'(Map, Key) ->
  case maps:is_key(Key, Map) of
    true ->
      Val = maps:get(Key, Map),
      clj_core:vector([Key, Val]);
    false -> undefined
  end.

'clojerl.Associative.assoc'(Map, Key, Value) ->
  Map#{Key => Value}.

'clojerl.Counted.count'(Map) -> maps:size(Map).

'clojerl.IColl.cons'(Map, X) ->
  IsVector = clj_core:'vector?'(X),
  IsMap    = clj_core:'map?'(X),
  case clj_core:seq_to_list(X) of
    [K, V] when IsVector ->
      Map#{K => V};
    KVs when IsMap ->
      Fun = fun(KV, Acc) ->
                clj_core:assoc(Acc, clj_core:first(KV), clj_core:second(KV))
            end,
      lists:foldl(Fun, Map, KVs);
    _ ->
      throw(<<"Can't conj something that is not a key/value pair to a map.">>)
  end.

%% clojerl.IColl

'clojerl.IColl.empty'(_) -> #{}.

%% clojerl.IEquiv

'clojerl.IEquiv.equiv'(X, Y) when is_map(X), is_map(Y) ->
  case maps:size(X) =:= maps:size(Y) of
    false -> false;
    true  ->
      X1 = maps:fold(fun remove_meta/3, #{}, X),
      Y1 = maps:fold(fun remove_meta/3, #{}, Y),

      FunEquiv = fun(K) ->
                     maps:is_key(K, Y1)
                       andalso clj_core:equiv( maps:get(K, X1)
                                             , maps:get(K, Y1))
                 end,

      lists:all(FunEquiv, maps:keys(X1))
  end;
'clojerl.IEquiv.equiv'(X, Y) when is_map(X) ->
  case clj_core:'map?'(Y) of
    true  ->
      Keys = clj_core:keys(Y),
      Fun = fun(Key) ->
                maps:is_key(Key, X) andalso
                  clj_core:equiv(maps:get(Key, X), clj_core:get(Y, Key))
            end,
      maps:size(X) == clj_core:count(Y)
        andalso lists:all(Fun, Keys);
    false -> false
  end.

remove_meta(#?TYPE{} = K, V, Acc) ->
  K1 = K#?TYPE{info = undefined},
  Acc#{K1 => V};
remove_meta(K, V, Acc) ->
  Acc#{K => V}.

%% clojerl.IFn

'clojerl.IFn.invoke'(Map, [Key]) ->
  clj_core:get(Map, Key);
'clojerl.IFn.invoke'(Map, [Key, NotFound]) ->
  clj_core:get(Map, Key, NotFound);
'clojerl.IFn.invoke'(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for map, got: ", CountBin/binary>>).

'clojerl.IHash.hash'(Map) -> erlang:phash2(Map).

'clojerl.ILookup.get'(Map, Key) ->
  'clojerl.ILookup.get'(Map, Key, undefined).

'clojerl.ILookup.get'(Map, Key, NotFound) ->
  maps:get(Key, Map, NotFound).

'clojerl.IMap.keys'(Map) ->
  maps:keys(Map).

'clojerl.IMap.vals'(Map) ->
  maps:values(Map).

'clojerl.IMap.without'(Map, Key) ->
  maps:remove(Key, Map).

'clojerl.Stringable.str'(Map) when is_map(Map) ->
  StrFun = fun(Key) ->
               KeyStr = clj_core:str(Key),
               ValStr = clj_core:str(maps:get(Key, Map)),

               clj_utils:binary_join([KeyStr, ValStr], <<" ">>)
           end,
  KeyValueStrs = lists:map(StrFun, maps:keys(Map)),
  Strs = clj_utils:binary_join(KeyValueStrs, <<", ">>),
  <<"{", Strs/binary, "}">>.

'clojerl.Seqable.seq'(Map) when is_map(Map) ->
  FoldFun = fun(K, V, List) ->
                [clj_core:vector([K, V]) | List]
            end,
  case maps:fold(FoldFun, [], Map) of
    [] -> undefined;
    X -> X
  end.
