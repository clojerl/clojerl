-module('clojerl.erlang.Map').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.ILookup').
-behavior('clojerl.IMap').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export(['clojerl.Counted.count'/1]).
-export([ 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        ]).
-export(['clojerl.IEquiv.equiv'/2]).
-export([ 'clojerl.ILookup.get'/2
        , 'clojerl.ILookup.get'/3
        ]).
-export([ 'clojerl.IMap.keys'/1
        , 'clojerl.IMap.vals'/1
        ]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

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

'clojerl.Counted.count'(Map) -> maps:size(Map).

'clojerl.IColl.cons'(Map, X) ->
  case clj_core:seq(X) of
    [K, V] ->
      Map#{K => V};
    _ ->
      throw(<<"Can't conj something that is not"
              " a key/value pair to a map.">>)
  end.

'clojerl.IColl.empty'(_) -> #{}.

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
'clojerl.IEquiv.equiv'(_, _) ->
  false.

remove_meta(#?TYPE{} = K, V, Acc) ->
  K1 = K#?TYPE{info = undefined},
  Acc#{K1 => V};
remove_meta(K, V, Acc) ->
  Acc#{K => V}.

'clojerl.ILookup.get'(Map, Key) ->
  'clojerl.ILookup.get'(Map, Key, undefined).

'clojerl.ILookup.get'(Map, Key, NotFound) ->
  maps:get(Key, Map, NotFound).

'clojerl.IMap.keys'(Map) ->
  maps:keys(Map).

'clojerl.IMap.vals'(Map) -> 
  maps:values(Map).
