-module('clojerl.Map').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.ILookup').
-behavior('clojerl.IMap').
-behavior('clojerl.IMeta').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([new/1]).
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
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

-type type() :: #?TYPE{}.

-spec new(list()) -> type().
new(KeyValues) when is_list(KeyValues) ->
  KeyValuePairs = build_key_values([], KeyValues),
  #?TYPE{name = ?M, data = maps:from_list(KeyValuePairs)}.

-spec build_key_values(list(), list()) -> [{any(), any()}].
build_key_values(KeyValues, []) ->
  lists:reverse(KeyValues);
build_key_values(KeyValues, [K, V | Items]) ->
  build_key_values([{K, V} | KeyValues], Items).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'(#?TYPE{name = ?M, data = Map}) -> maps:size(Map).

'clojerl.IEquiv.equiv'( #?TYPE{name = ?M, data = X}
                      , #?TYPE{name = ?M, data = Y}
                      ) ->
  clj_core:equiv(X, Y);
'clojerl.IEquiv.equiv'(#?TYPE{name = ?M, data = X}, Y) ->
  case clj_core:'map?'(Y) of
    true  -> clj_core:equiv(X, Y);
    false -> false
  end.

'clojerl.IColl.cons'(#?TYPE{name = ?M, data = Map} = HashMap, X) ->
  case clj_core:seq(X) of
    [K, V] ->
      HashMap#?TYPE{data = Map#{K => V}};
    _ ->
      throw(<<"Can't conj something that is not a key/value pair.">>)
  end.

'clojerl.IColl.empty'(_) -> new([]).

'clojerl.ILookup.get'(#?TYPE{name = ?M} = Map, Key) ->
  'clojerl.ILookup.get'(Map, Key, undefined).

'clojerl.ILookup.get'(#?TYPE{name = ?M, data = Map}, Key, NotFound) ->
  maps:get(Key, Map, NotFound).

'clojerl.IMap.keys'(#?TYPE{name = ?M, data = Map}) ->
  maps:keys(Map).

'clojerl.IMap.vals'(#?TYPE{name = ?M, data = Map}) ->
  maps:values(Map).

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?TYPE{name = ?M, info = Info} = Map, Metadata) ->
  Map#?TYPE{info = Info#{meta => Metadata}}.

'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = Map}) ->
  FoldFun = fun(K, V, List) ->
                [clj_core:vector([K, V]) | List]
            end,
  case maps:fold(FoldFun, [], Map) of
    [] -> undefined;
    X -> X
  end.

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = Map}) ->
  StrFun = fun(Key) ->
               KeyStr = clj_core:str(Key),
               ValStr = clj_core:str(maps:get(Key, Map)),

               clj_utils:binary_join([KeyStr, ValStr], <<" ">>)
           end,
  KeyValueStrs = lists:map(StrFun, maps:keys(Map)),
  Strs = clj_utils:binary_join(KeyValueStrs, <<", ">>),
  <<"{", Strs/binary, "}">>.
