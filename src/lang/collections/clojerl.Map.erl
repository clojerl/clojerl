-module('clojerl.Map').

-include("clojerl.hrl").

-behavior('clojerl.Associative').
-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
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

%% clojerl.Associative

'clojerl.Associative.contains_key'(#?TYPE{name = ?M, data = Map}, Key) ->
  maps:is_key(Key, Map).

'clojerl.Associative.entry_at'(#?TYPE{name = ?M, data = Map}, Key) ->
  case maps:is_key(Key, Map) of
    true ->
      Val = maps:get(Key, Map),
      clj_core:vector([Key, Val]);
    false -> undefined
  end.

'clojerl.Associative.assoc'(#?TYPE{name = ?M, data = Map} = M, Key, Value) ->
  M#?TYPE{data = Map#{Key => Value}}.

%% clojerl.Counted

'clojerl.Counted.count'(#?TYPE{name = ?M, data = Map}) -> maps:size(Map).

%% clojerl.IEquiv

'clojerl.IEquiv.equiv'( #?TYPE{name = ?M, data = X}
                      , #?TYPE{name = ?M, data = Y}
                      ) ->
  clj_core:equiv(X, Y);
'clojerl.IEquiv.equiv'(#?TYPE{name = ?M, data = X}, Y) ->
  case clj_core:'map?'(Y) of
    true  -> clj_core:equiv(X, Y);
    false -> false
  end.

%% clojerl.IFn

'clojerl.IFn.invoke'(#?TYPE{name = ?M, data = Map}, [Key]) ->
  clj_core:get(Map, Key);
'clojerl.IFn.invoke'(#?TYPE{name = ?M, data = Map}, [Key, NotFound]) ->
  clj_core:get(Map, Key, NotFound);
'clojerl.IFn.invoke'(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for map, got: ", CountBin/binary>>).

%% clojerl.IColl

'clojerl.IColl.cons'(#?TYPE{name = ?M, data = Map} = HashMap, X) ->
  HashMap#?TYPE{data = clj_core:conj(Map, X)}.

'clojerl.IColl.empty'(_) -> new([]).

%% clojerl.ILookup

'clojerl.ILookup.get'(#?TYPE{name = ?M} = Map, Key) ->
  'clojerl.ILookup.get'(Map, Key, undefined).

'clojerl.ILookup.get'(#?TYPE{name = ?M, data = Map}, Key, NotFound) ->
  maps:get(Key, Map, NotFound).

%% clojerl.IMap

'clojerl.IMap.keys'(#?TYPE{name = ?M, data = Map}) ->
  maps:keys(Map).

'clojerl.IMap.vals'(#?TYPE{name = ?M, data = Map}) ->
  maps:values(Map).

'clojerl.IMap.without'(#?TYPE{name = ?M, data = Map} = M, Key) ->
  M#?TYPE{data = maps:remove(Key, Map)}.

%% clojerl.IMeta

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?TYPE{name = ?M, info = Info} = Map, Metadata) ->
  Map#?TYPE{info = Info#{meta => Metadata}}.

%% clojerl.Seqable

'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = Map}) ->
  FoldFun = fun(K, V, List) ->
                [clj_core:vector([K, V]) | List]
            end,
  case maps:fold(FoldFun, [], Map) of
    [] -> undefined;
    X -> X
  end.

%% clojerl.Stringable

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = Map}) ->
  StrFun = fun(Key) ->
               KeyStr = clj_core:str(Key),
               ValStr = clj_core:str(maps:get(Key, Map)),

               clj_utils:binary_join([KeyStr, ValStr], <<" ">>)
           end,
  KeyValueStrs = lists:map(StrFun, maps:keys(Map)),
  Strs = clj_utils:binary_join(KeyValueStrs, <<", ">>),
  <<"{", Strs/binary, "}">>.
