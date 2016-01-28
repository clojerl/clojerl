-module('clojerl.Map').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.Stringable').
-behavior('clojerl.Seqable').
-behavior('clojerl.ISeq').
-behavior('clojerl.IMeta').
-behavior('clojerl.IColl').
-behavior('clojerl.ILookup').

-export([new/1, keys/1, vals/1]).
-export(['clojerl.Counted.count'/1]).
-export(['clojerl.Stringable.str'/1]).
-export(['clojerl.Seqable.seq'/1]).
-export([ 'clojerl.ISeq.first'/1
        , 'clojerl.ISeq.next'/1
        , 'clojerl.ISeq.more'/1
        ]).
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export([ 'clojerl.IColl.count'/1
        , 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        , 'clojerl.IColl.equiv'/2
        ]).
-export([ 'clojerl.ILookup.get'/2
        , 'clojerl.ILookup.get'/3
        ]).

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

-spec keys(type()) -> list().
keys(#?TYPE{name = ?M, data = Map}) -> maps:keys(Map).

-spec vals(type()) -> list().
vals(#?TYPE{name = ?M, data = Map}) -> maps:values(Map).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'(#?TYPE{name = ?M, data = Map}) -> maps:size(Map).

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = Map}) ->
  StrFun = fun(Key) ->
               KeyStr = clj_core:str(Key),
               ValStr = clj_core:str(maps:get(Key, Map)),

               clj_utils:binary_join([KeyStr, ValStr], <<" ">>)
           end,
  KeyValueStrs = lists:map(StrFun, maps:keys(Map)),
  Strs = clj_utils:binary_join(KeyValueStrs, <<", ">>),
  <<"{", Strs/binary, "}">>.

'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = Map}) ->
  FoldFun = fun(K, V, List) ->
                [clj_core:vector([K, V]) | List]
            end,
  case maps:fold(FoldFun, [], Map) of
    [] -> undefined;
    X -> X
  end.

'clojerl.ISeq.first'(Map) -> clj_core:first(clj_core:seq(Map)).

'clojerl.ISeq.next'(Map) -> clj_core:next(clj_core:seq(Map)).

'clojerl.ISeq.more'(Map) -> clj_core:rest(clj_core:seq(Map)).

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?TYPE{name = ?M, info = Info} = Map, Metadata) ->
  Map#?TYPE{info = Info#{meta => Metadata}}.

'clojerl.IColl.count'(#?TYPE{name = ?M, data = Map}) -> maps:size(Map).

'clojerl.IColl.cons'(#?TYPE{name = ?M, data = Map} = HashMap, X) ->
  case clj_core:seq(X) of
    [K, V] ->
      HashMap#?TYPE{data = Map#{K => V}};
    _ ->
      throw(<<"Can't conj something that is not a key/value pair.">>)
  end.

'clojerl.IColl.empty'(_) -> new([]).

'clojerl.IColl.equiv'(X, X) -> true;
'clojerl.IColl.equiv'(_, _) -> false.

'clojerl.ILookup.get'(#?TYPE{name = ?M} = Map, Key) ->
  'clojerl.ILookup.get'(Map, Key, undefined).

'clojerl.ILookup.get'(#?TYPE{name = ?M, data = Map}, Key, NotFound) ->
  maps:get(Key, Map, NotFound).
