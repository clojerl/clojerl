-module('clojerl.SortedMap').

-compile({no_auto_import, [{apply, 2}]}).

-include("clojerl.hrl").

-behavior('clojerl.Associative').
-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.ILookup').
-behavior('clojerl.IMap').
-behavior('clojerl.IMeta').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

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

-type type()     :: #?TYPE{data :: orddict:orddict()}.

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(KeyValues) when is_list(KeyValues) ->
  KeyValuePairs = build_key_values([], KeyValues),
  #?TYPE{name = ?M, data = orddict:from_list(KeyValuePairs)}.

%% @private
-spec build_key_values(list(), list()) -> [{any(), any()}].
build_key_values(KeyValues, []) ->
  lists:reverse(KeyValues);
build_key_values(KeyValues, [K, V | Items]) ->
  build_key_values([{K, V} | KeyValues], Items).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.Associative

contains_key(#?TYPE{name = ?M, data = Map}, Key) ->
  orddict:is_key(Key, Map).

entry_at(#?TYPE{name = ?M, data = Map}, Key) ->
  case orddict:is_key(Key, Map) of
    true ->
      Val = orddict:fetch(Key, Map),
      clj_core:vector([Key, Val]);
    false -> undefined
  end.

assoc(#?TYPE{name = ?M, data = Map} = M, Key, Value) ->
  M#?TYPE{data = orddict:store(Key, Value, Map)}.

%% clojerl.Counted

count(#?TYPE{name = ?M, data = Map}) ->
  orddict:size(Map).

%% clojerl.IEquiv

equiv( #?TYPE{name = ?M, data = Map1}
     , #?TYPE{name = ?M, data = Map2}
     ) ->
  clj_murmur3:ordered(Map1) =:= clj_murmur3:ordered(Map2);
equiv(#?TYPE{name = ?M, data = Map}, Y) ->
  case clj_core:'map?'(Y) of
    true  ->
      Fun = fun(Key) ->
                orddict:is_key(Key, Map) andalso
                  clj_core:equiv(orddict:fetch(Key, Map), clj_core:get(Y, Key))
            end,
      orddict:size(Map) == clj_core:count(Y)
        andalso lists:all(Fun, orddict:fetch_keys(Map));
    false -> false
  end.

%% clojerl.IFn

apply(#?TYPE{name = ?M} = M, [Key]) ->
  apply(M, [Key, undefined]);
apply(#?TYPE{name = ?M, data = Map}, [Key, NotFound]) ->
  case orddict:is_key(Key, Map) of
    true  -> orddict:fetch(Key, Map);
    false -> NotFound
  end;
apply(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for map, got: ", CountBin/binary>>).

%% clojerl.IColl

cons(#?TYPE{name = ?M} = Map, undefined) ->
  Map;
cons(#?TYPE{name = ?M, data = Map} = M, X) ->
  IsVector = clj_core:'vector?'(X),
  IsMap    = clj_core:'map?'(X),
  case clj_core:to_list(X) of
    [K, V] when IsVector ->
      M#?TYPE{data = orddict:store(K, V, Map)};
    KVs when IsMap ->
      Fun = fun(KV, Acc) ->
                assoc(Acc, clj_core:first(KV), clj_core:second(KV))
            end,
      lists:foldl(Fun, M, KVs);
    _ ->
      throw(<<"Can't conj something that is not a key/value pair or "
              "another map to a map.">>)
  end.

empty(_) -> ?CONSTRUCTOR([]).

%% clojerl.IHash

hash(#?TYPE{name = ?M} = Map) ->
  clj_murmur3:unordered(Map).

%% clojerl.ILookup

get(#?TYPE{name = ?M} = Map, Key) ->
  get(Map, Key, undefined).

get(#?TYPE{name = ?M, data = Map}, Key, NotFound) ->
  case orddict:is_key(Key, Map) of
    true  -> orddict:fetch(Key, Map);
    false -> NotFound
  end.

%% clojerl.IMap

keys(#?TYPE{name = ?M, data = Map}) ->
  orddict:fetch_keys(Map).

vals(#?TYPE{name = ?M, data = Map}) ->
  [Val || {_, Val} <- orddict:to_list(Map)].

without(#?TYPE{name = ?M, data = Map} = M, Key) ->
  M#?TYPE{data = orddict:erase(Key, Map)}.

%% clojerl.IMeta

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

with_meta(#?TYPE{name = ?M, info = Info} = Map, Metadata) ->
  Map#?TYPE{info = Info#{meta => Metadata}}.

%% clojerl.Seqable

seq(#?TYPE{name = ?M} = Map) ->
  case to_list(Map) of
    [] -> undefined;
    X -> X
  end.

to_list(#?TYPE{name = ?M, data = Map}) ->
  FoldFun = fun(Key, List) ->
                [clj_core:vector([Key, orddict:fetch(Key, Map)]) | List]
            end,
  List = lists:foldl(FoldFun, [], orddict:fetch_keys(Map)),
  lists:reverse(List).

%% clojerl.Stringable

str(#?TYPE{name = ?M, data = Map}) ->
  StrFun = fun(Key) ->
               KeyStr = clj_core:str(Key),
               ValStr = clj_core:str(orddict:fetch(Key, Map)),
               'clojerl.String':join([KeyStr, ValStr], <<" ">>)
           end,
  KeyValueStrs = lists:map(StrFun, orddict:fetch_keys(Map)),
  Strs = 'clojerl.String':join(KeyValueStrs, <<", ">>),
  <<"{", Strs/binary, "}">>.
