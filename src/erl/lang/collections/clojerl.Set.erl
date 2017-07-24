-module('clojerl.Set').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.ISet').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([?CONSTRUCTOR/1]).
-export([count/1]).
-export([ cons/2
        , empty/1
        ]).
-export([equiv/2]).
-export([apply/2]).
-export([hash/1]).
-export([ meta/1
        , with_meta/2
        ]).
-export([ disjoin/2
        , contains/2
        , get/2
        ]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

-type type() :: #?TYPE{}.

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(Values) when is_list(Values) ->
  KVs = lists:map(fun(X) -> {'clojerl.IHash':hash(X), X} end, Values),
  #?TYPE{data = maps:from_list(KVs)}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.Counted

count(#?TYPE{name = ?M, data = MapSet}) -> maps:size(MapSet).

%% clojerl.IColl

cons(#?TYPE{name = ?M, data = MapSet} = Set, X) ->
  Hash = 'clojerl.IHash':hash(X),
  case maps:is_key(Hash, MapSet) of
    true  -> Set;
    false -> Set#?TYPE{data = MapSet#{Hash => X}}
  end.

empty(_) -> ?CONSTRUCTOR([]).

%% clojerl.IEquiv

equiv( #?TYPE{name = ?M, data = X}
     , #?TYPE{name = ?M, data = Y}
     ) ->
  clj_rt:equiv(X, Y);
equiv(#?TYPE{name = ?M} = X, Y) ->
  clj_rt:'set?'(Y) andalso 'clojerl.IHash':hash(Y) =:= hash(X).

%% clojerl.IFn

apply(#?TYPE{name = ?M, data = MapSet}, [Item]) ->
  Hash = 'clojerl.IHash':hash(Item),
  maps:get(Hash, MapSet, ?NIL);
apply(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for set, got: ", CountBin/binary>>).

%% clojerl.IHash

hash(#?TYPE{name = ?M, data = MapSet}) ->
  clj_murmur3:unordered(maps:values(MapSet)).

%% clojerl.IMeta

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, ?NIL).

with_meta(#?TYPE{name = ?M, info = Info} = Set, Metadata) ->
  Set#?TYPE{info = Info#{meta => Metadata}}.

%% clojerl.ISet

disjoin(#?TYPE{name = ?M, data = MapSet} = Set, Value) ->
  Hash = 'clojerl.IHash':hash(Value),
  Set#?TYPE{name = ?M, data = maps:remove(Hash, MapSet)}.

contains(#?TYPE{name = ?M, data = MapSet}, Value) ->
  Hash = 'clojerl.IHash':hash(Value),
  maps:is_key(Hash, MapSet).

get(#?TYPE{name = ?M, data = MapSet}, Value) ->
  Hash = 'clojerl.IHash':hash(Value),
  case maps:is_key(Hash, MapSet) of
    true  -> maps:get(Hash, MapSet);
    false -> ?NIL
  end.

%% clojerl.Seqable

seq(#?TYPE{name = ?M, data = MapSet}) ->
  case maps:size(MapSet) of
    0 -> ?NIL;
    _ -> maps:values(MapSet)
  end.

to_list(#?TYPE{name = ?M, data = MapSet}) ->
  maps:values(MapSet).

%% clojerl.Stringable

str(#?TYPE{name = ?M} = Set) ->
  clj_rt:print(Set).
