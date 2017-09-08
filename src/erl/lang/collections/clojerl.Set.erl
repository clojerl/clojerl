-module('clojerl.Set').

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.ISet').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

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

-type type() :: #{ ?TYPE => ?M
                 , set   => map()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(Values) when is_list(Values) ->
  KVs = lists:map(fun(X) -> {'clojerl.IHash':hash(X), X} end, Values),
  #{ ?TYPE => ?M
   , set   => maps:from_list(KVs)
   , meta  => ?NIL
   };
?CONSTRUCTOR(Values) ->
  ?CONSTRUCTOR(clj_rt:to_list(Values)).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(#{?TYPE := ?M, set := MapSet}) -> maps:size(MapSet).

%% clojerl.IColl

cons(#{?TYPE := ?M, set := MapSet} = Set, X) ->
  Hash = 'clojerl.IHash':hash(X),
  case maps:is_key(Hash, MapSet) of
    true  -> Set;
    false -> Set#{set => MapSet#{Hash => X}}
  end.

empty(_) -> ?CONSTRUCTOR([]).

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, set := X}
     , #{?TYPE := ?M, set := Y}
     ) ->
  'erlang.Map':equiv(X, Y);
equiv(#{?TYPE := ?M} = X, Y) ->
  clj_rt:'set?'(Y) andalso 'clojerl.IHash':hash(Y) =:= hash(X).

%% clojerl.IFn

apply(#{?TYPE := ?M, set := MapSet}, [Item]) ->
  Hash = 'clojerl.IHash':hash(Item),
  maps:get(Hash, MapSet, ?NIL);
apply(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for set, got: ", CountBin/binary>>).

%% clojerl.IHash

hash(#{?TYPE := ?M, set := MapSet}) ->
  clj_murmur3:unordered(maps:values(MapSet)).

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = Set, Metadata) ->
  Set#{meta => Metadata}.

%% clojerl.ISet

disjoin(#{?TYPE := ?M, set := MapSet} = Set, Value) ->
  Hash = 'clojerl.IHash':hash(Value),
  Set#{set => maps:remove(Hash, MapSet)}.

contains(#{?TYPE := ?M, set := MapSet}, Value) ->
  Hash = 'clojerl.IHash':hash(Value),
  maps:is_key(Hash, MapSet).

get(#{?TYPE := ?M, set := MapSet}, Value) ->
  Hash = 'clojerl.IHash':hash(Value),
  case maps:is_key(Hash, MapSet) of
    true  -> maps:get(Hash, MapSet);
    false -> ?NIL
  end.

%% clojerl.ISeqable

seq(#{?TYPE := ?M, set := MapSet}) ->
  case maps:size(MapSet) of
    0 -> ?NIL;
    _ -> maps:values(MapSet)
  end.

to_list(#{?TYPE := ?M, set := MapSet}) ->
  maps:values(MapSet).

%% clojerl.IStringable

str(#{?TYPE := ?M} = Set) ->
  clj_rt:print(Set).
