-module('clojerl.SortedSet').

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

-export([ ?CONSTRUCTOR/1
        , ?CONSTRUCTOR/2
        ]).
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

-type type() :: #{ ?TYPE  => ?M
                 , hashes => map()
                 , dict   => any()
                 , meta   => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(Values) when is_list(Values) ->
  Hashes = [{clj_rt:hash(X), X} || X <- Values],
  Vals   = [{X, true} || X <- Values],
  #{ ?TYPE  => ?M
   , hashes => maps:from_list(Hashes)
   , dict   => rbdict:from_list(Vals)
   , meta   => ?NIL
   }.

-spec ?CONSTRUCTOR(function(), list()) -> type().
?CONSTRUCTOR(Compare, Values) when is_list(Values) ->
  Hashes = [{clj_rt:hash(X), X} || X <- Values],
  Vals   = [{X, true} || X <- Values],
  #{ ?TYPE  => ?M
   , hashes => maps:from_list(Hashes)
   , dict   => rbdict:from_list(Compare, Vals)
   , meta   => ?NIL
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(#{?TYPE := ?M, hashes := Hashes}) -> maps:size(Hashes).

%% clojerl.IColl

cons(#{?TYPE := ?M, hashes := Hashes, dict := Dict} = S, X) ->
  Hash = clj_rt:hash(X),
  case maps:is_key(Hash, Hashes) of
    true  -> S;
    false -> S#{ hashes => Hashes#{Hash => X}
               , dict   => rbdict:store(X, true, Dict)
               }
  end.

empty(#{?TYPE := ?M, dict := Vals}) ->
  Compare = rbdict:compare_fun(Vals),
  ?CONSTRUCTOR(Compare, []).

%% clojerl.IEquiv

equiv(#{?TYPE := ?M} = X, #{?TYPE := ?M} = Y) ->
  hash(X) =:= hash(Y);
equiv(#{?TYPE := ?M} = X, Y) ->
  clj_rt:'set?'(Y) andalso clj_rt:hash(Y) =:= hash(X).

%% clojerl.IFn

apply(#{?TYPE := ?M, hashes := Hashes}, [Item]) ->
  Hash = clj_rt:hash(Item),
  case maps:is_key(Hash, Hashes) of
    true  -> maps:get(Hash, Hashes);
    false -> ?NIL
  end;
apply(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for set, got: ", CountBin/binary>>).

%% clojerl.IHash

hash(#{?TYPE := ?M, hashes := Hashes}) ->
  clj_murmur3:unordered(maps:values(Hashes)).

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = Set, Metadata) ->
  Set#{meta => Metadata}.

%% clojerl.ISet

disjoin(#{?TYPE := ?M, hashes := Hashes, dict := Dict} = S, Value) ->
  Hash = clj_rt:hash(Value),
  case maps:is_key(Hash, Hashes) of
    false -> S;
    true  ->
      S#{ hashes => maps:remove(Hash, Hashes)
        , dict   => rbdict:erase(Value, Dict)
        }
  end.

contains(#{?TYPE := ?M, hashes := Hashes}, Value) ->
  Hash = clj_rt:hash(Value),
  maps:is_key(Hash, Hashes).

get(#{?TYPE := ?M, hashes := Hashes}, Value) ->
  Hash = clj_rt:hash(Value),
  case maps:is_key(Hash, Hashes) of
    true  -> maps:get(Hash, Hashes);
    false -> ?NIL
  end.

%% clojerl.ISeqable

seq(#{?TYPE := ?M, hashes := Hashes} = Set) ->
  case maps:size(Hashes) of
    0 -> ?NIL;
    _ -> to_list(Set)
  end.

to_list(#{?TYPE := ?M, dict := Dict}) ->
  [K || {K, _} <- rbdict:to_list(Dict)].

%% clojerl.IStringable

str(#{?TYPE := ?M} = SortedSet) ->
  clj_rt:print(SortedSet).
