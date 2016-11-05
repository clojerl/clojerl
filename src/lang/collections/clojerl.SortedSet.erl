-module('clojerl.SortedSet').

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
  Hashes = [{'clojerl.IHash':hash(X), X} || X <- Values],
  Vals   = [{X, true} || X <- Values],
  #?TYPE{data = {maps:from_list(Hashes), rbdict:from_list(Vals)}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.Counted

count(#?TYPE{name = ?M, data = {Hashes, _}}) -> maps:size(Hashes).

%% clojerl.IColl

cons(#?TYPE{name = ?M, data = {Hashes, Vals}} = S, X) ->
  Hash = 'clojerl.IHash':hash(X),
  case maps:is_key(Hash, Hashes) of
    true  -> S;
    false -> S#?TYPE{data = {Hashes#{Hash => X}, rbdict:store(X, true, Vals)}}
  end.

empty(_) -> ?CONSTRUCTOR([]).

%% clojerl.IEquiv

equiv(#?TYPE{name = ?M} = X, #?TYPE{name = ?M} = Y) ->
  hash(X) =:= hash(Y);
equiv(#?TYPE{name = ?M} = X, Y) ->
  clj_core:'set?'(Y) andalso 'clojerl.IHash':hash(Y) =:= hash(X).

%% clojerl.IFn

apply(#?TYPE{name = ?M, data = {Hashes, _}}, [Item]) ->
  Hash = 'clojerl.IHash':hash(Item),
  case maps:is_key(Hash, Hashes) of
    true  -> maps:get(Hash, Hashes);
    false -> ?NIL
  end;
apply(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for set, got: ", CountBin/binary>>).

%% clojerl.IHash

hash(#?TYPE{name = ?M, data = {Hashes, _}}) ->
  clj_murmur3:unordered(maps:values(Hashes)).

%% clojerl.IMeta

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, ?NIL).

with_meta(#?TYPE{name = ?M, info = Info} = Set, Metadata) ->
  Set#?TYPE{info = Info#{meta => Metadata}}.

%% clojerl.ISet

disjoin(#?TYPE{name = ?M, data = {Hashes, Vals}} = S, Value) ->
  Hash = 'clojerl.IHash':hash(Value),
  case maps:is_key(Hash, Hashes) of
    false -> S;
    true  ->
      S#?TYPE{data = {maps:remove(Hash, Hashes), rbdict:erase(Value, Vals)}}
  end.

contains(#?TYPE{name = ?M, data = {Hashes, _}}, Value) ->
  Hash = 'clojerl.IHash':hash(Value),
  maps:is_key(Hash, Hashes).

get(#?TYPE{name = ?M, data = {Hashes, _}}, Value) ->
  Hash = 'clojerl.IHash':hash(Value),
  case maps:is_key(Hash, Hashes) of
    true  -> maps:get(Hash, Hashes);
    false -> ?NIL
  end.

%% clojerl.Seqable

seq(#?TYPE{name = ?M, data = {Hashes, _}} = Set) ->
  case maps:size(Hashes) of
    0 -> ?NIL;
    _ -> to_list(Set)
  end.

to_list(#?TYPE{name = ?M, data = {_, Vals}}) ->
  [K || {K, _} <- rbdict:to_list(Vals)].

%% clojerl.Stringable

str(#?TYPE{name = ?M, data = {_, Vals}}) ->
  Items = [clj_core:str(K) || {K, _} <- rbdict:to_list(Vals)],
  Strs  = 'clojerl.String':join(Items, <<" ">>),
  <<"#{", Strs/binary, "}">>.
