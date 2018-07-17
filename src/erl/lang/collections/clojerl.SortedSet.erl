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
-behavior('clojerl.ISorted').
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
-export(['_'/1]).
-export([str/1]).

-import( clj_hash_collision
       , [ get_entry/3
         , create_entry/4
         , without_entry/3
         ]
       ).

-type mappings() :: #{integer() => {any(), true} | [{any(), true}]}.

-type type() :: #{ ?TYPE  => ?M
                 , hashes => mappings()
                 , dict   => any()
                 , meta   => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(Values) when is_list(Values) ->
  Vals   = [{X, true} || X <- Values],
  #{ ?TYPE  => ?M
   , hashes => lists:foldl(fun build_mappings/2, #{}, Values)
   , dict   => rbdict:from_list(Vals)
   , meta   => ?NIL
   }.

-spec ?CONSTRUCTOR(function(), list()) -> type().
?CONSTRUCTOR(Compare, Values) when is_list(Values) ->
  Vals   = [{X, true} || X <- Values],
  #{ ?TYPE  => ?M
   , hashes => lists:foldl(fun build_mappings/2, #{}, Values)
   , dict   => rbdict:from_list(Compare, Vals)
   , meta   => ?NIL
   }.

%% @private
-spec build_mappings(any(), mappings()) -> mappings().
build_mappings(Value, Map) ->
  Hash = clj_rt:hash(Value),
  Map#{Hash => create_entry(Map, Hash, Value, true)}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(#{?TYPE := ?M, dict := Dict}) -> rbdict:size(Dict).

%% clojerl.IColl

cons(#{?TYPE := ?M, hashes := Hashes, dict := Dict} = S, X) ->
  Hash = clj_rt:hash(X),
  case get_entry(Hashes, Hash, X) of
    ?NIL ->
      Entry = create_entry(Hashes, Hash, X, true),
      S#{ hashes => Hashes#{Hash => Entry}
        , dict   => rbdict:store(X, true, Dict)
        };
    _ -> S
  end.

empty(#{?TYPE := ?M, dict := Vals}) ->
  Compare = rbdict:compare_fun(Vals),
  ?CONSTRUCTOR(Compare, []).

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, hashes := MapSetX} = X
     , #{?TYPE := ?M, hashes := MapSetY} = Y) ->
  count(X) == count(Y) andalso clj_hash_collision:equiv(MapSetX, MapSetY);
equiv(#{?TYPE := ?M, hashes := MapSetX}, Y) ->
  clj_rt:'set?'(Y) andalso do_equiv(maps:values(MapSetX), Y).

do_equiv([], _) ->
  true;
do_equiv([{V, _} | Rest], Y) ->
  case 'clojerl.ISet':contains(Y, V) of
    false -> false;
    true  -> do_equiv(Rest, Y)
  end.

%% clojerl.IFn

apply(#{?TYPE := ?M, hashes := Hashes}, [Item]) ->
  Hash = clj_rt:hash(Item),
  case get_entry(Hashes, Hash, Item) of
    ?NIL   -> ?NIL;
    {K, _} -> K
  end;
apply(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for set, got: ", CountBin/binary>>).

%% clojerl.IHash

hash(#{?TYPE := ?M, dict := Dict}) ->
  clj_murmur3:unordered(rbdict:fetch_keys(Dict)).

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = Set, Metadata) ->
  Set#{meta => Metadata}.

%% clojerl.ISet

disjoin(#{?TYPE := ?M, hashes := Hashes0, dict := Dict} = S, Value) ->
  Hash = clj_rt:hash(Value),
  case get_entry(Hashes0, Hash, Value) of
    ?NIL   -> S;
    {V, _} ->
      Hashes1 = without_entry(Hashes0, Hash, Value),
      S#{ hashes => Hashes1
        , dict   => rbdict:erase(V, Dict)
        }
  end.

contains(#{?TYPE := ?M, hashes := Hashes}, Value) ->
  Hash = clj_rt:hash(Value),
  get_entry(Hashes, Hash, Value) /= ?NIL.

get(#{?TYPE := ?M, hashes := Hashes}, Value) ->
  Hash = clj_rt:hash(Value),
  case get_entry(Hashes, Hash, Value) of
    ?NIL   -> ?NIL;
    {V, _} -> V
  end.

%% clojerl.ISeqable

seq(#{?TYPE := ?M, hashes := Hashes} = Set) ->
  case maps:size(Hashes) of
    0 -> ?NIL;
    _ -> to_list(Set)
  end.

to_list(#{?TYPE := ?M, dict := Dict}) ->
  rbdict:fetch_keys(Dict).

%% clojerl.ISorted

'_'(_) -> ?NIL.

%% clojerl.IStringable

str(#{?TYPE := ?M} = SortedSet) ->
  clj_rt:print(SortedSet).
