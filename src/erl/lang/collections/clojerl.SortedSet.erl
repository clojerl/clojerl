-module('clojerl.SortedSet').

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.ILookup').
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
-export([ get/2
        , get/3
        ]).
-export([ meta/1
        , with_meta/2
        ]).
-export([ disjoin/2
        , contains/2
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
  {Count, Hashes} = lists:foldl(fun build_mappings/2, {0, #{}}, Values),
  #{ ?TYPE  => ?M
   , hashes => Hashes
   , dict   => rbdict:from_list(Vals)
   , count  => Count
   , meta   => ?NIL
   }.

-spec ?CONSTRUCTOR(function(), list()) -> type().
?CONSTRUCTOR(Compare, Values) when is_list(Values) ->
  Vals   = [{X, true} || X <- Values],
  {Count, Hashes} = lists:foldl(fun build_mappings/2, {0, #{}}, Values),
  #{ ?TYPE  => ?M
   , hashes => Hashes
   , dict   => rbdict:from_list(Compare, Vals)
   , count  => Count
   , meta   => ?NIL
   }.

%% @private
-spec build_mappings(any(), {integer(), mappings()}) -> {integer(), mappings()}.
build_mappings(Value, {Count, Map}) ->
  Hash = clj_rt:hash(Value),
  {Diff, Entry} = create_entry(Map, Hash, Value, true),
  {Count + Diff, Map#{Hash => Entry}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(#{?TYPE := ?M, count := Count}) -> Count.

%% clojerl.IColl

cons(#{?TYPE := ?M, hashes := Hashes, dict := Dict, count := Count} = S, X) ->
  Hash = clj_rt:hash(X),
  case get_entry(Hashes, Hash, X) of
    ?NIL ->
      {Diff, Entry} = create_entry(Hashes, Hash, X, true),
      S#{ hashes => Hashes#{Hash => Entry}
        , dict   => rbdict:store(X, true, Dict)
        , count  => Count + Diff
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

%% clojerl.ILookup

get(#{?TYPE := ?M} = Set, Value) ->
  get(Set, Value, ?NIL).

get(#{?TYPE := ?M, hashes := Hashes}, Value, NotFound) ->
  Hash = clj_rt:hash(Value),
  case get_entry(Hashes, Hash, Value) of
    ?NIL   -> NotFound;
    {V, _} -> V
  end.

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = Set, Metadata) ->
  Set#{meta => Metadata}.

%% clojerl.ISet

disjoin( #{?TYPE := ?M, hashes := Hashes0, dict := Dict, count := Count} = S
       , Value
       ) ->
  Hash = clj_rt:hash(Value),
  case get_entry(Hashes0, Hash, Value) of
    ?NIL   -> S;
    {V, _} ->
      {Diff, Hashes1} = without_entry(Hashes0, Hash, Value),
      S#{ hashes => Hashes1
        , dict   => rbdict:erase(V, Dict)
        , count  => Count + Diff
        }
  end.

contains(#{?TYPE := ?M, hashes := Hashes}, Value) ->
  Hash = clj_rt:hash(Value),
  get_entry(Hashes, Hash, Value) /= ?NIL.

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
