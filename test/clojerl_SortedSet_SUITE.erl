-module(clojerl_SortedSet_SUITE).

-include("clojerl.hrl").

-export([all/0, init_per_suite/1]).

-export([ new/1
        , count/1
        , str/1
        , seq/1
        , equiv/1
        , hash/1
        , cons/1
        , apply/1
        , disjoin/1
        , complete_coverage/1
        ]).

-type config() :: list().
-type result() :: {comments, string()}.

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(clojerl),
  Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec new(config()) -> result().
new(_Config) ->
  Set = sorted_set([1, 2, 3, 4]),
  1 = clj_core:get(Set, 1),
  2 = clj_core:get(Set, 2),
  3 = clj_core:get(Set, 3),
  4 = clj_core:get(Set, 4),
  undefined   = clj_core:get(Set, 5),
  'not-found' = clj_core:get(Set, 5, 'not-found'),

  Set2 = sorted_set([]),
  undefined = clj_core:get(Set2, whatever),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Set = sorted_set([1, 2, 3, 4]),
  4 = clj_core:count(Set),

  Set2 = sorted_set([]),
  0 = clj_core:count(Set2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Set = sorted_set([1, 2, 3, 4]),
  <<"#{1 2 3 4}">> = clj_core:str(Set),

  Set2 = sorted_set([]),
  <<"#{}">> = clj_core:str(Set2),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Set = sorted_set([1, 2, 3, 4]),
  [1, 2, 3, 4] = lists:sort(clj_core:seq(Set)),
  [1, 2, 3, 4] = lists:sort(clj_core:to_list(Set)),

  Set2 = sorted_set([]),
  undefined = clj_core:seq(Set2),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that sets with the same elements are equivalent"),
  Set1 = clj_core:with_meta(sorted_set([1, 2, 3, 4]), #{a => 1}),
  Set2 = clj_core:with_meta(sorted_set([3, 4, 1, 2]), #{b => 2}),
  true = clj_core:equiv(Set1, Set2),

  ct:comment("Check that sets with the same elements are not equivalent"),
  Set3 = clj_core:with_meta(sorted_set([5, 6, 3, 4]), #{c => 3}),
  false = clj_core:equiv(Set1, Set3),

  ct:comment("A clojerl.Set and something else"),
  false = clj_core:equiv(Set1, whatever),
  false = clj_core:equiv(Set1, 1),
  false = clj_core:equiv(Set1, [1]),

  {comments, ""}.


-spec hash(config()) -> result().
hash(_Config) ->
  List1 = sorted_set([1, 2, 3]),
  List2 = sorted_set([1, 3, 2]),
  List3 = sorted_set([1.0, 2, 3]),

  Hash1 = 'clojerl.IHash':hash(List1),
  Hash2 = 'clojerl.IHash':hash(List2),
  Hash3 = 'clojerl.IHash':hash(List3),

  true = Hash1 == Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  EmptySet = sorted_set([]),

  ct:comment("Conj an element to an empty set"),
  OneSet = clj_core:conj(EmptySet, 1),

  1    = clj_core:count(OneSet),
  true = clj_core:equiv(OneSet, sorted_set([1])),

  ct:comment("Conj a different element to a set with one element"),
  TwoSet = clj_core:conj(OneSet, 2),

  2    = clj_core:count(TwoSet),
  true = clj_core:equiv(TwoSet, sorted_set([1, 2])),

  ct:comment("Conj an existing element in the set"),
  TwoSet = clj_core:conj(TwoSet, 1),

  {comments, ""}.

-spec apply(config()) -> result().
apply(_Config) ->
  HelloKeyword = clj_core:keyword(<<"hello">>),
  EmptySet     = sorted_set([]),

  undefined = clj_core:apply(EmptySet, [HelloKeyword]),

  HelloSet = clj_core:conj(EmptySet, HelloKeyword),
  HelloKeyword = clj_core:apply(HelloSet, [HelloKeyword]),

  ok = try
         clj_core:apply(HelloSet, [HelloKeyword, extra]),
         error
       catch _:_ ->
           ok
       end,

  {comments, ""}.

-spec disjoin(config()) -> result().
disjoin(_Config) ->
  EmptySet = sorted_set([]),
  EmptySet = clj_core:disj(EmptySet, whatever),

  OneSet = clj_core:conj(EmptySet, 1),
  true   = clj_core:equiv(clj_core:disj(OneSet, 1), EmptySet),

  TwoSet = clj_core:conj(OneSet, 2),
  true   = clj_core:equiv(clj_core:disj(TwoSet, 2), OneSet),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  NotEmptySet = sorted_set([a, b, 2, 3]),
  EmptySet    = clj_core:empty(NotEmptySet),
  EmptySet    = sorted_set([]),

  SetMeta  = clj_core:with_meta(sorted_set([1, 2, 3, 4]), #{a => 1}),
  #{a := 1} = clj_core:meta(SetMeta),

  {comments, ""}.

-spec sorted_set([any()]) -> 'clojerl.SortedSet':type().
sorted_set(List) ->
  'clojerl.SortedSet':?CONSTRUCTOR(List).
