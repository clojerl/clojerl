-module(clojerl_SortedSet_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ new/1
        , count/1
        , str/1
        , seq/1
        , equiv/1
        , hash/1
        , hash_collision/1
        , cons/1
        , apply/1
        , disjoin/1
        , complete_coverage/1
        ]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec new(config()) -> result().
new(_Config) ->
  Set = sorted_set([1, 2, 3, 4]),
  1 = clj_rt:get(Set, 1),
  2 = clj_rt:get(Set, 2),
  3 = clj_rt:get(Set, 3),
  4 = clj_rt:get(Set, 4),
  ?NIL   = clj_rt:get(Set, 5),
  'not-found' = clj_rt:get(Set, 5, 'not-found'),

  Set2 = sorted_set([]),
  ?NIL = clj_rt:get(Set2, whatever),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Set = sorted_set([1, 2, 3, 4]),
  4 = clj_rt:count(Set),

  Set2 = sorted_set([]),
  0 = clj_rt:count(Set2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Set = sorted_set([1, 2, 3, 4]),
  <<"#{1 2 3 4}">> = clj_rt:str(Set),

  Set2 = sorted_set([]),
  <<"#{}">> = clj_rt:str(Set2),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Set = sorted_set([1, 2, 3, 4]),
  [1, 2, 3, 4] = lists:sort(clj_rt:seq(Set)),
  [1, 2, 3, 4] = lists:sort(clj_rt:to_list(Set)),

  Set2 = sorted_set([]),
  ?NIL = clj_rt:seq(Set2),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that sets with the same elements are equivalent"),
  Set1 = clj_rt:with_meta(sorted_set([1, 2, 3, 4]), #{a => 1}),
  Set2 = clj_rt:with_meta(sorted_set([3, 4, 1, 2]), #{b => 2}),
  true = clj_rt:equiv(Set1, Set2),

  ct:comment("Check that sets with different elements are not equivalent"),
  Set3  = clj_rt:with_meta(sorted_set([5, 6, 3, 4]), #{c => 3}),
  false = clj_rt:equiv(Set1, Set3),

  ct:comment("Check that sets with less elements are not equivalent"),
  Set4  = clj_rt:with_meta(sorted_set([1, 2, 3]), #{c => 3}),
  false = clj_rt:equiv(Set1, Set4),

  ct:comment("A clojerl.Set and a clojerl.SortedSet"),
  HashSet1 = clj_rt:hash_set([1, 2, 3, 4]),
  true     = clj_rt:equiv(Set1, HashSet1),
  HashSet2 = clj_rt:hash_set([1, 2, 3]),
  false    = clj_rt:equiv(Set1, HashSet2),

  ct:comment("A clojerl.Set and something else"),
  false = clj_rt:equiv(Set1, whatever),
  false = clj_rt:equiv(Set1, 1),
  false = clj_rt:equiv(Set1, [1]),

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

-spec hash_collision(config()) -> result().
hash_collision(_Config) ->
  EmptySet   = clj_rt:hash_set([]),
  EmptyMap   = clj_rt:hash_map([]),
  SortedSet1 = sorted_set([EmptyMap, EmptySet]),

  2        = clj_rt:count(SortedSet1),
  EmptyMap = clj_rt:get(SortedSet1, EmptyMap),
  EmptySet = clj_rt:get(SortedSet1, EmptySet),

  SortedSet2 = clj_rt:disj(SortedSet1, EmptyMap),
  1        = clj_rt:count(SortedSet2),
  EmptySet = clj_rt:get(SortedSet2, EmptySet),

  SortedSet3 = clj_rt:disj(SortedSet1, EmptySet),
  1        = clj_rt:count(SortedSet3),
  EmptyMap = clj_rt:get(SortedSet3, EmptyMap),

  SortedSet4 = clj_rt:conj(SortedSet3, EmptySet),
  2        = clj_rt:count(SortedSet4),
  EmptyMap = clj_rt:get(SortedSet4, EmptyMap),
  EmptySet = clj_rt:get(SortedSet4, EmptySet),

  SortedSet5 = sorted_set([EmptySet, EmptyMap]),
  true       = clj_rt:equiv(SortedSet5, SortedSet1),
  true       = clj_rt:equiv(SortedSet1, SortedSet5),

  {comments, ""}.


-spec cons(config()) -> result().
cons(_Config) ->
  EmptySet = sorted_set([]),

  ct:comment("Conj an element to an empty set"),
  OneSet = clj_rt:conj(EmptySet, 1),

  1    = clj_rt:count(OneSet),
  true = clj_rt:equiv(OneSet, sorted_set([1])),

  ct:comment("Conj a different element to a set with one element"),
  TwoSet = clj_rt:conj(OneSet, 2),

  2    = clj_rt:count(TwoSet),
  true = clj_rt:equiv(TwoSet, sorted_set([1, 2])),

  ct:comment("Conj an existing element in the set"),
  TwoSet = clj_rt:conj(TwoSet, 1),

  ct:comment("Conj the same symbol with different meta to an empty set"),
  OneSymbol         = clj_rt:symbol(<<"one">>),
  OneSymbolWithMeta = clj_rt:with_meta(OneSymbol, #{one => 1}),
  OneSymbolSet      = clj_rt:conj(EmptySet, OneSymbol),
  OneSymbolSet2     = clj_rt:conj(OneSymbolSet, OneSymbolWithMeta),

  ct:comment("Sets should be equivalent"),
  true      = clj_rt:equiv(OneSymbolSet, OneSymbolSet2),
  ct:comment("The symbol with the metadata is not added to the set"),
  OneSymbol = clj_rt:first(OneSymbolSet2),

  {comments, ""}.

-spec apply(config()) -> result().
apply(_Config) ->
  HelloKeyword = clj_rt:keyword(<<"hello">>),
  EmptySet     = sorted_set([]),

  ?NIL = clj_rt:apply(EmptySet, [HelloKeyword]),

  HelloSet = clj_rt:conj(EmptySet, HelloKeyword),
  HelloKeyword = clj_rt:apply(HelloSet, [HelloKeyword]),

  ok = try
         clj_rt:apply(HelloSet, [HelloKeyword, extra]),
         error
       catch _:_ ->
           ok
       end,

  {comments, ""}.

-spec disjoin(config()) -> result().
disjoin(_Config) ->
  EmptySet = sorted_set([]),
  EmptySet = clj_rt:disj(EmptySet, whatever),

  OneSet = clj_rt:conj(EmptySet, 1),
  true   = clj_rt:equiv(clj_rt:disj(OneSet, 1), EmptySet),

  TwoSet = clj_rt:conj(OneSet, 2),
  true   = clj_rt:equiv(clj_rt:disj(TwoSet, 2), OneSet),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  NotEmptySet = sorted_set([a, b, 2, 3]),
  EmptySet    = clj_rt:empty(NotEmptySet),
  true        = clj_rt:equiv(sorted_set([]), EmptySet),

  SetMeta  = clj_rt:with_meta(sorted_set([1, 2, 3, 4]), #{a => 1}),
  #{a := 1} = clj_rt:meta(SetMeta),

  ?NIL = 'clojerl.SortedSet':'_'([]),

  {comments, ""}.

-spec sorted_set([any()]) -> 'clojerl.SortedSet':type().
sorted_set(List) ->
  'clojerl.SortedSet':?CONSTRUCTOR(List).
