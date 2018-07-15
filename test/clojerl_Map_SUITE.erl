-module(clojerl_Map_SUITE).

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
        , apply/1
        , hash/1
        , hash_collision/1
        , cons/1
        , associative/1
        , to_erl/1
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
  Map = clj_rt:hash_map([1, 2, 3, 4]),
  2 = clj_rt:get(Map, 1),
  4 = clj_rt:get(Map, 3),

  [1, 3] = lists:sort(clj_rt:keys(Map)),
  [2, 4] = lists:sort(clj_rt:vals(Map)),

  Map2 = clj_rt:hash_map([]),
  ?NIL = clj_rt:seq(Map2),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Map = clj_rt:hash_map([1, 2, 3, 4]),
  2 = clj_rt:count(Map),

  Map2 = clj_rt:hash_map([]),
  0 = clj_rt:count(Map2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Map = clj_rt:hash_map([1, 2, 3, 4]),
  <<"{3 4, 1 2}">> = clj_rt:str(Map),

  Map2 = clj_rt:hash_map([]),
  <<"{}">> = clj_rt:str(Map2),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Map = clj_rt:hash_map([1, 2, 3, 4]),

  KVs  = lists:map(fun clj_rt:seq/1, clj_rt:seq(Map)),
  true = clj_rt:equiv([[1, 2], [3, 4]], lists:sort(KVs)),

  Map2 = clj_rt:hash_map([]),
  ?NIL = clj_rt:seq(Map2),

  MapList = clj_rt:to_list(Map),
  true = clj_rt:equiv([[1, 2], [3, 4]], lists:sort(MapList)),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that maps with the same elements are equivalent"),
  Map1 = clj_rt:with_meta(clj_rt:hash_map([1.0, 2, 3, 4]), #{a => 1}),
  Map2 = clj_rt:with_meta(clj_rt:hash_map([3, 4, 1.0, 2]), #{b => 2}),
  true = clj_rt:equiv(Map1, Map2),

  ct:comment("Check that maps with different elements are not equivalent"),
  Map3 = clj_rt:with_meta(clj_rt:hash_map([5, 6, 3, 4]), #{c => 3}),
  false = clj_rt:equiv(Map1, Map3),

  ct:comment("A clojerl.Map and an erlang.Map"),
  true = clj_rt:equiv(Map1, #{1.0 => 2, 3 => 4}),
  false = clj_rt:equiv(Map1, #{1.0 => 2}),
  false = clj_rt:equiv(Map1, #{1.0 => 2, 3 => 4, 5 => 6}),

  ct:comment("A clojerl.Map and something else"),
  false = clj_rt:equiv(Map1, whatever),
  false = clj_rt:equiv(Map1, 1),
  false = clj_rt:equiv(Map1, [1]),

  {comments, ""}.

-spec apply(config()) -> result().
apply(_Config) ->
  Map = clj_rt:hash_map([1, a, 2, b]),

  ct:comment("Invoke a map"),
  a = clj_rt:apply(Map, [1]),
  b = clj_rt:apply(Map, [2]),
  ?NIL = clj_rt:apply(Map, [3]),

  ct:comment("Invoke a map with a not-found value"),
  c = clj_rt:apply(Map, [3, c]),

  ct:comment("Invoke a map with three arguments"),
  ok = try clj_rt:apply(Map, [1, 2, 3]), error
       catch _:_ -> ok end,

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  Map1 = clj_rt:hash_map([1, a, 2, b]),
  Map1 = clj_rt:hash_map([2, b, 1, a]),
  Map2 = clj_rt:hash_map([2, b, 1, a, 3, c]),
  Map3 = clj_rt:hash_map([2.0, b, 1.0, a, 3.0, c]),

  Hash1 = 'clojerl.IHash':hash(Map1),
  Hash2 = 'clojerl.IHash':hash(Map2),
  Hash3 = 'clojerl.IHash':hash(Map3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  EmptyMap  = clj_rt:hash_map([]),
  HashMap   = 'clojerl.IHash':hash(EmptyMap),

  EmptyList = 'clojerl.List':?CONSTRUCTOR([]),
  HashList  = 'clojerl.IHash':hash(EmptyList),
  true      = HashMap =/= HashList,

  EmptySet  = 'clojerl.Set':?CONSTRUCTOR([]),
  HashSet   = 'clojerl.IHash':hash(EmptySet),
  true      = HashMap =:= HashSet,

  {comments, ""}.

-spec hash_collision(config()) -> result().
hash_collision(_Config) ->
  EmptySet = clj_rt:hash_set([]),
  EmptyMap = clj_rt:hash_map([]),
  HashMap1 = clj_rt:hash_map([EmptyMap, map, EmptySet, set]),

  2   = clj_rt:count(HashMap1),
  map = clj_rt:get(HashMap1, EmptyMap),
  set = clj_rt:get(HashMap1, EmptySet),

  HashMap2 = clj_rt:dissoc(HashMap1, EmptyMap),
  1   = clj_rt:count(HashMap2),
  set = clj_rt:get(HashMap2, EmptySet),

  HashMap3 = clj_rt:dissoc(HashMap1, EmptySet),
  1   = clj_rt:count(HashMap3),
  map = clj_rt:get(HashMap3, EmptyMap),

  HashMap4 = clj_rt:assoc(HashMap3, EmptySet, set),
  2   = clj_rt:count(HashMap4),
  map = clj_rt:get(HashMap4, EmptyMap),
  set = clj_rt:get(HashMap4, EmptySet),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  EmptyMap = clj_rt:hash_map([]),

  ct:comment("Conj a key-value pair to an empty map"),
  OneMap = clj_rt:conj(EmptyMap, clj_rt:vector([1, 2])),

  1    = clj_rt:count(OneMap),
  true = clj_rt:equiv(OneMap, #{1 => 2}),

  ct:comment("Conj a key-value pair to a map with one"),
  TwoMap = clj_rt:conj(OneMap, clj_rt:vector([3, 4])),

  2    = clj_rt:count(TwoMap),
  true = clj_rt:equiv(TwoMap, #{1 => 2, 3 => 4}),

  ct:comment("Conj another map to a map with one"),
  ThreeMap = clj_rt:conj(TwoMap, #{5 => 6}),
  3    = clj_rt:count(ThreeMap),
  true = clj_rt:equiv(ThreeMap, #{1 => 2, 3 => 4, 5 => 6}),

  ct:comment("Conj a map to nil"),
  EmptyMap = clj_rt:conj(EmptyMap, ?NIL),
  OneMap   = clj_rt:conj(OneMap, ?NIL),
  TwoMap   = clj_rt:conj(TwoMap, ?NIL),
  ThreeMap = clj_rt:conj(ThreeMap, ?NIL),

  ct:comment("Conj something that is not a key-value pair to an empty map"),
  ok = try clj_rt:conj(EmptyMap, clj_rt:vector([1])), error
       catch _:_ -> ok end,

  {comments, ""}.

-spec associative(config()) -> result().

associative(_Config) ->
  EmptyMap = clj_rt:hash_map([]),
  false    = clj_rt:'contains?'(EmptyMap, 1),

  OneMap = clj_rt:assoc(EmptyMap, 1, a),
  true   = clj_rt:'contains?'(OneMap, 1),
  false  = clj_rt:'contains?'(OneMap, 2),

  TwoMap = clj_rt:assoc(OneMap, 2, b),
  true   = clj_rt:'contains?'(TwoMap, 1),
  true   = clj_rt:'contains?'(TwoMap, 2),

  Pair   = clj_rt:find(TwoMap, 2),
  true   = clj_rt:equiv(Pair, clj_rt:vector([2, b])),

  ?NIL   = clj_rt:find(TwoMap, 3),

  TwoMap = clj_rt:dissoc(TwoMap, 3),
  OneMap = clj_rt:dissoc(TwoMap, 2),

  ct:comment("Use symbols with different metadata as keys"),
  HelloSym1 = clj_rt:with_meta(clj_rt:symbol(<<"hello">>), #{a => 1}),
  HelloSym2 = clj_rt:with_meta(clj_rt:symbol(<<"hello">>), #{b => 2}),
  HelloMap  = clj_rt:assoc(EmptyMap, HelloSym1, a),
  HelloPair = clj_rt:find(HelloMap, HelloSym2),
  true      = clj_rt:equiv(HelloPair, clj_rt:vector([HelloSym1, a])),
  true      = clj_rt:'contains?'(HelloMap, HelloSym2),

  ct:comment("The new key with different metadata doesn't replace the old one"),
  HelloMap2 = clj_rt:assoc(HelloMap, HelloSym2, a),
  HelloSym1 = clj_rt:first(clj_rt:find(HelloMap2, HelloSym2)),

  {comments, ""}.

-spec to_erl(config()) -> result().
to_erl(_Config) ->
  EmptyMap = clj_rt:hash_map([]),
  #{} = clj_rt:'->erl'(EmptyMap, false),
  #{} = clj_rt:'->erl'(EmptyMap, true),

  Map1     = clj_rt:hash_map([EmptyMap, EmptyMap]),

  Map1Erl1 = clj_rt:'->erl'(Map1, false),
  EmptyMap = maps:get(EmptyMap, Map1Erl1),

  Map1Erl2 = clj_rt:'->erl'(Map1, true),
  Value    = maps:get(#{}, Map1Erl2),
  Value    = #{},

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  NotEmptyMap = clj_rt:hash_map([a, b, 2, 3]),
  EmptyMap    = clj_rt:empty(NotEmptyMap),
  EmptyMap    = clj_rt:hash_map([]),

  MapMeta  = clj_rt:with_meta(clj_rt:hash_map([1, 2, 3, 4]), #{a => 1}),
  #{a := 1} = clj_rt:meta(MapMeta),

  Hash1 = 'clojerl.IHash':hash(NotEmptyMap),
  Hash2 = 'clojerl.IHash':hash(EmptyMap),

  true = Hash1 =/= Hash2,

  Hash1 = 'clojerl.IHash':hash(NotEmptyMap),

  #{a := b, 2 := 3} = 'clojerl.Map':'->erl'(NotEmptyMap, false),

  {comments, ""}.
