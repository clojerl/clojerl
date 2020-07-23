-module(erlang_Map_SUITE).

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
        , cons/1
        , associative/1
        , to_clj/1
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
  Map = #{1 => 2, 3 => 4},
  2 = clj_rt:get(Map, 1),
  4 = clj_rt:get(Map, 3),

  [1, 3] = lists:sort(clj_rt:keys(Map)),
  [2, 4] = lists:sort(clj_rt:vals(Map)),

  Map2 = #{},
  not_found = clj_rt:get(Map2, 3, not_found),
  ?NIL = clj_rt:vals(Map2),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Map = #{1 => 2, 3 => 4},
  2 = clj_rt:count(Map),

  Map2 = #{},
  0 = clj_rt:count(Map2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Map = #{1 => 2, 3 => 4},
  <<"#erl{3 4, 1 2}">> = clj_rt:str(Map),

  Map2 = #{},
  <<"#erl{}">> = clj_rt:str(Map2),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Map = #{1 => 2, 3 => 4},

  KVs  = lists:map(fun clj_rt:seq/1, clj_rt:seq(Map)),
  true = clj_rt:equiv([[1, 2], [3, 4]], lists:sort(KVs)),

  Map2 = #{},
  ?NIL = clj_rt:seq(Map2),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  Symbol = clj_rt:symbol(<<"hello">>),

  ct:comment("Check that maps with the same elements are equivalent"),
  Map1 = #{1 => 2, Symbol => 4},
  Map2 = #{Symbol => 4, 1 => 2},
  true = clj_rt:equiv(Map1, Map2),

  ct:comment("Check maps with the same number of keys are not equivalent"),
  Map3 = #{5 => 6, 3 => 4},
  false = clj_rt:equiv(Map1, Map3),

  ct:comment("Check maps with different number of keys are not equivalent"),
  Map4 = #{5 => 6, 3 => 4, 7 => 8},
  false = clj_rt:equiv(Map1, Map4),

  ct:comment("A erlang.Map and an clojerl.Map"),
  true = clj_rt:equiv(Map1, clj_rt:hash_map([1, 2, Symbol, 4])),
  false = clj_rt:equiv(Map1, clj_rt:hash_map([1, 2])),
  false = clj_rt:equiv(Map1, clj_rt:hash_map([1, 2, 3, 4, 5, 6])),

  ct:comment("A erlang.Map and something else"),
  false = clj_rt:equiv(Map1, whatever),
  false = clj_rt:equiv(Map1, 1),
  false = clj_rt:equiv(Map1, [1]),

  {comments, ""}.

-spec apply(config()) -> result().
apply(_Config) ->
  Map = #{1 => a, 2 => b},

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
  Map1 = #{1 => a, 2 => b},
  Map1 = #{2 => b, 1 => a},
  Map2 = #{2 => b, 1 => a, 3 => c},
  Map3 = #{2.0 => b, 1.0 => a, 3.0 => c},

  Hash1 = 'clojerl.IHash':hash(Map1),
  Hash2 = 'clojerl.IHash':hash(Map2),
  Hash3 = 'clojerl.IHash':hash(Map3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  EmptyMap = #{},

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

  ct:comment("Conj something that is not a key-value pair to an empty map"),
  ok = try clj_rt:conj(EmptyMap, clj_rt:vector([1])), error
       catch _:_ -> ok end,

  {comments, ""}.

-spec associative(config()) -> result().
associative(_Config) ->
  EmptyMap = #{},
  false    = clj_rt:'contains?'(EmptyMap, 1),

  OneMap = clj_rt:assoc(EmptyMap, 1, a),
  true   = clj_rt:'contains?'(OneMap, 1),
  false  = clj_rt:'contains?'(OneMap, 2),

  TwoMap = clj_rt:assoc(OneMap, 2, b),
  true   = clj_rt:'contains?'(TwoMap, 1),
  true   = clj_rt:'contains?'(TwoMap, 2),

  Pair   = clj_rt:find(TwoMap, 2),
  true   = clj_rt:equiv(Pair, clj_rt:vector([2, b])),

  ?NIL = clj_rt:find(TwoMap, 3),

  TwoMap = clj_rt:dissoc(TwoMap, 3),
  OneMap = clj_rt:dissoc(TwoMap, 2),

  {comments, ""}.

-spec to_clj(config()) -> result().
to_clj(_Config) ->
  Map1    = #{a => 1, b => 2},
  CljMap1 = clj_rt:'erl->clj'(Map1, true),
  true    = clj_rt:equiv(CljMap1, clj_rt:hash_map([a, 1, b, 2])),

  Map2    = #{{} => []},
  CljMap2 = clj_rt:'erl->clj'(Map2, false),
  true    = clj_rt:equiv(CljMap2, clj_rt:hash_map([{}, []])),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  NotEmptyMap = #{a => b, 2 => 3},
  EmptyMap    = clj_rt:empty(NotEmptyMap),
  EmptyMap    = #{},

  {comments, ""}.
