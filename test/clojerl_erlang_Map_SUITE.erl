-module(clojerl_erlang_Map_SUITE).

-export([all/0, init_per_suite/1]).

-export([ new/1
        , count/1
        , str/1
        , seq/1
        , equiv/1
        , invoke/1
        , cons/1
        , associative/1
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
  Map = #{1 => 2, 3 => 4},
  2 = clj_core:get(Map, 1),
  4 = clj_core:get(Map, 3),

  [1, 3] = lists:sort(clj_core:keys(Map)),
  [2, 4] = lists:sort(clj_core:vals(Map)),

  Map2 = #{},
  not_found = clj_core:get(Map2, 3, not_found),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Map = #{1 => 2, 3 => 4},
  2 = clj_core:count(Map),

  Map2 = #{},
  0 = clj_core:count(Map2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Map = #{1 => 2, 3 => 4},
  <<"{1 2, 3 4}">> = clj_core:str(Map),

  Map2 = clj_core:hash_map([]),
  <<"{}">> = clj_core:str(Map2),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Map = #{1 => 2, 3 => 4},

  KVs = lists:map(fun clj_core:seq/1, clj_core:seq(Map)),
  [[1, 2], [3, 4]] = lists:sort(KVs),

  Map2 = #{},
  undefined = clj_core:seq(Map2),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  Symbol = clj_core:symbol(<<"hello">>),

  ct:comment("Check that maps with the same elements are equivalent"),
  Map1 = #{1 => 2, Symbol => 4},
  Map2 = #{Symbol => 4, 1 => 2},
  true = clj_core:equiv(Map1, Map2),

  ct:comment("Check that maps with the same number of keys are not equivalent"),
  Map3 = #{5 => 6, 3 => 4},
  false = clj_core:equiv(Map1, Map3),

  ct:comment("Check that maps with different number of keys are not equivalent"),
  Map4 = #{5 => 6, 3 => 4, 7 => 8},
  false = clj_core:equiv(Map1, Map4),

  ct:comment("A clojerl.erlang.Map and an clojerl.Map"),
  true = clj_core:equiv(Map1, clj_core:hash_map([1, 2, Symbol, 4])),
  false = clj_core:equiv(Map1, clj_core:hash_map([1, 2])),
  false = clj_core:equiv(Map1, clj_core:hash_map([1, 2, 3, 4, 5, 6])),

  ct:comment("A clojerl.erlang.Map and something else"),
  false = clj_core:equiv(Map1, whatever),
  false = clj_core:equiv(Map1, 1),
  false = clj_core:equiv(Map1, [1]),

  {comments, ""}.

-spec invoke(config()) -> result().
invoke(_Config) ->
  Map = #{1 => a, 2 => b},

  ct:comment("Invoke a map"),
  a = clj_core:invoke(Map, [1]),
  b = clj_core:invoke(Map, [2]),
  undefined = clj_core:invoke(Map, [3]),

  ct:comment("Invoke a map with a not-found value"),
  c = clj_core:invoke(Map, [3, c]),

  ct:comment("Invoke a map with three arguments"),
  ok = try clj_core:invoke(Map, [1, 2, 3]), error
       catch _:_ -> ok end,

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  EmptyMap = #{},

  ct:comment("Conj a key-value pair to an empty map"),
  OneMap = clj_core:conj(EmptyMap, clj_core:vector([1, 2])),

  1    = clj_core:count(OneMap),
  true = clj_core:equiv(OneMap, #{1 => 2}),

  ct:comment("Conj a key-value pair to a map with one"),
  TwoMap = clj_core:conj(OneMap, clj_core:vector([3, 4])),

  2    = clj_core:count(TwoMap),
  true = clj_core:equiv(TwoMap, #{1 => 2, 3 => 4}),

  ct:comment("Conj another map to a map with one"),
  ThreeMap = clj_core:conj(TwoMap, #{5 => 6}),
  3    = clj_core:count(ThreeMap),
  true = clj_core:equiv(ThreeMap, #{1 => 2, 3 => 4, 5 => 6}),

  ct:comment("Conj something that is not a key-value pair to an empty map"),
  ok = try clj_core:conj(EmptyMap, clj_core:vector([1])), error
       catch _:_ -> ok end,

  {comments, ""}.

-spec associative(config()) -> result().
associative(_Config) ->
  EmptyMap = #{},
  false    = clj_core:'contains?'(EmptyMap, 1),

  OneMap = clj_core:assoc(EmptyMap, 1, a),
  true   = clj_core:'contains?'(OneMap, 1),
  false  = clj_core:'contains?'(OneMap, 2),

  TwoMap = clj_core:assoc(OneMap, 2, b),
  true   = clj_core:'contains?'(TwoMap, 1),
  true   = clj_core:'contains?'(TwoMap, 2),

  Pair   = clj_core:find(TwoMap, 2),
  true   = clj_core:equiv(Pair, clj_core:vector([2, b])),

  undefined = clj_core:find(TwoMap, 3),

  TwoMap = clj_core:dissoc(TwoMap, 3),
  OneMap = clj_core:dissoc(TwoMap, 2),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  NotEmptyMap = #{a => b, 2 => 3},
  EmptyMap    = clj_core:empty(NotEmptyMap),
  EmptyMap    = #{},

  {comments, ""}.
