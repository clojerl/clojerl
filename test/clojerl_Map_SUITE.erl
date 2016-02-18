-module(clojerl_Map_SUITE).

-export([all/0, init_per_suite/1]).

-export([ new/1
        , count/1
        , str/1
        , seq/1
        , equiv/1
        , cons/1
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
  Map = clj_core:hash_map([1, 2, 3, 4]),
  2 = clj_core:get(Map, 1),
  4 = clj_core:get(Map, 3),

  [1, 3] = lists:sort(clj_core:keys(Map)),
  [2, 4] = lists:sort(clj_core:vals(Map)),

  Map2 = clj_core:hash_map([]),
  undefined = clj_core:seq(Map2),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Map = clj_core:hash_map([1, 2, 3, 4]),
  2 = clj_core:count(Map),

  Map2 = clj_core:hash_map([]),
  0 = clj_core:count(Map2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Map = clj_core:hash_map([1, 2, 3, 4]),
  <<"{1 2, 3 4}">> = clj_core:str(Map),

  Map2 = clj_core:hash_map([]),
  <<"{}">> = clj_core:str(Map2),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Map = clj_core:hash_map([1, 2, 3, 4]),

  KVs = lists:map(fun clj_core:seq/1, clj_core:seq(Map)),
  [[1, 2], [3, 4]] = lists:sort(KVs),

  Map2 = clj_core:hash_map([]),
  undefined = clj_core:seq(Map2),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that maps with the same elements are equivalent"),
  Map1 = clj_core:with_meta(clj_core:hash_map([1, 2, 3, 4]), #{a => 1}),
  Map2 = clj_core:with_meta(clj_core:hash_map([3, 4, 1, 2]), #{b => 2}),
  true = clj_core:equiv(Map1, Map2),

  ct:comment("Check that maps with the same elements are not equivalent"),
  Map3 = clj_core:with_meta(clj_core:hash_map([5, 6, 3, 4]), #{c => 3}),
  false = clj_core:equiv(Map1, Map3),

  ct:comment("A clojerl.Map and an clojerl.erlang.Map"),
  true = clj_core:equiv(Map1, #{1 => 2, 3 => 4}),
  false = clj_core:equiv(Map1, #{1 => 2}),
  false = clj_core:equiv(Map1, #{1 => 2, 3 => 4, 5 => 6}),

  ct:comment("A clojerl.Map and something else"),
  false = clj_core:equiv(Map1, whatever),
  false = clj_core:equiv(Map1, 1),
  false = clj_core:equiv(Map1, [1]),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  EmptyMap = clj_core:hash_map([]),

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

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  NotEmptyMap = clj_core:hash_map([a, b, 2, 3]),
  EmptyMap    = clj_core:empty(NotEmptyMap),
  EmptyMap    = clj_core:hash_map([]),

  MapMeta  = clj_core:with_meta(clj_core:hash_map([1, 2, 3, 4]), #{a => 1}),
  #{a := 1} = clj_core:meta(MapMeta),

  {comments, ""}.
