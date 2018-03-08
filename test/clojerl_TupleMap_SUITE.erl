-module(clojerl_TupleMap_SUITE).

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
  Map = tuple_map([1, 2, 3, 4]),
  2 = clj_rt:get(Map, 1),
  4 = clj_rt:get(Map, 3),
  ?NIL = clj_rt:get(Map, 5),

  [1, 3] = lists:sort(clj_rt:keys(Map)),
  [2, 4] = lists:sort(clj_rt:vals(Map)),

  Map2 = tuple_map([]),
  ?NIL = clj_rt:seq(Map2),

  ct:comment("Create while checking for duplicate keys"),
  Items1 = [a, 1, b, 2],
  Map3   = tuple_map(Items1),
  Map3   = 'clojerl.TupleMap':create_with_check(Items1),

  ok = try 'clojerl.TupleMap':create_with_check([a, 1, b, 2, a, 2]), error
       catch error:Error ->
           Msg = clj_rt:str(Error),
           {match, _} = re:run(Msg, "Duplicate key"),
           ok
       end,

  ct:comment("Create as if key-value pairs were added with assoc"),
  Items2 = [a, 1, b, 2],
  Map4   = tuple_map(Items2),
  Map4   = 'clojerl.TupleMap':create_with_assoc([a, 2 | Items2]),

  ct:comment("The first value for the key is kept"),
  HelloSym1 = clj_rt:with_meta(clj_rt:symbol(<<"hello">>), #{a => 1}),
  HelloSym2 = clj_rt:with_meta(clj_rt:symbol(<<"hello">>), #{b => 2}),
  Items3    = [HelloSym1, 2, b, 2],
  Map5      = tuple_map(Items3),
  Items4    = [HelloSym1, 1, b, 2, HelloSym2, 2],
  Map5      = 'clojerl.TupleMap':create_with_assoc(Items4),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Map = tuple_map([1, 2, 3, 4]),
  2 = clj_rt:count(Map),

  Map2 = tuple_map([]),
  0 = clj_rt:count(Map2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Map = tuple_map([1, 2, 3, 4]),
  <<"{1 2, 3 4}">> = clj_rt:str(Map),

  Map2 = tuple_map([]),
  <<"{}">> = clj_rt:str(Map2),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Map = tuple_map([1, 2, 3, 4]),

  KVs  = lists:map(fun clj_rt:seq/1, clj_rt:seq(Map)),
  true = clj_rt:equiv([[1, 2], [3, 4]], lists:sort(KVs)),

  Map2 = tuple_map([]),
  ?NIL = clj_rt:seq(Map2),

  MapList = clj_rt:to_list(Map),
  true = clj_rt:equiv([[1, 2], [3, 4]], lists:sort(MapList)),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Maps with the same elements are equivalent"),
  Map1 = clj_rt:with_meta(tuple_map([1.0, 2, 3, 4]), #{a => 1}),
  Map2 = clj_rt:with_meta(tuple_map([3, 4, 1.0, 2]), #{b => 2}),
  true = clj_rt:equiv(Map1, Map2),

  ct:comment("Maps with different elements are not equivalent"),
  Map3 = clj_rt:with_meta(tuple_map([5, 6, 3, 4]), #{c => 3}),
  false = clj_rt:equiv(Map1, Map3),

  ct:comment("Maps with same keys different values are not equivalent"),
  Map4 = clj_rt:with_meta(tuple_map([1.0, a, 3, b]), #{a => 1}),
  false = clj_rt:equiv(Map1, Map4),

  ct:comment("A clojerl.TupleMap and an erlang.Map"),
  true  = clj_rt:equiv(Map1, #{1.0 => 2, 3 => 4}),
  false = clj_rt:equiv(Map1, #{1.0 => 2}),
  false = clj_rt:equiv(Map1, #{1.0 => 2, 3 => 4, 5 => 6}),
  false = clj_rt:equiv(Map1, #{1.0 => a, 3 => b}),

  ct:comment("A clojerl.TupleMap and something else"),
  false = clj_rt:equiv(Map1, whatever),
  false = clj_rt:equiv(Map1, 1),
  false = clj_rt:equiv(Map1, [1]),

  {comments, ""}.

-spec apply(config()) -> result().
apply(_Config) ->
  Map = tuple_map([1, a, 2, b]),

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
  Map1a = tuple_map([1, a, 2, b]),
  Map1b = tuple_map([2, b, 1, a]),
  Map2  = tuple_map([2, b, 1, a, 3, c]),
  Map3  = tuple_map([2.0, b, 1.0, a, 3.0, c]),

  Hash1 = 'clojerl.IHash':hash(Map1a),
  Hash1 = 'clojerl.IHash':hash(Map1b),
  Hash2 = 'clojerl.IHash':hash(Map2),
  Hash3 = 'clojerl.IHash':hash(Map3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  EmptyMap = tuple_map([]),

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
  EmptyMap = tuple_map([]),
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

  ct:comment("Associng past the threshold returns a hash map"),
  LimitMap      = tuple_map(lists:seq(1, 16 * 2)),
  PastLimitMap  = clj_rt:assoc(LimitMap, 33, 34),
  'clojerl.Map' = clj_rt:type_module(PastLimitMap),

  {comments, ""}.

-spec to_erl(config()) -> result().
to_erl(_Config) ->
  Value     = #{1 => 2},

  Map1     = tuple_map([1, 2]),
  Value    = clj_rt:'->erl'(Map1, false),
  Value    = clj_rt:'->erl'(Map1, true),

  Map2     = tuple_map([Map1, Map1]),

  Map2Erl1 = clj_rt:'->erl'(Map2, false),
  Map1     = maps:get(Map1, Map2Erl1),

  Map2Erl2 = clj_rt:'->erl'(Map2, true),
  Value    = maps:get(#{1 => 2}, Map2Erl2),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  NotEmptyMap = tuple_map([a, b, 2, 3]),
  EmptyMap    = clj_rt:empty(NotEmptyMap),
  EmptyMap    = tuple_map([]),

  MapMeta  = clj_rt:with_meta(tuple_map([1, 2, 3, 4]), #{a => 1}),
  #{a := 1} = clj_rt:meta(MapMeta),

  Hash1 = 'clojerl.IHash':hash(NotEmptyMap),
  Hash2 = 'clojerl.IHash':hash(EmptyMap),

  true = Hash1 =/= Hash2,

  Hash1 = 'clojerl.IHash':hash(NotEmptyMap),

  #{a := b, 2 := 3} = 'clojerl.TupleMap':'->erl'(NotEmptyMap, false),

  {comments, ""}.

-spec tuple_map(list()) -> 'clojerl.TupleMap':type().
tuple_map(KeyValues) ->
  'clojerl.TupleMap':?CONSTRUCTOR(KeyValues).
