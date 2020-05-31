-module(clojerl_Cycle_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ new/1
        , count/1
        , str/1
        , is_sequential/1
        , hash/1
        , seq/1
        , equiv/1
        , cons/1
        , reduce/1
        , complete_coverage/1
        ]).

-clojure(true).

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
  Cycle = create_cycle([1, 2, 3]),
  Cycle = clj_rt:seq(Cycle),

  Empty = create_cycle(?NIL),
  ?NIL  = clj_rt:seq(Empty),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Cycle = create_cycle([1, 2, 3]),
  ok = try clj_rt:count(Cycle), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Cycle = create_cycle([1, 2, 3]),
  <<"#<clojerl.Cycle>">> = clj_rt:str(Cycle),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  Cycle = create_cycle([1, 2, 3]),
  true  = clj_rt:'sequential?'(Cycle),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  Cycle1 = create_cycle([1, 2, 3]),
  Cycle2 = create_cycle([1, 3, 2]),
  Cycle3 = create_cycle([1.0, 2, 3]),

  Hash1 = 'clojerl.IHash':hash(Cycle1),
  Hash2 = 'clojerl.IHash':hash(Cycle2),
  Hash3 = 'clojerl.IHash':hash(Cycle3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Cycle = create_cycle([1, 2, 3]),
  Next0  = clj_rt:next(Cycle),
  1 = clj_rt:first(Cycle),
  2 = clj_rt:first(Next0),
  2 = clj_rt:first(clj_rt:rest(Cycle)),

  Next1 = clj_rt:next(Next0),
  3 = clj_rt:first(Next1),

  Next2 = clj_rt:next(Next1),
  1 = clj_rt:first(Next2),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that lists with the same elements are equivalent"),
  Cycle1 = clj_rt:with_meta(create_cycle([1, 2, 3]), #{a => 1}),
  Cycle2 = clj_rt:with_meta(create_cycle([1, 2, 3]), #{b => 2}),

  true   = clj_rt:equiv(Cycle1, Cycle2),

  ct:comment("Check that cycles with different elements are not equivalent"),
  Cycle3 = clj_rt:with_meta(create_cycle([1, 2, 3, 4]), #{c => 3}),
  false = clj_rt:equiv(Cycle1, Cycle3),

  ct:comment("A clojerl.Cycle and a list"),
  false = clj_rt:equiv(Cycle1, [1, 2, 3]),
  false = clj_rt:equiv(Cycle1, [1, 2, 3, a]),

  ct:comment("A clojerl.Cycle and something else"),
  false = clj_rt:equiv(Cycle1, whatever),
  false = clj_rt:equiv(Cycle1, #{}),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  Cycle0 = create_cycle([1, 2, 3]),

  ct:comment("Conj an element to a cycle"),
  Cycle1 = clj_rt:conj(Cycle0, 1),

  ok = try clj_rt:count(Cycle1), error
       catch _:_ -> ok
       end,

  ct:comment("Conj another element"),
  Cycle2 = clj_rt:conj(Cycle1, 2),

  ok = try clj_rt:count(Cycle2), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec reduce(config()) -> result().
reduce(_Config) ->
  TenCycle   = create_cycle([1]),
  PlusMaxFun = fun
                 (X, Y) when X < 10 -> X + Y;
                 (X, _) -> 'clojerl.Reduced':?CONSTRUCTOR(X)
               end,
  10   = 'clojerl.IReduce':reduce(TenCycle, PlusMaxFun),
  10.5 = 'clojerl.IReduce':reduce(TenCycle, PlusMaxFun, 0.5),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  NotEmptyCycle = create_cycle([a, b, 2, 3]),
  EmptyCycle    = clj_rt:empty(NotEmptyCycle),
  true          = clj_rt:equiv(EmptyCycle, []),

  CycleMeta = clj_rt:with_meta(create_cycle([1, 2, 3]), #{a => 1}),
  #{a := 1} = clj_rt:meta(CycleMeta),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec create_cycle(any()) -> 'clojerl.Cycle':type().
create_cycle(Seq) ->
  'clojerl.Cycle':?CONSTRUCTOR(Seq).
