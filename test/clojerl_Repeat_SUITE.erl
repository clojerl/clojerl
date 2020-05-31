-module(clojerl_Repeat_SUITE).

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
  Repeat = create_repeat(3, foo),
  Repeat = clj_rt:seq(Repeat),

  Empty = create_repeat(0, foo),
  ?NIL  = clj_rt:seq(Empty),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Repeat1 = create_repeat(1),

  ok = try clj_rt:count(Repeat1), error
       catch _:_ -> ok
       end,

  Repeat2 = create_repeat(42, foo),
  42 = clj_rt:count(Repeat2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Repeat1 = create_repeat(foo),

  ok = try clj_rt:str(Repeat1), error
       catch _:_ -> ok
       end,

  Repeat2 = create_repeat(3, foo),
  <<"(:foo :foo :foo)">> = clj_rt:str(Repeat2),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  Repeat = create_repeat([1, 2, 3]),
  true  = clj_rt:'sequential?'(Repeat),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  Repeat1 = create_repeat(20, foo),
  Repeat2 = create_repeat(42, foo),
  Repeat3 = create_repeat(10, foo),

  Hash1 = 'clojerl.IHash':hash(Repeat1),
  Hash2 = 'clojerl.IHash':hash(Repeat2),
  Hash3 = 'clojerl.IHash':hash(Repeat3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Repeat = create_repeat(foo),
  Next0  = clj_rt:next(Repeat),
  foo = clj_rt:first(Repeat),
  foo = clj_rt:first(Next0),
  foo = clj_rt:first(clj_rt:rest(Repeat)),

  Next1 = clj_rt:next(Next0),
  foo = clj_rt:first(Next1),

  Next2 = clj_rt:next(Next1),
  foo = clj_rt:first(Next2),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that lists with the same elements are equivalent"),
  Repeat1 = clj_rt:with_meta(create_repeat(3, foo), #{a => 1}),
  Repeat2 = clj_rt:with_meta(create_repeat(3, foo), #{b => 2}),
  true    = clj_rt:equiv(Repeat1, Repeat2),

  ct:comment("Check that repeats with different elements are not equivalent"),
  Repeat3 = clj_rt:with_meta(create_repeat(bar), #{c => 3}),
  false = clj_rt:equiv(Repeat1, Repeat3),

  ct:comment("A clojerl.Repeat and a list"),
  true  = clj_rt:equiv(Repeat1, [foo, foo, foo]),
  false = clj_rt:equiv(Repeat1, [1, 2, 3, a]),

  ct:comment("A clojerl.Repeat and something else"),
  false = clj_rt:equiv(Repeat1, whatever),
  false = clj_rt:equiv(Repeat1, #{}),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  Repeat0 = create_repeat([1, 2, 3]),

  ct:comment("Conj an element to a repeat"),
  Repeat1 = clj_rt:conj(Repeat0, 1),

  ok = try clj_rt:count(Repeat1), error
       catch _:_ -> ok
       end,

  ct:comment("Conj another element"),
  Repeat2 = clj_rt:conj(Repeat1, 2),

  ok = try clj_rt:count(Repeat2), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec reduce(config()) -> result().
reduce(_Config) ->
  PlusFun0  = fun
                ([]) -> 0;
                ([X, Y]) -> X + Y
              end,
  PlusFun   = 'clojerl.Fn':?CONSTRUCTOR(PlusFun0),
  EmptyList = create_repeat(0, foo),

  0  = 'clojerl.IReduce':reduce(EmptyList, PlusFun),
  42 = 'clojerl.IReduce':reduce(EmptyList, PlusFun, 42),

  TenRepeat   = create_repeat(1),
  PlusMaxFun = fun
                 (X, Y) when X < 10 -> X + Y;
                 (X, _) -> 'clojerl.Reduced':?CONSTRUCTOR(X)
               end,
  10   = 'clojerl.IReduce':reduce(TenRepeat, PlusMaxFun),
  10.5 = 'clojerl.IReduce':reduce(TenRepeat, PlusMaxFun, 0.5),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  NotEmptyRepeat = create_repeat(4, foo),
  EmptyRepeat    = clj_rt:empty(NotEmptyRepeat),
  true           = clj_rt:equiv(EmptyRepeat, []),

  RepeatMeta = clj_rt:with_meta(create_repeat(foo), #{a => 1}),
  #{a := 1} = clj_rt:meta(RepeatMeta),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec create_repeat(any()) -> 'clojerl.Repeat':type().
create_repeat(Value) ->
  'clojerl.Repeat':?CONSTRUCTOR(Value).

-spec create_repeat(non_neg_integer(), any()) -> 'clojerl.Repeat':type().
create_repeat(Count, Value) ->
  'clojerl.Repeat':?CONSTRUCTOR(Count, Value).
