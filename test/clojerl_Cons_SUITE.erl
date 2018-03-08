-module(clojerl_Cons_SUITE).

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
        , '->erl'/1
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
  Cons = range(1, 3),
  [1, 2, 3] = clj_rt:to_list(Cons),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Cons = range(1, 42),
  42 = clj_rt:count(Cons),

  Cons1 = range(1, 7),
  7 = clj_rt:count(Cons1),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Cons = range(1, 3),
  <<"(1 2 3)">> = clj_rt:str(Cons),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  Cons = range(1, 3),
  true = clj_rt:'sequential?'(Cons),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  Cons1 = range(1, 4),
  Cons2 = range(1, 3),
  Cons3 = range(1.0, 3.0),

  Hash1 = 'clojerl.IHash':hash(Cons1),
  Hash2 = 'clojerl.IHash':hash(Cons2),
  Hash3 = 'clojerl.IHash':hash(Cons3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Cons = range(1, 3),
  1 = clj_rt:first(Cons),
  2 = clj_rt:first(clj_rt:rest(Cons)),
  2 = clj_rt:first(clj_rt:next(Cons)),
  3 = clj_rt:first(clj_rt:rest(clj_rt:rest(Cons))),
  3 = clj_rt:first(clj_rt:next(clj_rt:next(Cons))),

  Cons1     = range(1, 1),
  1         = clj_rt:first(Cons1),
  ?NIL = clj_rt:next(Cons1),
  []        = clj_rt:rest(Cons1),

  [1, 2, 3] = clj_rt:to_list(Cons),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that lazy seqs with the same elements are equivalent"),
  Cons1 = clj_rt:with_meta(range(1, 3), #{a => 1}),
  Cons2 = clj_rt:with_meta(range(1, 3), #{b => 2}),
  true = clj_rt:equiv(Cons1, Cons2),

  ct:comment("Check that a lazy seq and a list with the "
             "same elements are equivalent"),
  true = clj_rt:equiv(Cons1, [1, 2, 3]),

  ct:comment("Check that lists with the same elements are not equivalent"),
  Cons3 = clj_rt:with_meta(range(1, 4), #{c => 3}),
  false = clj_rt:equiv(Cons1, Cons3),

  ct:comment("A clojerl.List and something else"),
  false = clj_rt:equiv(Cons1, whatever),
  false = clj_rt:equiv(Cons1, #{}),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  Cons = range(2, 2),

  ct:comment("Conj an element to a one element lazy seq"),
  TwoList = clj_rt:conj(Cons, 1),

  2    = clj_rt:count(TwoList),
  true = clj_rt:equiv(clj_rt:to_list(TwoList), [1, 2]),

  ct:comment("Conj an element to a list with one element"),
  ThreeList = clj_rt:conj(TwoList, 0),

  3    = clj_rt:count(ThreeList),
  true = clj_rt:equiv(clj_rt:to_list(ThreeList), [0, 1, 2]),

  {comments, ""}.

-spec reduce(config()) -> result().
reduce(_Config) ->
  PlusFun = fun
              ([]) -> 0;
              ([X, Y]) -> X + Y
            end,

  TenCons = range(1, 10),
  55 = 'clojerl.IReduce':reduce(TenCons, PlusFun),
  60 = 'clojerl.IReduce':reduce(TenCons, PlusFun, 5),

  PlusMaxFun = fun
                 ([]) -> 0;
                 ([X, Y]) when X < 10 -> X + Y;
                 ([X, _]) -> 'clojerl.Reduced':?CONSTRUCTOR(X)
            end,
  Reduced = 'clojerl.IReduce':reduce(TenCons, PlusMaxFun),
  10 = clj_rt:deref(Reduced),

  {comments, ""}.

-spec '->erl'(config()) -> result().
'->erl'(_Config) ->
  Cons1     = range(1, 3),
  [1, 2, 3] = clj_rt:'->erl'(Cons1, false),
  [1, 2, 3] = clj_rt:'->erl'(Cons1, true),

  Cons2          = clj_rt:cons(1, clj_rt:cons(Cons1, ?NIL)),
  [1, Cons1]     = clj_rt:'->erl'(Cons2, false),
  [1, [1, 2, 3]] = clj_rt:'->erl'(Cons2, true),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ?NIL = 'clojerl.Cons':'_'(?NIL),

  Cons = range(2, 2),
  []   = clj_rt:empty(Cons),

  Cons1     = clj_rt:with_meta(Cons, #{a => 1}),
  #{a := 1} = clj_rt:meta(Cons1),

  {comments, ""}.

-spec range(integer(), integer()) -> 'clojerl.LazySeq':type().
range(Start, End) when Start > End ->
  ?NIL;
range(Start, End) ->
  'clojerl.Cons':?CONSTRUCTOR(Start, range(Start + 1, End)).
