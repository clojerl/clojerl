-module(clojerl_Cons_SUITE).

-export([all/0, init_per_suite/1]).

-export([ new/1
        , count/1
        , str/1
        , is_sequential/1
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
  Cons = range(1, 3),
  [1, 2, 3] = clj_core:seq_to_list(Cons),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Cons = range(1, 42),
  42 = clj_core:count(Cons),

  Cons1 = range(1, 7),
  7 = clj_core:count(Cons1),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Cons = range(1, 3),
  <<"(1 2 3)">> = clj_core:str(Cons),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  Cons = range(1, 3),
  true = clj_core:'sequential?'(Cons),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Cons = range(1, 3),
  1 = clj_core:first(Cons),
  2 = clj_core:first(clj_core:next(Cons)),
  3 = clj_core:first(clj_core:next(clj_core:next(Cons))),

  Cons1     = range(1, 1),
  1         = clj_core:first(Cons1),
  undefined = clj_core:next(Cons1),
  []        = clj_core:rest(Cons1),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that lazy seqs with the same elements are equivalent"),
  Cons1 = clj_core:with_meta(range(1, 3), #{a => 1}),
  Cons2 = clj_core:with_meta(range(1, 3), #{b => 2}),
  true = clj_core:equiv(Cons1, Cons2),

  ct:comment("Check that a lazy seq and a list with the "
             "same elements are equivalent"),
  true = clj_core:equiv(Cons1, [1, 2, 3]),

  ct:comment("Check that lists with the same elements are not equivalent"),
  Cons3 = clj_core:with_meta(range(1, 4), #{c => 3}),
  false = clj_core:equiv(Cons1, Cons3),

  ct:comment("A clojerl.List and something else"),
  false = clj_core:equiv(Cons1, whatever),
  false = clj_core:equiv(Cons1, #{}),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  Cons = range(2, 2),

  ct:comment("Conj an element to a one element lazy seq"),
  TwoList = clj_core:conj(Cons, 1),

  2    = clj_core:count(TwoList),
  true = clj_core:equiv(clj_core:seq_to_list(TwoList), [1, 2]),

  ct:comment("Conj an element to a list with one element"),
  ThreeList = clj_core:conj(TwoList, 0),

  3    = clj_core:count(ThreeList),
  true = clj_core:equiv(clj_core:seq_to_list(ThreeList), [0, 1, 2]),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ok = 'clojerl.Cons':'clojerl.ISequential.noop'(ok),

  Cons = range(2, 2),
  []   = clj_core:empty(Cons),

  Cons1     = clj_core:with_meta(Cons, #{a => 1}),
  #{a := 1} = clj_core:meta(Cons1),

  {comments, ""}.

-spec range(integer(), integer()) -> 'clojerl.LazySeq':type().
range(Start, End) when Start > End ->
  undefined;
range(Start, End) ->
  'clojerl.Cons':new(Start, range(Start + 1, End)).
