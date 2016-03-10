-module(clojerl_LazySeq_SUITE).

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
  LazySeq3 = range(1, 3),
  [1, 2, 3] = clj_core:seq_to_list(LazySeq3),

  LazySeq0 = range(1, 0),
  undefined = clj_core:seq(LazySeq0),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  LazySeq42 = range(1, 42),
  42 = clj_core:count(LazySeq42),

  LazySeq0 = range(1, 0),
  0 = clj_core:count(LazySeq0),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  LazySeq3 = range(1, 3),
  Str3 = clj_core:str(LazySeq3),
  {match, _} = re:run(Str3, <<"#<clojerl.LazySeq@\\d+>">>),

  LazySeq0 = range(1, 0),
  <<>> = clj_core:str(LazySeq0),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  LazySeq = range(1, 3),
  true = clj_core:'sequential?'(LazySeq),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  LazySeq3 = range(1, 3),
  1 = clj_core:first(LazySeq3),
  2 = clj_core:first(clj_core:next(LazySeq3)),
  3 = clj_core:first(clj_core:next(clj_core:next(LazySeq3))),

  LazySeq1 = range(1, 1),
  1 = clj_core:first(LazySeq1),
  undefined = clj_core:next(LazySeq1),
  [] = clj_core:seq2(clj_core:rest(LazySeq1)),

  LazySeq0 = range(1, 0),
  undefined = clj_core:first(LazySeq0),
  undefined = clj_core:next(LazySeq0),
  undefined = clj_core:rest(LazySeq0),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that lists with the same elements are equivalent"),
  LazySeq1 = clj_core:with_meta(range(1, 3), #{a => 1}),
  LazySeq2 = clj_core:with_meta(range(1, 3), #{b => 2}),

  true = clj_core:equiv(LazySeq1, LazySeq2),

  ct:comment("Check that lists with the same elements are not equivalent"),
  LazySeq3 = clj_core:with_meta(range(1, 4), #{c => 3}),
  false = clj_core:equiv(LazySeq1, LazySeq3),

  ct:comment("A clojerl.List and something else"),
  false = clj_core:equiv(LazySeq1, whatever),
  false = clj_core:equiv(LazySeq1, #{}),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  LazySeq = range(2, 2),

  ct:comment("Conj an element to a one element lazy seq"),
  TwoList = clj_core:conj(LazySeq, 1),

  2    = clj_core:count(TwoList),
  true = clj_core:equiv(clj_core:seq_to_list(TwoList), [1, 2]),

  ct:comment("Conj an element to a list with one element"),
  ThreeList = clj_core:conj(TwoList, 0),

  3    = clj_core:count(ThreeList),
  true = clj_core:equiv(clj_core:seq_to_list(ThreeList), [0, 1, 2]),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ok = 'clojerl.LazySeq':'clojerl.ISequential.noop'(ok),

  LazySeq   = range(2, 2),
  EmptyList = clj_core:empty(LazySeq),
  EmptyList = [],

  LazySeqMeta = clj_core:with_meta(LazySeq, #{a => 1}),
  #{a := 1}   = clj_core:meta(LazySeqMeta),

  {comments, ""}.

-spec range(integer(), integer()) -> 'clojerl.LazySeq':type().
range(Start, End) ->
  Fun = fun
          () when Start =< End ->
            'clojerl.Cons':new(Start, range(Start + 1, End));
          () when Start > End ->
            undefined
        end,
  'clojerl.LazySeq':new(Fun).
