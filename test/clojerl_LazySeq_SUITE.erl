-module(clojerl_LazySeq_SUITE).

-export([all/0, init_per_suite/1]).

-export([ new/1
        , count/1
        , str/1
        , is_sequential/1
        , hash/1
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
  Regex = <<"#<clojerl.LazySeq@\\d+>">>,
  LazySeq3 = range(1, 3),
  {match, _} = re:run(clj_core:str(LazySeq3), Regex),

  LazySeq0 = range(1, 0),
  {match, _} = re:run(clj_core:str(LazySeq0), Regex),
  <<"()">> = clj_core:str(clj_core:rest(LazySeq0)),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  LazySeq = range(1, 3),
  true = clj_core:'sequential?'(LazySeq),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  LazySeq1 = range(1, 4),
  LazySeq2 = range(1, 3),
  LazySeq3 = range(1.0, 3.0),

  Hash1 = 'clojerl.IHash':hash(LazySeq1),
  Hash2 = 'clojerl.IHash':hash(LazySeq2),
  Hash3 = 'clojerl.IHash':hash(LazySeq3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  LazySeq = range(1, 3),
  1 = clj_core:first(LazySeq),
  2 = clj_core:first(clj_core:next(LazySeq)),
  3 = clj_core:first(clj_core:next(clj_core:next(LazySeq))),

  LazySeq1 = range(1, 1),
  1 = clj_core:first(LazySeq1),
  undefined = clj_core:next(LazySeq1),
  [] = clj_core:seq_to_list(clj_core:rest(LazySeq1)),
  undefined = clj_core:seq(clj_core:rest(LazySeq1)),

  LazySeq0  = range(1, 0),
  undefined = clj_core:first(LazySeq0),
  undefined = clj_core:next(LazySeq0),
  true      = undefined =/= clj_core:rest(LazySeq0),
  undefined = clj_core:seq(clj_core:rest(LazySeq0)),

  LazySeqBis = 'clojerl.LazySeq':new(fun() -> range(1, 3) end),
  1 = clj_core:first(LazySeqBis),
  2 = clj_core:first(clj_core:rest(LazySeqBis)),
  3 = clj_core:first(clj_core:next(clj_core:next(LazySeqBis))),
  [1, 2, 3] = clj_core:seq_to_list(clj_core:seq(LazySeqBis)),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that lazy seqs with the same elements are equivalent"),
  LazySeq1 = clj_core:with_meta(range(1, 3), #{a => 1}),
  LazySeq2 = clj_core:with_meta(range(1, 3), #{b => 2}),
  true = clj_core:equiv(LazySeq1, LazySeq2),

  ct:comment("Check that a lazy seq and a list with the "
             "same elements are equivalent"),
  true = clj_core:equiv(LazySeq1, [1, 2, 3]),

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
  undefined = 'clojerl.LazySeq':'_'(undefined),

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
