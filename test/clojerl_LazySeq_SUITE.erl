-module(clojerl_LazySeq_SUITE).

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
        , to_erl/1
        , infinite_seq/1
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
  LazySeq3 = range(1, 3),
  [1, 2, 3] = clj_rt:to_list(LazySeq3),

  LazySeq0 = range(1, 0),
  ?NIL = clj_rt:seq(LazySeq0),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  LazySeq42 = range(1, 42),
  42 = clj_rt:count(LazySeq42),

  LazySeq0 = range(1, 0),
  0 = clj_rt:count(LazySeq0),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Regex = <<"#<clojerl.LazySeq@\\d+>">>,
  LazySeq3 = range(1, 3),
  {match, _} = re:run(clj_rt:str(LazySeq3), Regex),

  LazySeq0     = range(1, 0),
  {match, _}   = re:run(clj_rt:str(LazySeq0), Regex),
  <<"">> = clj_rt:str(clj_rt:rest(LazySeq0)),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  LazySeq = range(1, 3),
  true = clj_rt:'sequential?'(LazySeq),

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
  1 = clj_rt:first(LazySeq),
  2 = clj_rt:first(clj_rt:next(LazySeq)),
  3 = clj_rt:first(clj_rt:next(clj_rt:next(LazySeq))),

  LazySeq1 = range(1, 1),
  1 = clj_rt:first(LazySeq1),
  ?NIL = clj_rt:next(LazySeq1),
  [] = clj_rt:to_list(clj_rt:rest(LazySeq1)),
  ?NIL = clj_rt:seq(clj_rt:rest(LazySeq1)),

  LazySeq0  = range(1, 0),
  ?NIL = clj_rt:first(LazySeq0),
  ?NIL = clj_rt:next(LazySeq0),
  true = ?NIL =/= clj_rt:rest(LazySeq0),
  ?NIL = clj_rt:seq(clj_rt:rest(LazySeq0)),

  LazySeqBis = 'clojerl.LazySeq':?CONSTRUCTOR(fun([]) -> range(1, 3) end),
  1 = clj_rt:first(LazySeqBis),
  2 = clj_rt:first(clj_rt:rest(LazySeqBis)),
  3 = clj_rt:first(clj_rt:next(clj_rt:next(LazySeqBis))),
  [1, 2, 3] = clj_rt:to_list(clj_rt:seq(LazySeqBis)),

  LazySeqEmpty = 'clojerl.LazySeq':?CONSTRUCTOR(fun([]) -> [] end),
  [] = clj_rt:to_list(LazySeqEmpty),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that lazy seqs with the same elements are equivalent"),
  LazySeq1 = clj_rt:with_meta(range(1, 3), #{a => 1}),
  LazySeq2 = clj_rt:with_meta(range(1, 3), #{b => 2}),
  true = clj_rt:equiv(LazySeq1, LazySeq2),

  ct:comment("Check that a lazy seq and a list with the "
             "same elements are equivalent"),
  true = clj_rt:equiv(LazySeq1, [1, 2, 3]),

  ct:comment("Check that lists with the same elements are not equivalent"),
  LazySeq3 = clj_rt:with_meta(range(1, 4), #{c => 3}),
  false = clj_rt:equiv(LazySeq1, LazySeq3),

  ct:comment("A clojerl.List and something else"),
  false = clj_rt:equiv(LazySeq1, whatever),
  false = clj_rt:equiv(LazySeq1, #{}),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  LazySeq = range(2, 2),

  ct:comment("Conj an element to a one element lazy seq"),
  TwoList = clj_rt:conj(LazySeq, 1),

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
  EmptyLazySeq = range(1, 0),

  0  = 'clojerl.IReduce':reduce(EmptyLazySeq, PlusFun),
  42 = 'clojerl.IReduce':reduce(EmptyLazySeq, PlusFun, 42),

  TenLazySeq = range(1, 10),
  55 = 'clojerl.IReduce':reduce(TenLazySeq, PlusFun),
  60 = 'clojerl.IReduce':reduce(TenLazySeq, PlusFun, 5),

  PlusMaxFun = fun
                 ([]) -> 0;
                 ([X, Y]) when X < 10 -> X + Y;
                 ([X, _]) -> 'clojerl.Reduced':?CONSTRUCTOR(X)
            end,
  10 = 'clojerl.IReduce':reduce(TenLazySeq, PlusMaxFun),

  {comments, ""}.

-spec to_erl(config()) -> result().
to_erl(_Config) ->
  LazySeq1  = range(1, 3),
  [1, 2, 3] = clj_rt:'->erl'(LazySeq1, false),
  [1, 2, 3] = clj_rt:'->erl'(LazySeq1, true),

  LazySeq2       = 'clojerl.LazySeq':?CONSTRUCTOR(fun([]) -> [1, LazySeq1] end),
  [1, LazySeq1]  = clj_rt:'->erl'(LazySeq2, false),
  [1, [1, 2, 3]] = clj_rt:'->erl'(LazySeq2, true),

  {comments, ""}.

-spec infinite_seq(config()) -> result().
infinite_seq(_Config) ->
  LazySeq = range(0, infinity),
  0   = clj_rt:nth(LazySeq, 0),
  42  = clj_rt:nth(LazySeq, 42),

  0   = clj_rt:nth(LazySeq, 0, foo),
  42  = clj_rt:nth(LazySeq, 42, foo),
  foo = clj_rt:nth(LazySeq, -1, foo),

  ct:comment("Negative index generates error"),
  ok  = try clj_rt:nth(LazySeq, -1), error
        catch _:_ -> ok end,

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ?NIL = 'clojerl.LazySeq':'_'(?NIL),

  LazySeq   = range(2, 2),
  EmptyList = clj_rt:empty(LazySeq),
  EmptyList = [],

  LazySeqMeta = clj_rt:with_meta(LazySeq, #{a => 1}),
  #{a := 1}   = clj_rt:meta(LazySeqMeta),

  {comments, ""}.

-spec range(integer(), integer()) -> 'clojerl.LazySeq':type().
range(Start, End) ->
  Fun = fun
          ([]) when Start =< End; End == infinity ->
            'clojerl.Cons':?CONSTRUCTOR(Start, range(Start + 1, End));
          ([]) when Start > End ->
            ?NIL
        end,
  'clojerl.LazySeq':?CONSTRUCTOR(Fun).
