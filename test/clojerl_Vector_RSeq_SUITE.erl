-module(clojerl_Vector_RSeq_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ count/1
        , str/1
        , is_sequential/1
        , hash/1
        , seq/1
        , equiv/1
        , cons/1
        , to_erl/1
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

-spec count(config()) -> result().
count(_Config) ->
  RSeq1 = rseq(64),
  64 = clj_rt:count(RSeq1),
  RSeq2 = rseq(0),
  0  = clj_rt:count(RSeq2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  RSeq = rseq(33),
  <<"(32 31 30 29 28 27 26 25"
    " 24 23 22 21 20 19 18 17"
    " 16 15 14 13 12 11 10 9"
    " 8 7 6 5 4 3 2 1 0)">> = clj_rt:str(RSeq),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  RSeq = rseq(64),
  true = clj_rt:'sequential?'(RSeq),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  RSeq1 = rseq(64),
  RSeq2 = clj_rt:with_meta(rseq(64), #{a => 1}),
  RSeq3 = rseq(128),

  Hash1 = 'clojerl.IHash':hash(RSeq1),
  Hash1 = 'clojerl.IHash':hash(RSeq2),
  Hash3 = 'clojerl.IHash':hash(RSeq3),

  true = Hash1 =/= Hash3,

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  RSeq1     = rseq(64),

  RNext1    = clj_rt:next(RSeq1),
  RNext1    = clj_rt:rest(RSeq1),
  63        = clj_rt:first(RSeq1),

  62        = clj_rt:first(RNext1),
  63        = clj_rt:count(RNext1),

  EmptyList = clj_rt:empty(RSeq1),
  true      = clj_rt:'seq?'(EmptyList),
  0         = clj_rt:count(EmptyList),

  RSeqOne   = rseq(1),
  1         = clj_rt:count(RSeqOne),
  EmptyRSeq = clj_rt:next(RSeqOne),
  EmptyList = clj_rt:rest(RSeqOne),
  ?NIL      = clj_rt:first(EmptyRSeq),

  List1     = clj_rt:rest(clj_rt:rest(EmptyRSeq)),
  ?NIL      = clj_rt:seq(List1),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  RSeq1 = rseq(64),
  RSeq1 = clj_rt:seq(RSeq1),
  RSeq2 = clj_rt:with_meta(RSeq1, #{a => 1}),
  RSeq3 = rseq(33),

  true  = clj_rt:equiv(RSeq1, RSeq2),
  true  = clj_rt:equiv(RSeq1, lists:reverse(lists:seq(0, 63))),
  false = clj_rt:equiv(RSeq1, RSeq3),
  false = clj_rt:equiv(RSeq1, clj_rt:symbol(<<"foo">>)),

  RSeq4     = rseq(64),
  NextFun   = fun(_, Acc) -> clj_rt:next(Acc) end,
  RSeq5     = lists:foldl(NextFun, RSeq4, lists:seq(1, 64)),
  ?NIL      = clj_rt:seq(RSeq5),

  RestFun   = fun(_, Acc) -> clj_rt:next(Acc) end,
  RSeq6     = lists:foldl(RestFun, RSeq4, lists:seq(1, 63)),
  ?NIL      = clj_rt:next(RSeq6),
  EmptyList = clj_rt:rest(RSeq6),
  0         = clj_rt:count(EmptyList),
  true      = ?NIL =/= EmptyList,

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  RSeq1 = rseq(64),
  Cons = clj_rt:conj(RSeq1, -1),
  65   = clj_rt:count(Cons),
  true = clj_rt:equiv(Cons, [-1 | lists:seq(63, 0, -1)]),

  {comments, ""}.

-spec to_erl(config()) -> result().
to_erl(_Config) ->
  List1 = lists:reverse(lists:seq(0, 63)),
  RSeq1 = rseq(64),
  List1 = clj_rt:'->erl'(RSeq1, false),
  List1 = clj_rt:'->erl'(RSeq1, true),

  Array = clj_vector:new([1, RSeq1]),
  RSeq2 = 'clojerl.Vector.RSeq':?CONSTRUCTOR(Array, 1),
  [RSeq1, 1] = clj_rt:'->erl'(RSeq2, false),
  [List1, 1] = clj_rt:'->erl'(RSeq2, true),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  RSeqMeta  = clj_rt:with_meta(rseq(64), #{a => 1}),
  #{a := 1} = clj_rt:meta(RSeqMeta),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec rseq(integer()) -> 'clojerl.Vector.RSeq':type().
rseq(Num) ->
  Vector = clj_rt:vector(lists:seq(0, Num - 1)),
  'clojerl.Vector':rseq(Vector).
