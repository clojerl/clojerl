-module(clojerl_Vector_Seq_SUITE).

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
        , reduce/1
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
  Seq1 = create_seq(64),
  64   = clj_rt:count(Seq1),
  Seq2 = create_seq(0),
  0    = clj_rt:count(Seq2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Seq = create_seq(33),
  <<"(0 1 2 3 4 5 6 7 8 9"
    " 10 11 12 13 14 15 16 17 18 19"
    " 20 21 22 23 24 25 26 27 28 29"
    " 30 31 32)">> = clj_rt:str(Seq),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  Seq = create_seq(64),
  true = clj_rt:'sequential?'(Seq),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  Seq1 = create_seq(64),
  Seq2 = clj_rt:with_meta(create_seq(64), #{a => 1}),
  Seq3 = create_seq(128),

  Hash1 = 'clojerl.IHash':hash(Seq1),
  Hash1 = 'clojerl.IHash':hash(Seq2),
  Hash3 = 'clojerl.IHash':hash(Seq3),

  true = Hash1 =/= Hash3,

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Seq1     = create_seq(64),

  Next1    = clj_rt:next(Seq1),
  Next1    = clj_rt:rest(Seq1),
  0        = clj_rt:first(Seq1),

  1        = clj_rt:first(Next1),
  63       = clj_rt:count(Next1),

  EmptyList = clj_rt:empty(Seq1),
  true      = clj_rt:'seq?'(EmptyList),
  0         = clj_rt:count(EmptyList),

  SeqOne    = create_seq(1),
  1         = clj_rt:count(SeqOne),
  EmptySeq  = clj_rt:next(SeqOne),
  EmptyList = clj_rt:rest(SeqOne),
  ?NIL      = clj_rt:first(EmptySeq),

  List1     = clj_rt:rest(clj_rt:rest(EmptySeq)),
  ?NIL      = clj_rt:seq(List1),

  Empty     = create_seq(0),
  ?NIL      = clj_rt:seq(Empty),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  Seq1 = create_seq(64),
  Seq1 = clj_rt:seq(Seq1),
  Seq2 = clj_rt:with_meta(Seq1, #{a => 1}),
  Seq3 = create_seq(33),

  true  = clj_rt:equiv(Seq1, Seq2),
  true  = clj_rt:equiv(Seq1, lists:seq(0, 63)),
  false = clj_rt:equiv(Seq1, Seq3),
  false = clj_rt:equiv(Seq1, clj_rt:symbol(<<"foo">>)),

  Seq4    = create_seq(64),
  NextFun = fun(_, Acc) -> clj_rt:next(Acc) end,
  Seq5    = lists:foldl(NextFun, Seq4, lists:seq(1, 64)),
  ?NIL    = clj_rt:seq(Seq5),

  RestFun   = fun(_, Acc) -> clj_rt:next(Acc) end,
  Seq6      = lists:foldl(RestFun, Seq4, lists:seq(1, 63)),
  ?NIL      = clj_rt:next(Seq6),
  EmptyList = clj_rt:rest(Seq6),
  0         = clj_rt:count(EmptyList),
  true      = ?NIL =/= EmptyList,

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  Seq1 = create_seq(64),
  Cons = clj_rt:conj(Seq1, -1),
  65   = clj_rt:count(Cons),
  true = clj_rt:equiv(Cons, [-1 | lists:seq(0, 63)]),

  {comments, ""}.

-spec reduce(config()) -> result().
reduce(_Config) ->
  Seq1 = create_seq(10),

  Fun1 = fun(X, Y) -> X + Y end,
  'clojerl.IReduce':reduce(Seq1, Fun1, 0),

  Fun2 = fun(X, Y) -> X + Y end,
  'clojerl.IReduce':reduce(Seq1, Fun2),

  {comments, ""}.

-spec to_erl(config()) -> result().
to_erl(_Config) ->
  List1 = lists:seq(0, 63),
  Seq1  = create_seq(64),
  List1 = clj_rt:'clj->erl'(Seq1, false),
  List1 = clj_rt:'clj->erl'(Seq1, true),

  Vector = 'clojerl.Vector':?CONSTRUCTOR([1 | List1]),
  Seq2   = 'clojerl.Vector.Seq':?CONSTRUCTOR(Vector),
  List2 = [1 | List1],
  List2 = clj_rt:'clj->erl'(Seq2, false),
  List2 = clj_rt:'clj->erl'(Seq2, true),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  SeqMeta  = clj_rt:with_meta(create_seq(64), #{a => 1}),
  #{a := 1} = clj_rt:meta(SeqMeta),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec create_seq(integer()) -> 'clojerl.Vector.Seq':type().
create_seq(Num) ->
  Vector = clj_rt:vector(lists:seq(0, Num - 1)),
  'clojerl.Vector.Seq':?CONSTRUCTOR(Vector).
