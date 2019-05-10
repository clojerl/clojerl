-module(clojerl_Vector_ChunkedSeq_SUITE).

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
        , chunked/1
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
  ChunkedSeq1 = chunked_seq(64),
  64 = clj_rt:count(ChunkedSeq1),
  ChunkedSeq2 = chunked_seq(0),
  0  = clj_rt:count(ChunkedSeq2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  ChunkedSeq = chunked_seq(33),
  <<"(0 1 2 3 4 5 6 7 8 9 "
    "10 11 12 13 14 15 16 17 18 19 "
    "20 21 22 23 24 25 26 27 28 29 "
    "30 31 32)">> = clj_rt:str(ChunkedSeq),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  ChunkedSeq = chunked_seq(64),
  true       = clj_rt:'sequential?'(ChunkedSeq),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  ChunkedSeq1 = chunked_seq(64),
  ChunkedSeq2 = clj_rt:with_meta(chunked_seq(64), #{a => 1}),
  ChunkedSeq3 = chunked_seq(128),

  Hash1 = 'clojerl.IHash':hash(ChunkedSeq1),
  Hash1 = 'clojerl.IHash':hash(ChunkedSeq2),
  Hash3 = 'clojerl.IHash':hash(ChunkedSeq3),

  true = Hash1 =/= Hash3,

  {comments, ""}.

-spec chunked(config()) -> result().
chunked(_Config) ->
  ChunkedSeq1 = chunked_seq(33),

  Chunk = 'clojerl.IChunkedSeq':chunked_first(ChunkedSeq1),
  'clojerl.TupleChunk' = clj_rt:type_module(Chunk),
  32 = clj_rt:count(Chunk),

  ChunkedNext1 = 'clojerl.IChunkedSeq':chunked_next(ChunkedSeq1),
  ChunkedNext1 = 'clojerl.IChunkedSeq':chunked_more(ChunkedSeq1),
  1    = clj_rt:count(ChunkedNext1),
  32   = clj_rt:first(ChunkedNext1),
  ?NIL = 'clojerl.IChunkedSeq':chunked_next(ChunkedNext1),
  EmptyList = 'clojerl.IChunkedSeq':chunked_more(ChunkedNext1),
  true = clj_rt:'list?'(EmptyList),
  0    = clj_rt:count(EmptyList),

  ChunkedSeq2 = chunked_seq(32),
  true = clj_rt:type_module(ChunkedSeq2) =/= 'clojerl.Vector.ChunkedSeq',

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  ChunkedSeq1 = chunked_seq(64),

  ChunkedNext1 = clj_rt:next(ChunkedSeq1),
  ChunkedNext1 = clj_rt:rest(ChunkedSeq1),
  0  = clj_rt:first(ChunkedSeq1),

  1  = clj_rt:first(ChunkedNext1),
  63 = clj_rt:count(ChunkedNext1),

  EmptyList = clj_rt:empty(ChunkedSeq1),
  true = clj_rt:'list?'(EmptyList),
  0    = clj_rt:count(EmptyList),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ChunkedSeq1 = chunked_seq(64),
  ChunkedSeq1 = clj_rt:seq(ChunkedSeq1),
  ChunkedSeq2 = clj_rt:with_meta(ChunkedSeq1, #{a => 1}),
  ChunkedSeq3 = chunked_seq(33),

  true  = clj_rt:equiv(ChunkedSeq1, ChunkedSeq2),
  true  = clj_rt:equiv(ChunkedSeq1, lists:seq(0, 63)),
  false = clj_rt:equiv(ChunkedSeq1, ChunkedSeq3),
  false = clj_rt:equiv(ChunkedSeq1, clj_rt:symbol(<<"foo">>)),

  ChunkedSeq4 = chunked_seq(64),
  NextFun     = fun(_, Acc) -> clj_rt:next(Acc) end,
  ChunkedSeq5 = lists:foldl(NextFun, ChunkedSeq4, lists:seq(1, 64)),
  ?NIL        = clj_rt:seq(ChunkedSeq5),

  RestFun     = fun(_, Acc) -> clj_rt:next(Acc) end,
  ChunkedSeq6 = lists:foldl(RestFun, ChunkedSeq4, lists:seq(1, 63)),
  ?NIL        = clj_rt:next(ChunkedSeq6),
  EmptyList   = clj_rt:rest(ChunkedSeq6),
  0           = clj_rt:count(EmptyList),
  true        = ?NIL =/= EmptyList,

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  ChunkedSeq1 = chunked_seq(64),
  Cons = clj_rt:conj(ChunkedSeq1, -1),
  65   = clj_rt:count(Cons),
  true = clj_rt:equiv(Cons, lists:seq(-1, 63)),

  {comments, ""}.

-spec reduce(config()) -> result().
reduce(_Config) ->
  PlusFun     = fun
                  ([]) -> 0;
                  ([X, Y]) -> X + Y
                end,

  ChunkedSeq1 = chunked_seq(64),
  2016        = 'clojerl.IReduce':reduce(ChunkedSeq1, PlusFun),

  ChunkedSeq2 = chunked_seq(33),
  528         = 'clojerl.IReduce':reduce(ChunkedSeq2, PlusFun),
  529         = 'clojerl.IReduce':reduce(ChunkedSeq2, PlusFun, 1),

  PlusMaxFun = fun
                 ([]) -> 0;
                 ([X, Y]) when X < 10 -> X + Y;
                 ([X, _]) -> 'clojerl.Reduced':?CONSTRUCTOR(X)
            end,

  10 = 'clojerl.IReduce':reduce(ChunkedSeq2, PlusMaxFun),

  Array           = clj_vector:new(lists:seq(0, 32)),
  EmptyChunkedSeq = 'clojerl.Vector.ChunkedSeq':?CONSTRUCTOR(Array, 32, 1),

  0 = 'clojerl.IReduce':reduce(EmptyChunkedSeq, PlusFun, 0),
  0 = 'clojerl.IReduce':reduce(EmptyChunkedSeq, PlusFun),

  {comments, ""}.

-spec to_erl(config()) -> result().
to_erl(_Config) ->
  Tuple1      = list_to_tuple(lists:seq(0, 63)),
  ChunkedSeq1 = chunked_seq(64),
  Tuple1      = clj_rt:'->erl'(ChunkedSeq1, false),
  Tuple1      = clj_rt:'->erl'(ChunkedSeq1, true),

  Array            = clj_vector:new([1, ChunkedSeq1]),
  ChunkedSeq2      = 'clojerl.Vector.ChunkedSeq':?CONSTRUCTOR(Array, 0, 0),
  {1, ChunkedSeq1} = clj_rt:'->erl'(ChunkedSeq2, false),
  {1, Tuple1}      = clj_rt:'->erl'(ChunkedSeq2, true),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ?NIL = 'clojerl.Vector.ChunkedSeq':'_'(?NIL),

  VectorMeta  = clj_rt:with_meta(chunked_seq(64), #{a => 1}),
  #{a := 1} = clj_rt:meta(VectorMeta),

  Array = clj_vector:new(lists:seq(0, 32)),
  ChunkedSeq = 'clojerl.Vector.ChunkedSeq':?CONSTRUCTOR(Array, 32, 1),
  ?NIL = clj_rt:seq(ChunkedSeq),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec chunked_seq(integer()) -> 'clojerl.Vector.ChunkedSeq':type().
chunked_seq(Num) ->
  Vector = clj_rt:vector(lists:seq(0, Num - 1)),
  clj_rt:seq(Vector).
