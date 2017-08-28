-module(clojerl_Range_SUITE).

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
        , chunked/1
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
  Range = 'clojerl.Range':?CONSTRUCTOR(0, 3, 1),
  [0, 1, 2] = clj_rt:to_list(Range),

  [] = 'clojerl.Range':?CONSTRUCTOR(2, 1, 1),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Range = 'clojerl.Range':?CONSTRUCTOR(0, 10, 1),
  10 = clj_rt:count(Range),

  Range2 = 'clojerl.Range':?CONSTRUCTOR(0, 10, 2),
  5 = clj_rt:count(Range2),

  Range3 = 'clojerl.Range':?CONSTRUCTOR(10, 0, -1),
  10 = clj_rt:count(Range3),

  Range4 = 'clojerl.Range':?CONSTRUCTOR(10, 0, -2),
  5 = clj_rt:count(Range4),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Range = 'clojerl.Range':?CONSTRUCTOR(0, 5, 1),
  <<"(0 1 2 3 4)">> = clj_rt:str(Range),

  Range2 = 'clojerl.Range':?CONSTRUCTOR(5, 0, 1),
  <<"#erl()">> = clj_rt:str(Range2),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  Range = 'clojerl.Range':?CONSTRUCTOR(0, 3, 1),
  true = clj_rt:'sequential?'(Range),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  Range1 = 'clojerl.Range':?CONSTRUCTOR(0, 3, 1),
  Range2 = 'clojerl.Range':?CONSTRUCTOR(3, 0, -1),

  Hash1 = 'clojerl.IHash':hash(Range1),
  Hash2 = 'clojerl.IHash':hash(Range2),

  true = Hash1 =/= Hash2,

  {comments, ""}.

-spec chunked(config()) -> result().
chunked(_Config) ->
  Range1 = 'clojerl.Range':?CONSTRUCTOR(0, 33, 1),

  Chunk1a = 'clojerl.IChunkedSeq':chunked_first(Range1),
  ?CHUNK_SIZE = clj_rt:count(Chunk1a),
  0 = clj_rt:nth(Chunk1a, 0),

  ChunkNext1 = 'clojerl.IChunkedSeq':chunked_more(Range1),
  Chunk1b = 'clojerl.IChunkedSeq':chunked_first(ChunkNext1),
  32 = clj_rt:nth(Chunk1b, 0),

  ?NIL = 'clojerl.IChunkedSeq':chunked_next(ChunkNext1),

  Range2  = 'clojerl.Range':?CONSTRUCTOR(32, -1, -1),

  Chunk2a = 'clojerl.IChunkedSeq':chunked_first(Range2),
  ?CHUNK_SIZE = clj_rt:count(Chunk2a),
  32 = clj_rt:nth(Chunk2a, 0),

  ChunkNext2 = 'clojerl.IChunkedSeq':chunked_more(Range2),
  Chunk2b = 'clojerl.IChunkedSeq':chunked_first(ChunkNext2),
  0 = clj_rt:nth(Chunk2b, 0),

  ?NIL = 'clojerl.IChunkedSeq':chunked_next(ChunkNext2),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Range1 = 'clojerl.Range':?CONSTRUCTOR(0, 3, 1),
  0 = clj_rt:first(Range1),
  [1, 2] = clj_rt:to_list(clj_rt:next(Range1)),
  [1, 2] = clj_rt:to_list(clj_rt:rest(Range1)),

  Range2 = 'clojerl.Range':?CONSTRUCTOR(1, 2, 1),
  1 = clj_rt:first(Range2),
  ?NIL = clj_rt:next(Range2),
  [] = clj_rt:to_list(clj_rt:rest(Range2)),

  Range3 = 'clojerl.Range':?CONSTRUCTOR(2, 1, 1),
  ?NIL = clj_rt:first(Range3),
  ?NIL = clj_rt:next(Range3),
  [] = clj_rt:rest(Range3),

  Range4 = 'clojerl.Range':?CONSTRUCTOR(0, 1, 0.2),
  0 = clj_rt:first(Range4),
  [0.2, 0.4, 0.4 + 0.2, 0.8] = clj_rt:to_list(clj_rt:next(Range4)),

  Range5 = 'clojerl.Range':?CONSTRUCTOR(0, 1.2, 0.2),
  0 = clj_rt:first(Range5),
  [0.2, 0.4, 0.4 + 0.2, 0.8, 1.0] = clj_rt:to_list(clj_rt:next(Range5)),

  Range6 = 'clojerl.Range':?CONSTRUCTOR(0.3, 0, -0.1),
  [0.3, 0.3 - 0.1, 0.3 - 0.1 - 0.1] = clj_rt:to_list(Range6),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  Range = 'clojerl.Range':?CONSTRUCTOR(0, 3, 1),

  ct:comment("Check that lists with the same elements are equivalent"),
  Range1 = clj_rt:with_meta(Range, #{a => 1}),
  Range2 = clj_rt:with_meta(Range, #{b => 2}),
  true   = clj_rt:equiv(Range1, Range2),

  ct:comment("Check that lists with the same elements are not equivalent"),
  Range3 = clj_rt:with_meta('clojerl.Range':?CONSTRUCTOR(1, 4, 1), #{c => 3}),
  false  = clj_rt:equiv(Range1, Range3),

  ct:comment("A clojerl.List and an erlang.List"),
  true  = clj_rt:equiv(Range, [0, 1, 2]),
  false = clj_rt:equiv(Range, [1, 2, 3, a]),

  ct:comment("A clojerl.List and something else"),
  false = clj_rt:equiv(Range1, whatever),
  false = clj_rt:equiv(Range1, #{}),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  OneRange = 'clojerl.Range':?CONSTRUCTOR(2, 0, -1),

  ct:comment("Conj an element to a range"),
  ThreeList = clj_rt:conj(OneRange, 3),

  3    = clj_rt:count(ThreeList),
  true = clj_rt:equiv(ThreeList, [3, 2, 1]),

  ct:comment("Conj an element to a list with one element"),
  FourList = clj_rt:conj(ThreeList, 4),

  4    = clj_rt:count(FourList),
  true = clj_rt:equiv(FourList, [4, 3, 2, 1]),

  {comments, ""}.

-spec reduce(config()) -> result().
reduce(_Config) ->
  PlusFun = fun
              ([]) -> 0;
              ([X, Y]) -> X + Y
            end,
  EmptyRange = 'clojerl.Range':?CONSTRUCTOR(2, 2, 0),

  0  = 'clojerl.IReduce':reduce(EmptyRange, PlusFun),
  42 = 'clojerl.IReduce':reduce(EmptyRange, PlusFun, 42),

  TenRange = 'clojerl.Range':?CONSTRUCTOR(1, 11, 1),
  55 = 'clojerl.IReduce':reduce(TenRange, PlusFun),
  60 = 'clojerl.IReduce':reduce(TenRange, PlusFun, 5),

  PlusMaxFun = fun
                 ([]) -> 0;
                 ([X, Y]) when X < 10 -> X + Y;
                 ([X, _]) -> 'clojerl.Reduced':?CONSTRUCTOR(X)
            end,
  Reduced = 'clojerl.IReduce':reduce(TenRange, PlusMaxFun),
  10 = clj_rt:deref(Reduced),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ?NIL  = 'clojerl.Range':'_'(?NIL),

  Range = 'clojerl.Range':?CONSTRUCTOR(1, 10, 1),
  []    = clj_rt:empty(Range),

  RangeMeta = clj_rt:with_meta(Range, #{a => 1}),
  #{a := 1} = clj_rt:meta(RangeMeta),

  Range2 = 'clojerl.Range':?CONSTRUCTOR(1, 1, 1),
  Range  = clj_rt:seq(Range),
  ?NIL   = clj_rt:seq(Range2),

  {comments, ""}.
