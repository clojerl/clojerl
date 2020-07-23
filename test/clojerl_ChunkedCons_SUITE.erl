-module(clojerl_ChunkedCons_SUITE).

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

-spec new(config()) -> result().
new(_Config) ->
  ChunkedCons1 = chunked_cons({1, 2, 3}, ?NIL),
  [1, 2, 3]    = clj_rt:to_list(ChunkedCons1),

  ChunkedCons2 = chunked_cons({}, ?NIL),
  ChunkedCons2 = clj_rt:seq(ChunkedCons2),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  ChunkedCons1 = chunked_cons({1, 2, 3}, ?NIL),
  3 = clj_rt:count(ChunkedCons1),

  ChunkedCons2 = chunked_cons({1, 2, 3}, [4, 5, 6]),
  6 = clj_rt:count(ChunkedCons2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  ChunkedCons1 = chunked_cons({1, 2, 3}, ?NIL),
  ChunkedCons2 = chunked_cons({1, 2, 3}, [4, 5, 6]),

  <<"(1 2 3)">>       = clj_rt:str(ChunkedCons1),
  <<"(1 2 3 4 5 6)">> = clj_rt:str(ChunkedCons2),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  ChunkedCons = chunked_cons({1, 2, 3}, ?NIL),
  true        = clj_rt:'sequential?'(ChunkedCons),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  ChunkedCons1 = chunked_cons({1, 2, 3}, ?NIL),
  ChunkedCons2 = chunked_cons({1, 2, 3}, [4, 5, 6]),
  ChunkedCons3 = chunked_cons({a, b, c}, ?NIL),

  Hash1 = 'clojerl.IHash':hash(ChunkedCons1),
  Hash2 = 'clojerl.IHash':hash(ChunkedCons2),
  Hash3 = 'clojerl.IHash':hash(ChunkedCons3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec chunked(config()) -> result().
chunked(_Config) ->
  ChunkedCons1 = chunked_cons({1, 2, 3}, [4, 5, 6]),

  Chunk = 'clojerl.IChunkedSeq':chunked_first(ChunkedCons1),
  'clojerl.TupleChunk' = clj_rt:type_module(Chunk),
  3 = clj_rt:count(Chunk),

  [4, 5, 6] = 'clojerl.IChunkedSeq':chunked_next(ChunkedCons1),
  [4, 5, 6] = 'clojerl.IChunkedSeq':chunked_more(ChunkedCons1),

  ChunkedCons2 = chunked_cons({1, 2, 3}, []),
  ?NIL = 'clojerl.IChunkedSeq':chunked_next(ChunkedCons2),
  []   = 'clojerl.IChunkedSeq':chunked_more(ChunkedCons2),

  ChunkedCons3 = chunked_cons({1, 2, 3}, ?NIL),
  ?NIL = 'clojerl.IChunkedSeq':chunked_next(ChunkedCons3),
  []   = 'clojerl.IChunkedSeq':chunked_more(ChunkedCons3),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  ChunkedCons1 = chunked_cons({1, 2, 3}, ?NIL),
  ChunkedCons1 = clj_rt:seq(ChunkedCons1),

  ChunkedCons2 = clj_rt:rest(ChunkedCons1),
  ChunkedCons2 = clj_rt:next(ChunkedCons1),
  ChunkedCons3 = clj_rt:rest(ChunkedCons2),
  ChunkedCons4 = clj_rt:rest(ChunkedCons3),
  1 = clj_rt:first(ChunkedCons1),
  2 = clj_rt:first(ChunkedCons2),
  3 = clj_rt:first(ChunkedCons3),
  ?NIL = clj_rt:first(ChunkedCons4),
  []   = ChunkedCons4,
  ?NIL = clj_rt:next(ChunkedCons3),

  [] = clj_rt:empty(ChunkedCons1),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ChunkedCons1 = chunked_cons({1, 2, 3}, [4, 5, 6]),
  ChunkedCons2 = chunked_cons({1, 2, 3}, clj_rt:list([4, 5, 6])),
  true  = clj_rt:equiv(ChunkedCons1, ChunkedCons2),
  true  = clj_rt:equiv(ChunkedCons1, [1, 2, 3, 4, 5, 6]),
  false = clj_rt:equiv(ChunkedCons1, [1, 2, 3, 4, 5]),
  false = clj_rt:equiv(ChunkedCons1, clj_rt:symbol(<<"foo">>)),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  ChunkedCons1 = chunked_cons({1, 2, 3}, [4, 5, 6]),
  Cons = clj_rt:conj(ChunkedCons1, 0),
  7    = clj_rt:count(Cons),
  true = clj_rt:equiv(Cons, [0, 1, 2, 3, 4, 5, 6]),

  {comments, ""}.

-spec reduce(config()) -> result().
reduce(_Config) ->
  %% We need to wrap the fun into a clojerl.Fn because of the need
  %% for multiple arities
  PlusFun0     = fun
                   ([]) -> 0;
                   ([X, Y]) -> X + Y
                 end,
  PlusFun      = 'clojerl.Fn':?CONSTRUCTOR(PlusFun0),
  ChunkedCons1 = chunked_cons({}, ?NIL),

  0  = 'clojerl.IReduce':reduce(ChunkedCons1, PlusFun),
  42 = 'clojerl.IReduce':reduce(ChunkedCons1, PlusFun, 42),

  ChunkedCons2 = chunked_cons({1, 2, 3, 4, 5}, [6, 7, 8, 9, 10]),
  55 = 'clojerl.IReduce':reduce(ChunkedCons2, PlusFun),
  60 = 'clojerl.IReduce':reduce(ChunkedCons2, PlusFun, 5),

  PlusMaxFun = fun
                 (X, Y) when X < 10 -> X + Y;
                 (X, _) -> 'clojerl.Reduced':?CONSTRUCTOR(X)
               end,

  10 = clj_rt:deref('clojerl.IReduce':reduce(ChunkedCons2, PlusMaxFun)),

  {comments, ""}.

-spec to_erl(config()) -> result().
to_erl(_Config) ->
  ChunkedCons1 = chunked_cons({1, 2, 3}, ?NIL),
  [1, 2, 3] = clj_rt:'clj->erl'(ChunkedCons1, false),
  [1, 2, 3] = clj_rt:'clj->erl'(ChunkedCons1, true),

  ChunkedCons2         = chunked_cons({1, 2, ChunkedCons1}, ?NIL),
  [1, 2, ChunkedCons1] = clj_rt:'clj->erl'(ChunkedCons2, false),
  [1, 2, [1, 2, 3]]    = clj_rt:'clj->erl'(ChunkedCons2, true),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ChunkedCons = chunked_cons({1, 2, 3}, ?NIL),

  ChunkedConsMeta = clj_rt:with_meta(ChunkedCons, #{a => 1}),
  #{a := 1} = clj_rt:meta(ChunkedConsMeta),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec chunked_cons(tuple(), list()) -> 'clojerl.ChunkedCons':type().
chunked_cons(Tuple, More) ->
  Chunk = 'clojerl.TupleChunk':?CONSTRUCTOR(Tuple),
  'clojerl.ChunkedCons':?CONSTRUCTOR(Chunk, More).
