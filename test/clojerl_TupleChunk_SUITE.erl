-module(clojerl_TupleChunk_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ count/1
        , chunk/1
        , indexed/1
        , reduce/1
        ]).

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
  TupleChunk1 = new({1, 2, 3}),
  3 = clj_rt:count(TupleChunk1),

  TupleChunk2 = new({a, b, c, d, e, f}),
  6 = clj_rt:count(TupleChunk2),

  {comments, ""}.

-spec chunk(config()) -> result().
chunk(_Config) ->
  TupleChunk1 = new({1, 2, 3}),
  3 = clj_rt:count(TupleChunk1),

  TupleChunk2 = 'clojerl.IChunk':drop_first(TupleChunk1),
  2 = clj_rt:count(TupleChunk2),

  {comments, ""}.

-spec indexed(config()) -> result().
indexed(_Config) ->
  TupleChunk = new({1, 2, 3}),
  1    = clj_rt:nth(TupleChunk, 0),
  2    = clj_rt:nth(TupleChunk, 1),
  3    = clj_rt:nth(TupleChunk, 2),
  ?NIL = clj_rt:nth(TupleChunk, 3),

  1    = clj_rt:nth(TupleChunk, 0, foo),
  foo  = clj_rt:nth(TupleChunk, 42, foo),

  {comments, ""}.

-spec reduce(config()) -> result().
reduce(_Config) ->
  TupleChunk1 = new({1, 2, 3}),

  Sum = fun(Acc, Item) -> Acc + Item end,
  6   = 'clojerl.IReduce':reduce(TupleChunk1, Sum),
  10  = 'clojerl.IReduce':reduce(TupleChunk1, Sum, 4),

  TupleChunk2 = new({42}),
  42  = 'clojerl.IReduce':reduce(TupleChunk2, Sum),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper function
%%------------------------------------------------------------------------------

new(Tuple) ->
  'clojerl.TupleChunk':?CONSTRUCTOR(Tuple).
