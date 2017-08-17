-module(erlang_Tuple_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ count/1
        , equiv/1
        , hash/1
        , is_sequential/1
        , nth/1
        , seq/1
        , str/1
        , complete_coverage/1
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
  3 = clj_rt:count({1, 2, 3}),
  0 = clj_rt:count({}),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  Symbol = clj_rt:symbol(<<"hello">>),
  ct:comment("Check that tuples with the same elements are equivalent"),
  Tuple1 = {clj_rt:with_meta(Symbol, #{a => 1}), 1, a},
  Tuple2 = {clj_rt:with_meta(Symbol, #{b => 2}), 1, a},
  true = clj_rt:equiv(Tuple1, Tuple2),

  ct:comment("Check that maps with different elements are not equivalent"),
  false = clj_rt:equiv(Tuple1, {a, b, c}),
  false = clj_rt:equiv(Tuple1, {1, 2, 3, 4}),

  ct:comment("A Tuple and a List"),
  true = clj_rt:equiv(Tuple1, [Symbol, 1, a]),
  false = clj_rt:equiv(Tuple1, [1, 2, 3, 4]),
  false = clj_rt:equiv(Tuple1, [2, 3]),

  ct:comment("A Tuple and something else"),
  false = clj_rt:equiv(Tuple1, whatever),
  false = clj_rt:equiv(Tuple1, 1),
  false = clj_rt:equiv(Tuple1, [1]),
  false = clj_rt:equiv(Tuple1, #{}),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  Hash1 = 'clojerl.IHash':hash({1, 2, 3}),
  Hash2 = 'clojerl.IHash':hash({1, 3, 2}),
  Hash3 = 'clojerl.IHash':hash({1, 3, 2.0}),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  true = clj_rt:'sequential?'({1, 2, 3}),
  true = clj_rt:'sequential?'({}),

  {comments, ""}.

-spec nth(config()) -> result().
nth(_Config) ->
  Tuple = {1, b, 3},

  1 = clj_rt:nth(Tuple, 0),
  b = clj_rt:nth(Tuple, 1),
  3 = clj_rt:nth(Tuple, 2),

  3 = clj_rt:nth(Tuple, 2, not_found),
  not_found = clj_rt:nth(Tuple, 42, not_found),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  [1, 2, 3] = clj_rt:seq({1, 2, 3}),

  [1] = clj_rt:seq({1}),

  ?NIL = clj_rt:seq({}),
  [1, 2, 3] = clj_rt:to_list({1, 2, 3}),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Tuple = {1, 2, 3, 4},
  <<"#erl[1 2 3 4]">> = clj_rt:str(Tuple),

  EmptyTuple = {},
  <<"#erl[]">> = clj_rt:str(EmptyTuple),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ?NIL = 'erlang.Tuple':'_'(?NIL),

  {comments, ""}.
