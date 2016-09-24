-module(clojerl_erlang_Tuple_SUITE).

-export([all/0, init_per_suite/1]).

-export([ count/1
        , hash/1
        , is_sequential/1
        , nth/1
        , seq/1
        , str/1
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

-spec count(config()) -> result().
count(_Config) ->
  3 = clj_core:count({1, 2, 3}),
  0 = clj_core:count({}),

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
  true = clj_core:'sequential?'({1, 2, 3}),
  true = clj_core:'sequential?'({}),

  {comments, ""}.

-spec nth(config()) -> result().
nth(_Config) ->
  Tuple = {1, b, 3},

  1 = clj_core:nth(Tuple, 0),
  b = clj_core:nth(Tuple, 1),
  3 = clj_core:nth(Tuple, 2),

  3 = clj_core:nth(Tuple, 2, not_found),
  not_found = clj_core:nth(Tuple, 42, not_found),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  [1, 2, 3] = clj_core:seq({1, 2, 3}),

  [1] = clj_core:seq({1}),

  undefined = clj_core:seq({}),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Tuple = {1, 2, 3, 4},
  <<"#[1, 2, 3, 4]">> = clj_core:str(Tuple),

  EmptyTuple = {},
  <<"#[]">> = clj_core:str(EmptyTuple),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  undefined = 'clojerl.erlang.Tuple':'_'(undefined),

  {comments, ""}.
