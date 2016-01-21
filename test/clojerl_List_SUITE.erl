-module(clojerl_List_SUITE).

-export([all/0, init_per_suite/1]).

-export([ new/1
        , count/1
        , str/1
        , is_sequential/1
        , seq/1
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
  List = clj_core:list([1, 2, 3]),
  [1, 2, 3] = clj_core:seq(List),

  List2 = clj_core:list([]),
  undefined = clj_core:seq(List2),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  List = clj_core:list([1, 2, 3]),
  3 = clj_core:count(List),

  List2 = clj_core:list([]),
  0 = clj_core:count(List2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  List = clj_core:list([1, 2, 3]),
  <<"(1 2 3)">> = clj_core:str(List),

  List2 = clj_core:list([]),
  <<"()">> = clj_core:str(List2),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  List = clj_core:list([1, 2, 3]),
  true = clj_core:'sequential?'(List),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  List = clj_core:list([1, 2, 3]),
  1 = clj_core:first(List),
  [2, 3] = clj_core:seq(clj_core:next(List)),
  [2, 3] = clj_core:seq(clj_core:rest(List)),

  List2 = clj_core:list([1]),
  1 = clj_core:first(List2),
  undefined = clj_core:next(List2),
  [] = clj_core:seq2(clj_core:rest(List2)),

  List3 = clj_core:list([]),
  undefined = clj_core:first(List3),
  undefined = clj_core:next(List3),
  undefined = clj_core:rest(List3),

  {comments, ""}.
