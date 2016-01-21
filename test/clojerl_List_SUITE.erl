-module(clojerl_List_SUITE).

-export([all/0]).

-export([ new/1
        , count/1
        , str/1
        ]).

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-type config() :: list().
-type result() :: {comments, string()}.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec new(config()) -> result().
new(_Config) ->
  List = clj_core:list([1, 2, 3]),
  [1, 2, 3] = clj_core:seq(List).

-spec count(config()) -> result().
count(_Config) ->
  List = clj_core:list([1, 2, 3]),
  3 = clj_core:count(List),

  List2 = clj_core:list([]),
  0 = clj_core:count(List2).

-spec str(config()) -> result().
str(_Config) ->
  List = clj_core:list([1, 2, 3]),
  <<"(1, 2, 3)">> = clj_core:str(List),

  List2 = clj_core:list([]),
  <<"()">> = clj_core:str(List2).
