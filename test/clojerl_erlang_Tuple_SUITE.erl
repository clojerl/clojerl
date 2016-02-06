-module(clojerl_erlang_Tuple_SUITE).

-export([all/0, init_per_suite/1]).

-export([str/1]).

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

-spec str(config()) -> result().
str(_Config) ->
  Tuple = {1, 2, 3, 4},
  <<"#[1, 2, 3, 4]">> = clj_core:str(Tuple),

  EmptyTuple = {},
  <<"#[]">> = clj_core:str(EmptyTuple),

  {comments, ""}.
