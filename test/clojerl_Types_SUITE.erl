-module(clojerl_Types_SUITE).

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
  ct:comment("Check the string representation of a string"),
  <<"hello">> = clj_core:str(<<"hello">>),
  <<"">> = clj_core:str(<<"">>),

  ct:comment("Check the string representation of an integer"),
  <<"42">> = clj_core:str(42),

  ct:comment("Check the string representation of a float"),
  <<"3.14">> = clj_core:str(3.14),
  <<"3.1416">> = clj_core:str(3.1416),
  <<"3.14159265">> = clj_core:str(3.14159265),

  {comments, ""}.
