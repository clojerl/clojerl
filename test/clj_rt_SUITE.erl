-module(clj_rt_SUITE).

-include("clojerl.hrl").
-include("clojerl_int.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([print/1]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

-spec init_per_testcase(_, config()) -> config().
init_per_testcase(_, Config) ->
  Config.

-spec end_per_testcase(_, config()) -> config().
end_per_testcase(_, Config) ->
  Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec print(config()) -> result().
print(_Config) ->
  ct:comment("The underlying processes used for printing don't hang around"),
  Count = length(erlang:processes()),
  <<"#erl[]">> = clj_rt:print({}),
  Count = length(erlang:processes()),

  {comments, ""}.
