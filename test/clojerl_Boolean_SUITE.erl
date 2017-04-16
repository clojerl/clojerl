-module(clojerl_Boolean_SUITE).

-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([hash/1]).
-export([str/1]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec hash(config()) -> result().
hash(_Config) ->
  ct:comment("Check the hash for true and false are different"),
  HashTrue  = 'clojerl.IHash':hash(true),
  HashFalse = 'clojerl.IHash':hash(false),
  true = HashTrue =/= HashFalse,

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  ct:comment("Check the string representation of true and false"),
  <<"true">> = clj_core:str(true),
  <<"false">> = clj_core:str(false),

  {comments, ""}.
