-module(erlang_Process_SUITE).

-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ hash/1
        , str/1
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

-spec hash(config()) -> result().
hash(_Config) ->
  ct:comment("Different process are different"),
  Pid1 = spawn(fun() -> ok end),
  Pid2 = spawn(fun() -> ok end),

  Hash1 = 'clojerl.IHash':hash(Pid1),
  Hash2 = 'clojerl.IHash':hash(Pid2),

  true = Hash1 =/= Hash2,

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  ct:comment("Check the str representation of a pid"),
  Regex = <<"#<\\d+\\.\\d+\\.\\d+>">>,
  match = re:run(clj_rt:str(self()), Regex, [{capture, none}]),

  {comments, ""}.
