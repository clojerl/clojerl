-module(erlang_Reference_SUITE).

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
  ct:comment("Different references are different"),
  Ref1 = erlang:make_ref(),
  Ref2 = erlang:make_ref(),

  Hash1 = 'clojerl.IHash':hash(Ref1),
  Hash2 = 'clojerl.IHash':hash(Ref2),

  true = Hash1 =/= Hash2,

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  ct:comment("Check the str representation of a reference"),
  Regex = <<"#<Ref \\d+\\.\\d+\\.\\d+\\.\\d+>">>,
  match = re:run(clj_rt:str(make_ref()), Regex, [{capture, none}]),

  {comments, ""}.
