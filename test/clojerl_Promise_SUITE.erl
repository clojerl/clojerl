-module(clojerl_Promise_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ close/1
        , deref/1
        , deliver/1
        , equiv/1
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

-spec close(config()) -> result().
close(_Config) ->
  Promise = 'clojerl.Promise':?CONSTRUCTOR(),

  ct:comment("Close promise and check if the process is still alive"),
  _  = 'erlang.io.ICloseable':close(Promise),
  ok = try 'clojerl.Promise':deref(Promise), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec deref(config()) -> result().
deref(_Config) ->
  Promise = 'clojerl.Promise':?CONSTRUCTOR(),

  ct:comment("promise shouldn't be realized"),
  false = 'clojerl.Promise':'realized?'(Promise),

  ct:comment("deref a promise with a timeout while not delivered"),
  default = 'clojerl.Promise':deref(Promise, 10, default),

  ct:comment("deref a promise after delivering value"),
  Promise = 'clojerl.Promise':deliver(Promise, 1),
  1       = 'clojerl.Promise':deref(Promise),
  true    = 'clojerl.Promise':'realized?'(Promise),

  ct:comment("first delivered value should be returned"),
  Promise = 'clojerl.Promise':deliver(Promise, 2),
  1       = 'clojerl.Promise':deref(Promise),

  ct:comment("deref a promise with a timeout"),
  1 = 'clojerl.Promise':deref(Promise, 10, default),

  {comments, ""}.

-spec deliver(config()) -> result().
deliver(_Config) ->
  Promise = 'clojerl.Promise':?CONSTRUCTOR(),
  Self    = self(),
  N       = 10,
  Fun     = fun() -> X = 'clojerl.Promise':deref(Promise), Self ! X end,
  [spawn(Fun) || _ <- lists:seq(1, 10)],

  ct:comment("process won't continue until value is delivered"),
  Value   = erlang:make_ref(),
  timeout = clj_test_utils:wait_for(Value, N, 10),
  'clojerl.Promise':deliver(Promise, Value),
  ok      = clj_test_utils:wait_for(Value, N, 10),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  Promise1 = 'clojerl.Promise':?CONSTRUCTOR(),
  Promise2 = 'clojerl.Promise':?CONSTRUCTOR(),

  ct:comment("Same promise is equivalent"),
  true = clj_rt:equiv(Promise1, Promise1),

  ct:comment("Different promises are not equivalent"),
  false = clj_rt:equiv(Promise1, Promise2),

  ct:comment("An promise and something else"),
  false = clj_rt:equiv(Promise1, whatever),
  false = clj_rt:equiv(Promise1, #{}),
  false = clj_rt:equiv(Promise1, 1),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Promise = 'clojerl.Promise':?CONSTRUCTOR(),

  <<"#<clojerl.Promise ", _/binary>> = clj_rt:str(Promise),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  Promise = 'clojerl.Promise':?CONSTRUCTOR(),

  Hash = 'clojerl.IHash':hash(Promise),
  Hash = 'clojerl.IHash':hash(Promise),
  true = erlang:is_integer(Hash),

  {noreply, state} = 'clojerl.Promise':handle_info(msg, state),
  {ok, state}      = 'clojerl.Promise':terminate(msg, state),
  {ok, state}      = 'clojerl.Promise':code_change(msg, from, state),

  {comments, ""}.
