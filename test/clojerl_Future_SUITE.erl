-module(clojerl_Future_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ close/1
        , deref/1
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
  Future = 'clojerl.Future':?CONSTRUCTOR(fun() -> 1 end),

  ct:comment("Close future and check if the process is still alive"),
  false = 'clojerl.Future':'cancelled?'(Future),
  _     = 'erlang.io.ICloseable':close(Future),
  true  = 'clojerl.Future':'cancelled?'(Future),

  {comments, ""}.

-spec deref(config()) -> result().
deref(_Config) ->
  Future = 'clojerl.Future':?CONSTRUCTOR(fun() -> 1 end),

  ct:comment("deref a future..."),
  1 = 'clojerl.Future':deref(Future),

  ct:comment("deref a future with a timeout..."),
  1 = 'clojerl.Future':deref(Future, 1000, default),

  ct:comment("... and check it's done"),
  true = 'clojerl.Future':'done?'(Future),
  true = 'clojerl.Future':'realized?'(Future),

  ct:comment("Check status of a sleeping future..."),
  SleepFun       = fun() -> timer:sleep(10000) end,
  SleepingFuture = 'clojerl.Future':?CONSTRUCTOR(SleepFun),
  false          = 'clojerl.Future':'done?'(SleepingFuture),
  false          = 'clojerl.Future':'realized?'(SleepingFuture),
  false          = 'clojerl.Future':'cancelled?'(SleepingFuture),

  ct:comment("Cancel sleeping future and check status"),
  ok    = 'clojerl.Future':cancel(SleepingFuture),
  false = 'clojerl.Future':'done?'(SleepingFuture),
  false = 'clojerl.Future':'realized?'(SleepingFuture),
  true  = 'clojerl.Future':'cancelled?'(SleepingFuture),

  ct:comment("deref a future that generates an error"),
  ErrorFun    = fun() -> throw(foo) end,
  ErrorFuture = 'clojerl.Future':?CONSTRUCTOR(ErrorFun),
  ok = try clj_rt:deref(ErrorFuture), error
       catch _:_ -> ok
       end,

  ct:comment("Deref a sleeping future with a timeout"),
  SleepingFuture1 = 'clojerl.Future':?CONSTRUCTOR(SleepFun),
  default = 'clojerl.Future':deref(SleepingFuture1, 100, default),
  ok      = 'clojerl.Future':cancel(SleepingFuture1),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  Future1 = 'clojerl.Future':?CONSTRUCTOR(fun() -> 1 end),
  Future2 = 'clojerl.Future':?CONSTRUCTOR(fun() -> 2 end),

  ct:comment("Same future is equivalent"),
  true = clj_rt:equiv(Future1, Future1),

  ct:comment("Different futures are not equivalent"),
  false = clj_rt:equiv(Future1, Future2),

  ct:comment("An future and something else"),
  false = clj_rt:equiv(Future1, whatever),
  false = clj_rt:equiv(Future1, #{}),
  false = clj_rt:equiv(Future1, 1),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Future = 'clojerl.Future':?CONSTRUCTOR(fun() -> 1 end),

  <<"#<clojerl.Future ", _/binary>> = clj_rt:str(Future),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  Future = 'clojerl.Future':?CONSTRUCTOR(1),

  Hash = 'clojerl.IHash':hash(Future),
  Hash = 'clojerl.IHash':hash(Future),
  true = erlang:is_integer(Hash),

  {noreply, state} = 'clojerl.Future':handle_info(msg, state),
  {ok, state}      = 'clojerl.Future':terminate(msg, state),
  {ok, state}      = 'clojerl.Future':code_change(msg, from, state),

  {comments, ""}.
