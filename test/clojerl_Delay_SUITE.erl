-module(clojerl_Delay_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ close/1
        , deref/1
        , equiv/1
        , force/1
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
  Delay = 'clojerl.Delay':?CONSTRUCTOR(fun() -> 1 end),

  ct:comment("Close delay and check if the process is still alive"),
  _  = 'erlang.io.ICloseable':close(Delay),

  {comments, ""}.

-spec deref(config()) -> result().
deref(_Config) ->
  Delay = 'clojerl.Delay':?CONSTRUCTOR(fun() -> 1 end),

  ct:comment("deref a delay"),
  1    = 'clojerl.Delay':deref(Delay),
  true = 'clojerl.Delay':'realized?'(Delay),

  ct:comment("deref a delay that generates an error"),
  ErrorFun    = fun() -> throw(foo) end,
  ErrorDelay = 'clojerl.Delay':?CONSTRUCTOR(ErrorFun),
  ok = try clj_rt:deref(ErrorDelay), error
       catch _:_ -> ok
       end,

  ct:comment("force then deref a delay with a side effect"),
  Self = self(),
  SideEffectDelay = 'clojerl.Delay':?CONSTRUCTOR(fun() -> Self ! foo, 1 end),

  1 = 'clojerl.Delay':force(SideEffectDelay),
  1 = 'clojerl.Delay':deref(SideEffectDelay),

  ct:comment("code should only run once"),
  ok = clj_test_utils:wait_for(foo, 1, 100),
  timeout = clj_test_utils:wait_for(foo, 1, 100),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  Delay1 = 'clojerl.Delay':?CONSTRUCTOR(fun() -> 1 end),
  Delay2 = 'clojerl.Delay':?CONSTRUCTOR(fun() -> 2 end),

  ct:comment("Same delay is equivalent"),
  true = clj_rt:equiv(Delay1, Delay1),

  ct:comment("Different delays are not equivalent"),
  false = clj_rt:equiv(Delay1, Delay2),

  ct:comment("An delay and something else"),
  false = clj_rt:equiv(Delay1, whatever),
  false = clj_rt:equiv(Delay1, #{}),
  false = clj_rt:equiv(Delay1, 1),

  {comments, ""}.

-spec force(config()) -> result().
force(_Config) ->
  Delay = 'clojerl.Delay':?CONSTRUCTOR(fun() -> 1 end),

  ct:comment("when a delay return the result"),
  1     = 'clojerl.Delay':force(Delay),

  ct:comment("when not a delay just return the same"),
  foo = 'clojerl.Delay':force(foo),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Delay = 'clojerl.Delay':?CONSTRUCTOR(fun() -> 1 end),

  <<"#<clojerl.Delay ", _/binary>> = clj_rt:str(Delay),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  Delay = 'clojerl.Delay':?CONSTRUCTOR(1),

  Hash = 'clojerl.IHash':hash(Delay),
  Hash = 'clojerl.IHash':hash(Delay),
  true = erlang:is_integer(Hash),

  {noreply, state} = 'clojerl.Delay':handle_cast(msg, state),
  {noreply, state} = 'clojerl.Delay':handle_info(msg, state),
  {ok, state}      = 'clojerl.Delay':terminate(msg, state),
  {ok, state}      = 'clojerl.Delay':code_change(msg, from, state),

  {comments, ""}.
