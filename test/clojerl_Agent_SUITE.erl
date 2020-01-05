-module(clojerl_Agent_SUITE).

-include("clojerl.hrl").
-include("clojerl_int.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ close/1
        , deref/1
        , error_handler/1
        , equiv/1
        , meta/1
        , restart/1
        , release_pending/1
        , str/1
        , validator/1
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
  0     = ets:info(?AGENT_TABLE, size),
  Count = length(erlang:processes()),

  Agent = 'clojerl.Agent':?CONSTRUCTOR(1),

  1     = ets:info(?AGENT_TABLE, size),
  Count = length(erlang:processes()) - 1,

  ?NIL  = 'clojerl.Agent':close(Agent),

  ok    = timer:sleep(100),

  0     = ets:info(?AGENT_TABLE, size),
  Count = length(erlang:processes()),

  {comments, ""}.

-spec deref(config()) -> result().
deref(_Config) ->
  Agent = 'clojerl.Agent':?CONSTRUCTOR(1),

  ct:comment("deref an agent"),
  1 = clj_rt:deref(Agent),

  ct:comment("deref an agent after applying action"),
  Agent = 'clojerl.Agent':dispatch(Agent, fun erlang:'+'/2, [1]),
  ok    = sync(Agent, 1000),
  2     = clj_rt:deref(Agent),

  {comments, ""}.

-spec error_handler(config()) -> result().
error_handler(_Config) ->
  Agent = 'clojerl.Agent':?CONSTRUCTOR(1),

  ct:comment("Error handler is called when error happens"),
  Self  = erlang:self(),
  Ref   = erlang:make_ref(),
  ErrorHandler = fun(_Agent, _Err) -> Self ! Ref end,
  ErrorHandler = 'clojerl.Agent':error_handler(Agent, ErrorHandler),
  Agent    = 'clojerl.Agent':dispatch(Agent, fun(_) -> throw(foo) end, []),
  %% Expect message from the error handler
  ok       = clj_test_utils:wait_for(Ref, 1000),
  continue = 'clojerl.Agent':error_mode(Agent),
  ?NIL     = 'clojerl.Agent':error(Agent),

  ct:comment("Error handler is what has been previously set"),
  ErrorHandler = 'clojerl.Agent':error_handler(Agent),

  ct:comment("Error handler generates an error but it's ignored"),
  ErrorHandlerFails = fun(_Agent, _Err) -> Self ! Ref, throw(handler) end,
  ErrorHandlerFails = 'clojerl.Agent':error_handler(Agent, ErrorHandlerFails),
  Agent    = 'clojerl.Agent':dispatch(Agent, fun(_) -> throw(foo) end, []),
  ok       = clj_test_utils:wait_for(Ref, 1000),
  continue = 'clojerl.Agent':error_mode(Agent),
  ?NIL     = 'clojerl.Agent':error(Agent),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  Agent1 = 'clojerl.Agent':?CONSTRUCTOR(1),
  Agent2 = 'clojerl.Agent':?CONSTRUCTOR(2),

  ct:comment("Check that the same atom with different meta is equivalent"),
  Agent3 = clj_rt:with_meta(Agent1, #{a => 1}),
  Agent4 = clj_rt:with_meta(Agent1, #{b => 2}),
  true  = clj_rt:equiv(Agent3, Agent4),

  ct:comment("Check that different atoms are not equivalent"),
  false = clj_rt:equiv(Agent1, Agent2),

  ct:comment("An atom and something else"),
  false = clj_rt:equiv(Agent1, whatever),
  false = clj_rt:equiv(Agent1, #{}),
  false = clj_rt:equiv(Agent1, 1),

  {comments, ""}.

-spec meta(config()) -> result().
meta(_Config) ->
  Agent     = 'clojerl.Agent':?CONSTRUCTOR(1),

  ct:comment("Change using with_meta"),
  Agent     = clj_rt:with_meta(Agent, #{a => 1}),
  #{a := 1} = clj_rt:meta(Agent),

  ct:comment("Change using reset_meta"),
  ?NIL      = 'clojerl.Agent':reset_meta(Agent, ?NIL),
  ?NIL      = clj_rt:meta(Agent),
  #{a := 1} = 'clojerl.Agent':reset_meta(Agent, #{a => 1}),
  #{a := 1} = clj_rt:meta(Agent),

  ct:comment("Alter meta successfully"),
  AlterFun  = fun(X) -> maps:put(b, 2, X) end,
  #{a := 1, b := 2} = 'clojerl.Agent':alter_meta(Agent, AlterFun, []),
  #{a := 1, b := 2} = clj_rt:meta(Agent),

  ct:comment("Alter function generates error, meta doesn't change"),
  ErrorFun  = fun(_X) -> error(foo) end,
  ok        = try 'clojerl.Agent':alter_meta(Agent, ErrorFun, []), error
              catch error:foo -> ok
              end,
  #{a := 1, b := 2} = clj_rt:meta(Agent),

  {comments, ""}.

-spec restart(config()) -> result().
restart(_Config) ->
  Agent = 'clojerl.Agent':?CONSTRUCTOR(1),
  fail  = 'clojerl.Agent':error_mode(Agent, fail),

  ct:comment("Agent does not need restart"),
  ok    = try 'clojerl.Agent':restart(Agent, 2, false), error
          catch _:_ -> ok
          end,

  ct:comment("Send failing action"),
  Agent = 'clojerl.Agent':dispatch(Agent, fun(_) -> throw(foo) end, []),
  %% We don't expect an async reply since the state will be failed
  _     = sync(Agent, 0),
  true  = 'clojerl.Agent':error(Agent) =/= ?NIL,

  ct:comment("Dispatch causes error when agent failed"),
  ok    = try 'clojerl.Agent':dispatch(Agent, fun(_) -> 42 end, []), error
          catch _:_ -> ok
          end,

  ct:comment("No error after restart"),
  2     = 'clojerl.Agent':restart(Agent, 2, false),
  ?NIL  = 'clojerl.Agent':error(Agent),

  ct:comment("Send failing action and restart with invalid value"),
  %% Add validator
  Valid = fun(X) -> X < 3 end,
  Valid = 'clojerl.Agent':validator(Agent, Valid),
  %% Generate error
  Agent = 'clojerl.Agent':dispatch(Agent, fun(_) -> throw(foo) end, []),
  _     = sync(Agent, 0),
  true  = 'clojerl.Agent':error(Agent) =/= ?NIL,
  %% Restart with invalid value
  ok    = try 'clojerl.Agent':restart(Agent, 42, false), error
          catch _:_ -> ok
          end,
  1    = 'clojerl.Agent':restart(Agent, 1, false),

  ct:comment("Send failing action with a valid one after that"),
  SleepFail = fun(_) -> timer:sleep(100), throw(foo) end,
  Agent = 'clojerl.Agent':dispatch(Agent, SleepFail, []),
  Agent = 'clojerl.Agent':dispatch(Agent, fun(X) -> X + 1 end, []),
  _     = sync(Agent, 0),
  true  = 'clojerl.Agent':error(Agent) =/= ?NIL,

  ct:comment("After restarting pending action is applied"),
  1    = 'clojerl.Agent':restart(Agent, 1, false),
  _    = sync(Agent, 0),
  2    = 'clojerl.Agent':deref(Agent),

  ct:comment("Send failing action with a valid one after that"),
  Agent = 'clojerl.Agent':dispatch(Agent, SleepFail, []),
  Agent = 'clojerl.Agent':dispatch(Agent, fun(X) -> X + 1 end, []),
  _     = sync(Agent, 0),
  true  = 'clojerl.Agent':error(Agent) =/= ?NIL,

  ct:comment("Restart pending action, clearing actions"),
  1    = 'clojerl.Agent':restart(Agent, 1, true),
  _    = sync(Agent, 0),

  ct:comment("Value is unchanged"),
  1    = 'clojerl.Agent':deref(Agent),

  {comments, ""}.

-spec release_pending(config()) -> result().
release_pending(_Config) ->
  ct:comment("Release pending from outside action returns 0"),
  0 = 'clojerl.Agent':release_pending_sends(),

  Agent = 'clojerl.Agent':?CONSTRUCTOR(1),

  ct:comment("Loose pending sendings when action fails"),
  Fail  = fun(_) ->
              'clojerl.Agent':dispatch(Agent, fun(X) -> X + 1 end, []),
              throw(foo)
          end,
  Agent = 'clojerl.Agent':dispatch(Agent, Fail, []),
  ok    = sync(Agent, 1000),

  ct:comment("Release actions before when action fails"),
  Release = fun(_) ->
                'clojerl.Agent':dispatch(Agent, fun(X) -> X + 1 end, []),
                'clojerl.Agent':release_pending_sends(),
                throw(foo)
            end,
  Agent   = 'clojerl.Agent':dispatch(Agent, Release, []),
  ok      = sync(Agent, 1000),
  2       = 'clojerl.Agent':deref(Agent),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Agent0 = 'clojerl.Agent':?CONSTRUCTOR(1),
  Agent1 = clj_rt:with_meta(Agent0, #{a => 1}),

  <<"#<clojerl.Agent ", _/binary>> = clj_rt:str(Agent1),

  {comments, ""}.

-spec validator(config()) -> result().
validator(_Config) ->
  Agent = 'clojerl.Agent':?CONSTRUCTOR(1),
  fail  = 'clojerl.Agent':error_mode(Agent, fail),

  ct:comment("Valid state is less than 3"),
  Valid = fun(X) -> X < 3 end,
  Valid = 'clojerl.Agent':validator(Agent, Valid),
  Valid = 'clojerl.Agent':validator(Agent),

  ct:comment("Change to a valid state"),
  Agent = 'clojerl.Agent':dispatch(Agent, fun erlang:'+'/2, [1]),
  ok    = sync(Agent, 1000),
  2     = clj_rt:deref(Agent),
  ?NIL  = 'clojerl.Agent':error(Agent),

  ct:comment("Change to an invalid state"),
  Inc   = fun(X) -> X + 1 end,
  ok    = sync(Agent, 1000, Inc),
  2     = clj_rt:deref(Agent),
  fail  = 'clojerl.Agent':error_mode(Agent),
  true  = 'clojerl.Agent':error(Agent) =/= ?NIL,

  ct:comment("Dispatch when agent is failed"),
  ok    = try 'clojerl.Agent':dispatch(Agent, Inc, []), error
          catch _:_ -> ok
          end,

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  Agent = 'clojerl.Agent':?CONSTRUCTOR(1),

  Hash = 'clojerl.IHash':hash(Agent),
  Hash = 'clojerl.IHash':hash(Agent),
  true = erlang:is_integer(Hash),

  %% clojerl.Agent
  {noreply, state} = 'clojerl.Agent':handle_info(msg, state),
  {ok, state}      = 'clojerl.Agent':terminate(msg, state),
  {ok, state}      = 'clojerl.Agent':code_change(msg, from, state),

  %% clojerl.Agent.Server
  {noreply, state} = 'clojerl.Agent.Server':handle_cast(msg, state),
  {noreply, state} = 'clojerl.Agent.Server':handle_info(msg, state),
  {ok, state}      = 'clojerl.Agent.Server':terminate(msg, state),
  {ok, state}      = 'clojerl.Agent.Server':code_change(msg, from, state),

  {error, _}       = 'clojerl.Agent.Server':create(1, <<"id">>, 1),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec sync('clojerl.Agent':type(), timeout()) -> ok | timeout.
sync(Agent, Timeout) ->
  sync(Agent, Timeout, fun(X) -> X end).

-spec sync('clojerl.Agent':type(), timeout(), function()) -> ok | timeout.
sync(Agent, Timeout, Fun) ->
  Self  = erlang:self(),
  Ref   = erlang:make_ref(),
  Agent = 'clojerl.Agent':dispatch(Agent, fun(X) -> Self ! Ref, Fun(X) end, []),
  Reply = clj_test_utils:wait_for(Ref, Timeout),

  %% This will send a synchronous message to the agent which
  %% will ensure any previous proceessing is done by the time
  %% we return from this function
  ErrorMode = 'clojerl.Agent':error_mode(Agent),
  ErrorMode = 'clojerl.Agent':error_mode(Agent, ErrorMode),

  Reply.
