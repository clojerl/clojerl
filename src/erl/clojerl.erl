-module(clojerl).

-behavior(application).

-export([start/0, unstick/0]).

-export([start/2, stop/1]).

-define(APP, clojerl).
-define(STICKY_MODULES, ['clojure.core']).

-spec start() -> ok.
start() ->
  {ok, _} = application:ensure_all_started(clojerl, permanent),
  ok.

-spec start(any(), any()) -> {ok, pid()} | {ok, pid(), any()} | {error, any()}.
start(_Type, _Args) ->
  {ok, _} = clojerl_sup:start_link(),
  ok = stacktrace_depth(),
  ok = io_options(),
  ok = stick(),
  {ok, self()}.

-spec stop(any()) -> ok.
stop(_State) -> ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec stacktrace_depth() -> ok.
stacktrace_depth() ->
  StacktraceDepth = application:get_env(?APP, backtrace_depth, 20),
  erlang:system_flag(backtrace_depth, StacktraceDepth),
  ok.

-spec io_options() -> ok.
io_options() ->
  %% Ensure encoding is unicode
  IoOpts = [{binary, true}, {encoding, unicode}],
  ok = io:setopts(IoOpts).

-spec stick() -> ok.
stick() ->
  [code:stick_mod(M) || M <- ?STICKY_MODULES],
  ok.

-spec unstick() -> ok.
unstick() ->
  [code:unstick_mod(M) || M <- ?STICKY_MODULES],
  ok.
