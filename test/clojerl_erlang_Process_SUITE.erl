-module(clojerl_erlang_Process_SUITE).

-export([all/0, init_per_suite/1]).

-export([ hash/1
        , str/1
        ]).

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
  match = re:run(clj_core:str(self()), Regex, [{capture, none}]),

  {comments, ""}.
