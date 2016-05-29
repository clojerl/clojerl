-module(clojerl_String_SUITE).

-export([all/0, init_per_suite/1]).

-export([ str/1
        , seq/1
        , count/1
        , complete_coverage/1
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

-spec count(config()) -> result().
count(_Config) ->
  3 = clj_core:count(<<"abc">>),

  0 = clj_core:count(<<>>),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  <<"hello">> = clj_core:str(<<"hello">>),
  <<>> = clj_core:str(<<>>),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  5 = clj_core:count(clj_core:seq(<<"hello">>)),
  [<<"h">>, <<"e">>, <<"l">>, <<"l">>, <<"o">>] = clj_core:seq(<<"hello">>),

  undefined = clj_core:seq(<<>>),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  true = 'clojerl.String':starts_with(<<>>, <<>>),

  true = 'clojerl.String':starts_with(<<"123456">>, <<"1">>),
  true = 'clojerl.String':starts_with(<<"123456">>, <<"12">>),
  true = 'clojerl.String':starts_with(<<"123456">>, <<"123">>),

  false = 'clojerl.String':starts_with(<<"123456">>, <<"a">>),

  <<"3">> = 'clojerl.String':char_at(<<"123456">>, 2),


  ok = 'clojerl.String':'clojerl.ISequential.noop'(ok),

  {comments, ""}.
