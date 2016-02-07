-module(clojerl_Keyword_SUITE).

-export([all/0, init_per_suite/1]).

-export([ invoke/1
        , name/1
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

-spec invoke(config()) -> result().
invoke(_Config) ->
  HelloKeyword = clj_core:keyword(<<"hello">>),
  world = clj_core:invoke(HelloKeyword, [#{HelloKeyword => world}]),

  not_found = clj_core:invoke(HelloKeyword, [#{bla => ble}, not_found]),

  ok = try
         clj_core:invoke(HelloKeyword, [#{bla => ble}, not_found, extra]),
         error
       catch _:_ ->
           ok
       end,

  {comments, ""}.

-spec name(config()) -> result().
name(_Config) ->
  HelloKeyword = clj_core:keyword(<<"hello">>),
  undefined   = clj_core:namespace(HelloKeyword),
  <<"hello">> = clj_core:name(HelloKeyword),

  HelloWorldKeyword = clj_core:keyword(<<"hello">>, <<"world">>),
  <<"hello">> = clj_core:namespace(HelloWorldKeyword),
  <<"world">> = clj_core:name(HelloWorldKeyword),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  HelloKeyword = clj_core:keyword(<<"hello">>),
  <<":hello">> = clj_core:str(HelloKeyword),

  HelloWorldKeyword = clj_core:keyword(<<"hello">>, <<"world">>),
  <<":hello/world">> = clj_core:str(HelloWorldKeyword),

  {comments, ""}.
