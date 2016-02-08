-module(clojerl_Symbol_SUITE).

-export([all/0, init_per_suite/1]).

-export([ equiv/1
        , meta/1
        , name/1
        , str/1
        , to_atom/1
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

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that symbols with the same names"),
  Symbol1 = clj_core:with_meta(clj_core:symbol(<<"hello">>), #{a => 1}),
  Symbol2 = clj_core:with_meta(clj_core:symbol(<<"hello">>), #{b => 2}),

  true  = clj_core:equiv(Symbol1, Symbol2),

  ct:comment("Check that symbols with the differente names are not equivalent"),
  Symbol3 = clj_core:with_meta(clj_core:symbol(<<"hello-world">>), #{c => 3}),
  false = clj_core:equiv(Symbol1, Symbol3),

  ct:comment("A clojerl.Symbol and something else"),
  false = clj_core:equiv(Symbol1, whatever),
  false = clj_core:equiv(Symbol1, #{}),

  {comments, ""}.

-spec meta(config()) -> result().
meta(_Config) ->
  Symbol3   = clj_core:with_meta(clj_core:symbol(<<"hello-world">>), #{c => 3}),
  #{c := 3} = clj_core:meta(Symbol3),

  {comments, ""}.

-spec name(config()) -> result().
name(_Config) ->
  HelloSymbol = clj_core:symbol(<<"hello">>),
  undefined   = clj_core:namespace(HelloSymbol),
  <<"hello">> = clj_core:name(HelloSymbol),

  HelloWorldSymbol = clj_core:symbol(<<"hello">>, <<"world">>),
  <<"hello">> = clj_core:namespace(HelloWorldSymbol),
  <<"world">> = clj_core:name(HelloWorldSymbol),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  HelloSymbol = clj_core:symbol(<<"hello">>),
  <<"hello">> = clj_core:str(HelloSymbol),

  HelloWorldSymbol = clj_core:symbol(<<"hello">>, <<"world">>),
  <<"hello/world">> = clj_core:str(HelloWorldSymbol),

  {comments, ""}.

-spec to_atom(config()) -> result().
to_atom(_Config) ->
  HelloSymbol = clj_core:symbol(<<"hello">>),
  hello = 'clojerl.Symbol':to_atom(HelloSymbol),

  HelloWorldSymbol = clj_core:symbol(<<"hello">>, <<"world">>),
  'hello/world' = 'clojerl.Symbol':to_atom(HelloWorldSymbol),

  {comments, ""}.
