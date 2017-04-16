-module(clojerl_Symbol_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ equiv/1
        , hash/1
        , apply/1
        , meta/1
        , name/1
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

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that symbols with the same names"),
  Symbol1 = clj_rt:with_meta(clj_rt:symbol(<<"hello">>), #{a => 1}),
  Symbol2 = clj_rt:with_meta(clj_rt:symbol(<<"hello">>), #{b => 2}),

  true  = clj_rt:equiv(Symbol1, Symbol2),

  ct:comment("Check that symbols with the differente names are not equivalent"),
  Symbol3 = clj_rt:with_meta(clj_rt:symbol(<<"hello-world">>), #{c => 3}),
  false = clj_rt:equiv(Symbol1, Symbol3),

  ct:comment("A clojerl.Symbol and something else"),
  false = clj_rt:equiv(Symbol1, whatever),
  false = clj_rt:equiv(Symbol1, #{}),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  HelloSymbol = clj_rt:symbol(<<"hello">>),
  Hash1 = 'clojerl.IHash':hash(HelloSymbol),
  Hash1 = 'clojerl.IHash':hash(HelloSymbol),

  WorldSymbol = clj_rt:symbol(<<"world">>),
  Hash2 = 'clojerl.IHash':hash(WorldSymbol),

  true = Hash1 =/= Hash2,

  {comments, ""}.

-spec apply(config()) -> result().
apply(_Config) ->
  HelloSymbol = clj_rt:symbol(<<"hello">>),
  world = clj_rt:apply(HelloSymbol, [#{HelloSymbol => world}]),

  ?NIL      = clj_rt:apply(HelloSymbol, [#{bla => ble}]),
  not_found = clj_rt:apply(HelloSymbol, [#{bla => ble}, not_found]),

  ok = try
         clj_rt:apply(HelloSymbol, [#{bla => ble}, not_found, extra]),
         error
       catch _:_ ->
           ok
       end,

  {comments, ""}.

-spec meta(config()) -> result().
meta(_Config) ->
  Symbol3   = clj_rt:with_meta(clj_rt:symbol(<<"hello-world">>), #{c => 3}),
  #{c := 3} = clj_rt:meta(Symbol3),

  {comments, ""}.

-spec name(config()) -> result().
name(_Config) ->
  HelloSymbol = clj_rt:symbol(<<"hello">>),
  ?NIL        = clj_rt:namespace(HelloSymbol),
  <<"hello">> = clj_rt:name(HelloSymbol),

  HelloWorldSymbol = clj_rt:symbol(<<"hello">>, <<"world">>),
  <<"hello">> = clj_rt:namespace(HelloWorldSymbol),
  <<"world">> = clj_rt:name(HelloWorldSymbol),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  HelloSymbol = clj_rt:symbol(<<"hello">>),
  <<"hello">> = clj_rt:str(HelloSymbol),

  HelloWorldSymbol = clj_rt:symbol(<<"hello">>, <<"world">>),
  <<"hello/world">> = clj_rt:str(HelloWorldSymbol),

  {comments, ""}.
