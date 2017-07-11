-module(clojerl_MultiFn_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ get_method/1
        , get_method_table/1
        , remove_method/1
        , ets_heir/1
        , complete_coverage/1
        ]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_, Config) ->
  ok = 'clojerl.MultiFn':add_method(method_var(), default, default_method),

  HelloSym     = clj_rt:symbol(<<"hello">>),
  HelloSymMeta = clj_rt:with_meta(HelloSym, #{private => true}),
  ok = 'clojerl.MultiFn':add_method( method_var()
                                   , HelloSymMeta
                                   , symbol_method
                                   ),

  Vector = clj_rt:vector([default, HelloSym]),
  ok = 'clojerl.MultiFn':add_method( method_var()
                                   , Vector
                                   , vector_method
                                   ),
  Config.

-spec end_per_testcase(atom(), config()) -> config().
end_per_testcase(_, Config) ->
  'clojerl.MultiFn':remove_all(method_var()),
  Config.

-spec method_var() -> 'clojerl.Var':type().
method_var() ->
  'clojerl.Var':?CONSTRUCTOR( <<"clojerl_MultiFn_SUITE">>
                            , <<"test-method">>
                            ).

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec get_method(config()) -> result().
get_method(_Config) ->
  ct:comment("Method with keyword value"),
  default_method = 'clojerl.MultiFn':get_method(method_var(), default),

  ct:comment("Method with symbol value"),
  HelloSym      = clj_rt:symbol(<<"hello">>),
  HelloSymMeta  = clj_rt:with_meta(HelloSym, #{private => true}),
  symbol_method = 'clojerl.MultiFn':get_method(method_var(), HelloSymMeta),
  symbol_method = 'clojerl.MultiFn':get_method(method_var(), HelloSym),

  ct:comment("Method with vector value"),
  Vector      = clj_rt:vector([default, HelloSym]),
  VectorMeta  = clj_rt:with_meta(Vector, #{some => thing}),
  Vector2     = clj_rt:vector([default, HelloSymMeta]),
  VectorMeta2 = clj_rt:with_meta(Vector2, #{some => thing}),
  vector_method = 'clojerl.MultiFn':get_method(method_var(), Vector),
  vector_method = 'clojerl.MultiFn':get_method(method_var(), VectorMeta),

  ct:comment("When elements differ in metadata the method should be found"),
  true = vector_method =:= 'clojerl.MultiFn':get_method( method_var()
                                                       , Vector2
                                                       ),
  true = vector_method =:= 'clojerl.MultiFn':get_method( method_var()
                                                       , VectorMeta2
                                                       ),

  ct:comment("Some other value will return the default implementation"),
  default_method =
    'clojerl.MultiFn':get_method(method_var(), hello, default, ?NIL),


  ct:comment("Try to get a non-existent method"),
  AnotherVar = 'clojerl.Var':?CONSTRUCTOR(<<"ns">>, <<"doesn't-exists">>),
  ?NIL = 'clojerl.MultiFn':get_method(AnotherVar, whatever),

  {comments, ""}.

-spec get_method_table(config()) -> result().
get_method_table(_Config) ->
  MethodTable = 'clojerl.MultiFn':get_method_table(method_var()),

  HelloSym    = clj_rt:symbol(<<"hello">>),
  Vector      = clj_rt:vector([default, HelloSym]),

  ExpectedTable = #{ default  => default_method
                   , HelloSym => symbol_method
                   , Vector   => vector_method
                   },

  true = clj_rt:equiv(ExpectedTable, MethodTable),

  3 = maps:size(MethodTable),

  {comment, ""}.

-spec remove_method(config()) -> result().
remove_method(_Config) ->
  MethodTable = 'clojerl.MultiFn':get_method_table(method_var()),
  3 = maps:size(MethodTable),

  true = 'clojerl.MultiFn':remove_method(method_var(), default),

  MethodTableAfter = 'clojerl.MultiFn':get_method_table(method_var()),
  2 = maps:size(MethodTableAfter),
  MethodTableAfter = maps:remove(default, MethodTable),

  {comment, ""}.

-spec ets_heir(config()) -> result().
ets_heir(_Config) ->
  GenServerName = 'clojerl.MultiFn',
  MethodsTable  = 'clojerl.MultiFn',
  HeirName      = 'clojerl.MultiFn.Heir',

  ct:comment("Check owner and heir"),
  Heir0  = ets:info(MethodsTable, heir),
  Owner0 = ets:info(MethodsTable, owner),

  Heir0  = erlang:whereis(HeirName),
  Owner0 = erlang:whereis(GenServerName),

  ct:comment("Kill the heir, check a new one is spawned"),
  exit(Heir0, kill),
  %% Give the gen_server some time to create an heir
  timer:sleep(100),
  Heir1 = ets:info(MethodsTable, heir),
  true  = is_pid(Heir1) andalso Heir0 =/= Heir1,

  ct:comment("After killing the owner the contents should be there"),
  ok = gen_server:stop(GenServerName),
  MethodTable = 'clojerl.MultiFn':get_method_table(method_var()),
  3 = maps:size(MethodTable),

  ct:comment("Heir should be the same"),
  Heir1 = ets:info(MethodsTable, heir),

  ct:comment("Owner should be the different"),
  Owner1 = ets:info(MethodsTable, owner),
  true = is_pid(Owner1) andalso Owner0 =/= Owner1,

  {comment, ""}.


-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ProcessName = 'clojerl.MultiFn',

  ct:comment("Force handle_info execution"),
  ProcessName ! ok,

  ct:comment("Force handle_cast execution"),
  gen_server:cast(ProcessName, ok),

  ct:comment("Call code_change"),
  ProcessName:code_change(ok, undefined, undefined),

  {comment, ""}.
