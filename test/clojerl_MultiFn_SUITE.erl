-module(clojerl_MultiFn_SUITE).

-export([ all/0
        , init_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ get_method/1
        , get_method_table/1
        , remove_method/1
        , complete_coverage/1
        ]).

-type config() :: list().
-type result() :: {comments, string()}.

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [ init_per_suite
                 , end_per_suite
                 , init_per_testcase
                 , end_per_testcase
                 , all
                 , module_info
                 ],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(clojerl),
  Config.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_, Config) ->
  ok = 'clojerl.MultiFn':add_method(<<"test-method">>, default, default_method),

  HelloSym     = clj_core:symbol(<<"hello">>),
  HelloSymMeta = clj_core:with_meta(HelloSym, #{private => true}),
  ok = 'clojerl.MultiFn':add_method( <<"test-method">>
                                   , HelloSymMeta
                                   , symbol_method
                                   ),

  Vector = clj_core:vector([default, HelloSym]),
  ok = 'clojerl.MultiFn':add_method( <<"test-method">>
                                   , Vector
                                   , vector_method
                                   ),
  Config.

-spec end_per_testcase(atom(), config()) -> config().
end_per_testcase(_, Config) ->
  'clojerl.MultiFn':remove_all(<<"test-method">>),
  Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec get_method(config()) -> result().
get_method(_Config) ->
  ct:comment("Method with keyword value"),
  default_method = 'clojerl.MultiFn':get_method(<<"test-method">>, default),

  ct:comment("Method with symbol value"),
  HelloSym      = clj_core:symbol(<<"hello">>),
  HelloSymMeta  = clj_core:with_meta(HelloSym, #{private => true}),
  symbol_method = 'clojerl.MultiFn':get_method(<<"test-method">>, HelloSymMeta),
  symbol_method = 'clojerl.MultiFn':get_method(<<"test-method">>, HelloSym),

  ct:comment("Method with vector value"),
  Vector      = clj_core:vector([default, HelloSym]),
  VectorMeta  = clj_core:with_meta(Vector, #{some => thing}),
  Vector2     = clj_core:vector([default, HelloSymMeta]),
  VectorMeta2 = clj_core:with_meta(Vector2, #{some => thing}),
  vector_method = 'clojerl.MultiFn':get_method(<<"test-method">>, Vector),
  vector_method = 'clojerl.MultiFn':get_method(<<"test-method">>, VectorMeta),

  ct:comment("When elements differ in metadata the method should be found"),
  true = vector_method =:= 'clojerl.MultiFn':get_method( <<"test-method">>
                                                        , Vector2
                                                        ),
  true = vector_method =:= 'clojerl.MultiFn':get_method( <<"test-method">>
                                                        , VectorMeta2
                                                        ),

  ct:comment("Some other value will return the default implementation"),
  default_method =
    'clojerl.MultiFn':get_method(<<"test-method">>, hello, default, undefined),


  ct:comment("Try to get a non-existent method"),
  undefined = 'clojerl.MultiFn':get_method( <<"doesn't-exists">>, whatever),

  {comments, ""}.

-spec get_method_table(config()) -> result().
get_method_table(_Config) ->
  MethodTable = 'clojerl.MultiFn':get_method_table(<<"test-method">>),

  HelloSym    = clj_core:symbol(<<"hello">>),
  Vector      = clj_core:vector([default, HelloSym]),

  ExpectedTable = #{ default  => default_method
                   , HelloSym => symbol_method
                   , Vector   => vector_method
                   },

  true = clj_core:equiv(ExpectedTable, MethodTable),

  3 = maps:size(MethodTable),

  {comment, ""}.

-spec remove_method(config()) -> result().
remove_method(_Config) ->
  MethodTable = 'clojerl.MultiFn':get_method_table(<<"test-method">>),
  3 = maps:size(MethodTable),

  true = 'clojerl.MultiFn':remove_method(<<"test-method">>, default),

  MethodTableAfter = 'clojerl.MultiFn':get_method_table(<<"test-method">>),
  2 = maps:size(MethodTableAfter),
  MethodTableAfter = maps:remove(default, MethodTable),

  {comment, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ProcessName = 'clojerl.MultiFn',

  ct:comment("Force handle_info execution"),
  ProcessName ! ok,

  ct:comment("Force handle_cast execution"),
  gen_server:cast(ProcessName, ok),

  ct:comment("Force terminate execution"),
  gen_server:stop(ProcessName),

  ct:comment("Call code_change"),
  ProcessName:code_change(ok, undefined, undefined),

  {comment, ""}.
