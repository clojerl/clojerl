-module(clj_multimethod_SUITE).

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
        ]).

-export([ 'test-method__val'/0
        , 'test-method'/0
        , 'test-method-map__val'/0
        ]).

-spec 'test-method__val'() -> any().
'test-method__val'() ->
  'clojerl.Var':?CONSTRUCTOR( <<"clj_multimethod_SUITE">>
                            , <<"test-method">>
                            ).

-spec 'test-method-map__val'() -> any().
'test-method-map__val'() ->
  'clojerl.Map':?CONSTRUCTOR([]).

-spec 'test-method'() -> any().
'test-method'() -> 42.

%%------------------------------------------------------------------------------
%% Common Test callbacks
%%------------------------------------------------------------------------------

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_, Config) ->
  clj_multimethod:init(method_var()),
  clj_multimethod:add_method(method_var(), default, default_method),

  HelloSym     = clj_rt:symbol(<<"hello">>),
  HelloSymMeta = clj_rt:with_meta(HelloSym, #{private => true}),
  clj_multimethod:add_method( method_var()
                              , HelloSymMeta
                              , symbol_method
                              ),

  Vector = clj_rt:vector([default, HelloSym]),
  clj_multimethod:add_method( method_var()
                              , Vector
                              , vector_method
                              ),
  Config.

-spec end_per_testcase(atom(), config()) -> config().
end_per_testcase(_, Config) ->
  clj_multimethod:remove_all(method_var()),
  Config.

-spec method_var() -> 'clojerl.Var':type().
method_var() ->
  'clojerl.Var':?CONSTRUCTOR( <<"clj_multimethod_SUITE">>
                            , <<"test-method">>
                            ).

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec get_method(config()) -> result().
get_method(_Config) ->
  ct:comment("Method with keyword value"),
  default_method = clj_multimethod:get_method(method_var(), default),

  ct:comment("Method with symbol value"),
  HelloSym      = clj_rt:symbol(<<"hello">>),
  HelloSymMeta  = clj_rt:with_meta(HelloSym, #{private => true}),
  symbol_method = clj_multimethod:get_method(method_var(), HelloSymMeta),
  symbol_method = clj_multimethod:get_method(method_var(), HelloSym),

  ct:comment("Method with vector value"),
  Vector      = clj_rt:vector([default, HelloSym]),
  VectorMeta  = clj_rt:with_meta(Vector, #{some => thing}),
  Vector2     = clj_rt:vector([default, HelloSymMeta]),
  VectorMeta2 = clj_rt:with_meta(Vector2, #{some => thing}),
  vector_method = clj_multimethod:get_method(method_var(), Vector),
  vector_method = clj_multimethod:get_method(method_var(), VectorMeta),

  ct:comment("When elements differ in metadata the method should be found"),
  true = vector_method =:= clj_multimethod:get_method( method_var()
                                                       , Vector2
                                                       ),
  true = vector_method =:= clj_multimethod:get_method( method_var()
                                                       , VectorMeta2
                                                       ),

  ct:comment("Some other value will return the default implementation"),
  default_method =
    clj_multimethod:get_method(method_var(), hello, default, ?NIL),


  ct:comment("Try to get a non-existent method"),
  AnotherVar = 'clojerl.Var':?CONSTRUCTOR(<<"ns">>, <<"doesn't-exists">>),
  ok = try clj_multimethod:get_method(AnotherVar, whatever), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec get_method_table(config()) -> result().
get_method_table(_Config) ->
  MethodTable = clj_multimethod:get_method_table(method_var()),

  HelloSym    = clj_rt:symbol(<<"hello">>),
  Vector      = clj_rt:vector([default, HelloSym]),

  ExpectedTable = #{ default  => default_method
                   , HelloSym => symbol_method
                   , Vector   => vector_method
                   },

  true = clj_rt:equiv(ExpectedTable, MethodTable),

  3 = clj_rt:count(MethodTable),

  {comment, ""}.

-spec remove_method(config()) -> result().
remove_method(_Config) ->
  MethodTable = clj_multimethod:get_method_table(method_var()),
  3 = clj_rt:count(MethodTable),

  _ = clj_multimethod:remove_method(method_var(), default),

  MethodTableAfter = clj_multimethod:get_method_table(method_var()),
  2 = clj_rt:count(MethodTableAfter),
  true = clj_rt:equiv(MethodTableAfter, clj_rt:dissoc(MethodTable, default)),

  {comment, ""}.
