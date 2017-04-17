-module(clj_compiler_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ compile/1
        , compile_file/1
        , compile_files/1
        , eval/1
        ]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

-spec init_per_testcase(_, config()) -> config().
init_per_testcase(_, Config) ->
  Bindings = #{ <<"#'clojure.core/*compile-files*">> => true
              , <<"#'clojure.core/*compile-path*">> => "ebin"
              },
  ok       = 'clojerl.Var':push_bindings(Bindings),
  Config.

-spec end_per_testcase(_, config()) -> config().
end_per_testcase(_, Config) ->
  'clojerl.Var':pop_bindings(),
  Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec compile(config()) -> result().
compile(_Config) ->
  ct:comment("Compile code and check a var's value by deref'ing it"),
  _Env = clj_compiler:compile(<<"(ns src) (def y :hello-world) 1">>),
  check_var_value(<<"src">>, <<"y">>, 'hello-world'),

  ct:comment("Try to compile invalid code"),
  ok = try
         clj_compiler:compile(<<"(ns hello) (def 42 :forty-two)">>),
         error
       catch _:_ ->
           ok
       end,

  {comments, ""}.

-spec compile_file(config()) -> result().
compile_file(_Config) ->
  Opts = #{verbose => true, time => true},

  ct:comment("Compile a file and check a var's value by deref'ing it"),
  SimplePath = clj_test_utils:relative_path(<<"test/clj/examples/simple.clje">>),
  _Env = clj_compiler:compile_file(SimplePath, Opts),
  check_var_value(<<"examples.simple">>, <<"x">>, 1),

  ct:comment("Try to compile a non-existen file"),
  NotExistsPath =
    clj_test_utils:relative_path(<<"test/clj/examples/abcdef_42.clje">>),
  ok = try clj_compiler:compile_file(NotExistsPath, Opts), error
       catch _:_ -> ok end,

  {comments, ""}.

-spec compile_files(config()) -> result().
compile_files(_Config) ->
  Opts     = #{verbose => true, time => true},
  SrcPath  = clj_test_utils:relative_path(<<"src/clj">>),
  TestPath = clj_test_utils:relative_path(<<"test/clj">>),
  true     = code:add_path(binary_to_list(SrcPath)),
  true     = code:add_path(binary_to_list(TestPath)),

  ct:comment("Compile two files and use vars from one and the other"),
  SimplePath  = <<TestPath/binary, "/examples/simple.clje">>,
  Simple2Path = <<TestPath/binary, "/examples/simple_2.clje">>,
  _ = clj_compiler:compile_files([SimplePath, Simple2Path], Opts),

  check_var_value(<<"examples.simple-2">>, <<"x">>, 1),

  ct:comment("Compile all src/clj/examples/*.clj files succesfully"),
  Wildcard2 = clj_test_utils:relative_path(<<"test/clj/examples/**/*.clje">>),
  Files2    = filelib:wildcard(binary_to_list(Wildcard2)),
  ErrorPath = clj_test_utils:relative_path(<<"test/clj/examples/error.clje">>),
  FilesBin2 = lists:map(fun list_to_binary/1, Files2) -- [ErrorPath],
  _Env2 = clj_compiler:compile_files(FilesBin2, Opts),

  {comments, ""}.

-spec eval(config()) -> result().
eval(_Config) ->
  'clojure.core':'in-ns'(clj_rt:symbol(<<"test.clj_compiler">>)),

  ct:comment("Eval form"),
  {1, _} = clj_compiler:eval(1),

  ct:comment("Define a var"),
  DefList = clj_reader:read(<<"(def hello :world)">>),
  {Var, _} = clj_compiler:eval(DefList),
  Var = find_var(<<"test.clj_compiler">>, <<"hello">>),

  check_var_value(<<"test.clj_compiler">>, <<"hello">>, world),

  ct:comment("Current namespace is changed"),
  {_, _}  = clj_compiler:eval(clj_reader:read(<<"(ns a)">>)),
  <<"a">> = clj_rt:str(clj_namespace:name(clj_namespace:current())),

  ct:comment("(erlang/self) should be this process"),
  Self      = erlang:self(),
  {Self, _} = clj_compiler:eval(clj_reader:read(<<"(erlang/self.e)">>)),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec check_var_value(binary(), binary(), any()) -> any().
check_var_value(Namespace, Name, Value) ->
  Var   = find_var(Namespace, Name),
  Value = clj_rt:deref(Var).

-spec find_var(binary(), binary()) -> 'clojerl.Var':type().
find_var(Namespace, Name) ->
  Symbol = clj_rt:symbol(Namespace, Name),
  case clj_namespace:find_var(Symbol) of
    ?NIL -> error;
    V -> V
  end.
