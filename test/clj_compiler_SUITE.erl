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
        , file/1
        , eval/1
        , def_var_compiled_ns/1
        ]).

-define( EXCLUDE_FILES
       , [ <<"scripts/examples/error.clje">>
         , <<"scripts/examples/deftype.clje">>
         , <<"scripts/examples/lazy_seq.clje">>
         , <<"scripts/examples/ns.clje">>
         , <<"scripts/examples/protocols.clje">>
         , <<"scripts/examples/some_ns/two.clje">>
         , <<"scripts/examples/some_ns/three.clje">>
         ]
       ).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

-spec init_per_testcase(_, config()) -> config().
init_per_testcase(_, Config) ->
  Bindings = #{ <<"#'clojure.core/*compile-files*">>          => true
              , <<"#'clojure.core/*compile-path*">>           => <<"ebin">>
              , <<"#'clojure.core/*compile-protocols-path*">> => <<"ebin">>
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
  _Env = clj_compiler:string(<<"(ns src) (def y :hello-world) 1">>),
  check_var_value(<<"src">>, <<"y">>, 'hello-world'),

  ct:comment("Try to compile invalid code"),
  ok = try
         clj_compiler:string(<<"(ns hello) (def 42 :forty-two)">>),
         error
       catch _:_ ->
           ok
       end,

  {comments, ""}.

-spec file(config()) -> result().
file(_Config) ->
  Opts = #{time => true},
  Dir  = <<"scripts/examples">>,
  ct:comment("Compile a file and check a var's value by deref'ing it"),
  SimplePath = clj_test_utils:relative_path(<<Dir/binary, "/simple.clje">>),
  _Env = clj_compiler:file(SimplePath, Opts),
  check_var_value(<<"examples.simple">>, <<"x">>, 1),

  ct:comment("Try to compile a non-existen file"),
  NotExistsPath =
    clj_test_utils:relative_path(<<Dir/binary, "/abcdef_42.clje">>),
  ok = try clj_compiler:file(NotExistsPath, Opts), error
       catch _:_ -> ok end,

  Opts     = #{time => true},
  SrcPath  = clj_test_utils:relative_path(<<"src/clj">>),
  TestPath = clj_test_utils:relative_path(<<"scripts">>),
  true     = code:add_path(binary_to_list(SrcPath)),
  true     = code:add_path(binary_to_list(TestPath)),

  ct:comment("Compile two files and use vars from one and the other"),
  SimplePath  = <<TestPath/binary, "/examples/simple.clje">>,
  Simple2Path = <<TestPath/binary, "/examples/simple_2.clje">>,
  [clj_compiler:file(F, Opts) || F <- [SimplePath, Simple2Path]],

  check_var_value(<<"examples.simple-2">>, <<"x">>, 1),

  ct:comment("Compile all scripts/examples/*.clje files successfully"),
  Wildcard2 = clj_test_utils:relative_path(<<"scripts/examples/**/*.clje">>),
  Files2    = filelib:wildcard(binary_to_list(Wildcard2)),
  Exclude   = [clj_test_utils:relative_path(Path) || Path <- ?EXCLUDE_FILES],
  FilesBin2 = lists:map(fun list_to_binary/1, Files2) -- Exclude,
  [clj_compiler:file(F, Opts) || F <- FilesBin2],

  {comments, ""}.

-spec eval(config()) -> result().
eval(_Config) ->
  'clojure.core':'in-ns'(clj_rt:symbol(<<"test.clj_compiler">>)),

  ct:comment("Eval form"),
  {1, _} = clj_compiler:eval(1),

  ct:comment("Define a var"),
  DefList = clj_reader:read(<<"(def hello :world)">>),
  {Var0, _} = clj_compiler:eval(DefList),
  Var1 = find_var(<<"test.clj_compiler">>, <<"hello">>),
  true = clj_rt:equiv(Var0, Var1),

  check_var_value(<<"test.clj_compiler">>, <<"hello">>, world),

  ct:comment("Current namespace is changed"),
  {_, _}  = clj_compiler:eval(clj_reader:read(<<"(ns a)">>)),
  <<"a">> = clj_rt:str('clojerl.Namespace':name('clojerl.Namespace':current())),

  ct:comment("(erlang/self) should be this process"),
  Self      = erlang:self(),
  {Self, _} = clj_compiler:eval(clj_reader:read(<<"(erlang/self)">>)),

  ct:comment("in-ns and ns should be available"),
  {_, _}  = clj_compiler:eval(clj_reader:read(<<"(in-ns 'b)">>)),
  ok      = try clj_compiler:eval(clj_reader:read(<<"in-ns">>)), ok
            catch _:_ -> error
            end,
  ok      = try clj_compiler:eval(clj_reader:read(<<"ns">>)), ok
            catch _:_ -> error
            end,

  {comments, ""}.

-spec def_var_compiled_ns(config()) -> result().
def_var_compiled_ns(_Config) ->
  ct:comment("Defining new var in compiled namespace is persisted"),
  _ = clj_compiler:string(<<"(ns examples.simple) (def foo 2)">>),
  _ = clj_compiler:string(<<"(ns examples.simple) (def x 1)">>),
  check_var_value(<<"examples.simple">>, <<"foo">>, 2),

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
  case 'clojerl.Var':find(Symbol) of
    ?NIL -> error;
    V -> V
  end.
