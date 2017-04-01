-module(clj_compiler_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
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
  SimplePath = clj_test_utils:relative_path(<<"test/clj/examples/simple.clj">>),
  _Env = clj_compiler:compile_file(SimplePath, Opts),
  check_var_value(<<"examples.simple">>, <<"x">>, 1),

  ct:comment("Try to compile a non-existen file"),
  NotExistsPath =
    clj_test_utils:relative_path(<<"test/clj/examples/abcdef_42.clj">>),
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

  ct:comment("Compile all src/clj/clojure/*.clj files succesfully"),
  Wildcard1 = clj_test_utils:relative_path(<<"src/clj/clojure/core.clj">>),
  Files1    = filelib:wildcard(binary_to_list(Wildcard1)),
  FilesBin1 = lists:map(fun erlang:list_to_binary/1, Files1),
  _ = clj_compiler:compile_files(FilesBin1, Opts),

  ct:comment("Compile two files and use vars from one and the other"),
  SimplePath  = clj_test_utils:relative_path(<<"test/clj/examples/simple.clj">>),
  Simple2Path = clj_test_utils:relative_path(<<"test/clj/examples/simple_2.clj">>),
  _ = clj_compiler:compile_files([SimplePath, Simple2Path], Opts),

  check_var_value(<<"examples.simple-2">>, <<"x">>, 1),

  ct:comment("Compile all src/clj/examples/*.clj files succesfully"),
  Wildcard2 = clj_test_utils:relative_path(<<"test/clj/examples/*.clj">>),
  Files2    = filelib:wildcard(binary_to_list(Wildcard2)),
  ErrorPath = clj_test_utils:relative_path(<<"test/clj/examples/error.clj">>),
  FilesBin2 = lists:map(fun list_to_binary/1, Files2) -- [ErrorPath],
  _Env2 = clj_compiler:compile_files(FilesBin2, Opts),

  {comments, ""}.

-spec eval(config()) -> result().
eval(_Config) ->
  ct:comment("Eval form"),
  {1, _} = clj_compiler:eval(1),

  ct:comment("Define a var"),
  DefList = clj_reader:read(<<"(def hello :world)">>),
  {Var, _} = clj_compiler:eval(DefList),
  Var = find_var(<<"clojure.core">>, <<"hello">>),

  check_var_value(<<"clojure.core">>, <<"hello">>, world),

  ct:comment("Current namespace is changed"),
  {_, _}  = clj_compiler:eval(clj_reader:read(<<"(ns a)">>)),
  <<"a">> = clj_core:str(clj_namespace:name(clj_namespace:current())),

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
  Value = clj_core:deref(Var).

-spec find_var(binary(), binary()) -> 'clojerl.Var':type().
find_var(Namespace, Name) ->
  Symbol = clj_core:symbol(Namespace, Name),
  case clj_namespace:find_var(Symbol) of
    ?NIL -> error;
    V -> V
  end.
