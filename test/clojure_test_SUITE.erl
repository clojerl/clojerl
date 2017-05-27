-module(clojure_test_SUITE).

-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([run/1]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

-spec init_per_testcase(_, config()) -> config().
init_per_testcase(_, Config) ->
  Bindings = #{ <<"#'clojure.core/*compile-files*">> => true
              , <<"#'clojure.core/*compile-path*">> => <<"ebin">>
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

-spec run(config()) -> result().
run(_Config) ->
  SrcPath  = clj_test_utils:relative_path(<<"src/clj/">>),
  RootPath = clj_test_utils:relative_path(<<"test/clj/">>),
  true     = code:add_path(binary_to_list(SrcPath)),
  true     = code:add_path(binary_to_list(RootPath)),

  compile(<<"src/clj/clojure/core.clje">>),
  compile(<<"src/clj/clojure/main.clje">>),
  'clojure.core':'in-ns'(clj_rt:gensym(<<"temp-ns">>)),
  'clojure.core':'use'([clj_rt:symbol(<<"clojure.core">>)]),
  compile(<<"test/clj/examples/run_tests.clje">>),

  TestsPath = <<RootPath/binary, "/clojure/test_clojure/">>,
  Result    = 'examples.run-tests':'-main'([TestsPath, RootPath]),

  0 = clj_rt:get(Result, fail),
  %% There are two tests that fail because of atoms not being implemented
  2 = clj_rt:get(Result, error),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper
%%------------------------------------------------------------------------------

compile(Path) ->
  RelativePath = clj_test_utils:relative_path(Path),
  clj_compiler:compile_file(RelativePath, #{time => true}).
