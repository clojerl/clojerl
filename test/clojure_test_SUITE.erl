-module(clojure_test_SUITE).

-export([ all/0
        , init_per_suite/1
        ]).

-export([run/1]).

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

-spec run(config()) -> result().
run(_Config) ->
  PrivPath = clj_test_utils:relative_path(<<"priv/">>),
  true     = code:add_path(binary_to_list(PrivPath)),


  compile(<<"priv/clojure/core.clj">>),
  compile(<<"priv/clojure/main.clj">>),
  'clojure.core':'in-ns'(clj_core:gensym(<<"temp-ns">>)),
  'clojure.core':'use'([clj_core:symbol(<<"clojure.core">>)]),
  compile(<<"priv/examples/run_tests.clj">>),

  TestsPath = clj_test_utils:relative_path(<<"priv/clojure/test_clojure/">>),
  'examples.run-tests':'-main'([TestsPath, PrivPath]),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper
%%------------------------------------------------------------------------------

compile(Path) ->
  RelativePath = clj_test_utils:relative_path(Path),
  clj_compiler:compile_file(RelativePath, #{time => true}).
