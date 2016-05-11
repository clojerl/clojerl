-module(clj_compiler_SUITE).

-export([ all/0
        , init_per_suite/1
        ]).

-export([ compile/1
        , compile_file/1
        , compile_files/1
        , eval/1
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
  SimplePath = relative_path(<<"priv/examples/simple.clj">>),
  _Env = clj_compiler:compile_file(SimplePath, Opts),
  check_var_value(<<"examples.simple">>, <<"x">>, 1),

  ct:comment("Try to compile an invalid file"),
  ErrorPath = relative_path(<<"priv/examples/error.clj">>),
  ok = try
         clj_compiler:compile_file(ErrorPath, Opts), error
       catch
         _:<<"error.clj:", _/binary>> -> ok
       end,

  ct:comment("Try to compile a non-existen file"),
  NotExistsPath = relative_path(<<"priv/examples/abcdef_42.clj">>),
  ok = try clj_compiler:compile_file(NotExistsPath, Opts), error
       catch _:_ -> ok end,

  {comments, ""}.

-spec compile_files(config()) -> result().
compile_files(_Config) ->
  Opts     = #{verbose => true, time => true},
  PrivPath = relative_path(<<"priv">>),
  true     = code:add_path(binary_to_list(PrivPath)),

  ct:comment("Compile all priv/clojure/*.clj files succesfully"),
  Wildcard1 = relative_path(<<"priv/clojure/core.clj">>),
  Files1    = filelib:wildcard(binary_to_list(Wildcard1)),
  FilesBin1 = lists:map(fun erlang:list_to_binary/1, Files1),
  _ = clj_compiler:compile_files(FilesBin1, Opts),

  ct:comment("Compile two files and use vars from one and the other"),
  SimplePath  = relative_path(<<"priv/examples/simple.clj">>),
  Simple2Path = relative_path(<<"priv/examples/simple-2.clj">>),
  _ = clj_compiler:compile_files([SimplePath, Simple2Path], Opts),

  check_var_value(<<"examples.simple-2">>, <<"x">>, 1),

  ct:comment("Compile all priv/examples/*.clj files succesfully"),
  Wildcard2 = relative_path(<<"priv/examples/*.clj">>),
  Files2    = filelib:wildcard(binary_to_list(Wildcard2)),
  ErrorPath = relative_path(<<"priv/examples/error.clj">>),
  FilesBin2 = lists:map(fun list_to_binary/1, Files2) -- [ErrorPath],
  _Env2 = clj_compiler:compile_files(FilesBin2, Opts),

  {comments, ""}.

-spec eval(config()) -> result().
eval(_Config) ->
  ct:comment("Eval form"),
  {1, _} = clj_compiler:eval(1),

  DefList = clj_reader:read(<<"(def hello :world)">>),
  {Var, _} = clj_compiler:eval(DefList),
  Var = find_var(<<"clojure.core">>, <<"hello">>),

  check_var_value(<<"clojure.core">>, <<"hello">>, world),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec relative_path(binary()) -> binary().
relative_path(Path) -> <<"../../", Path/binary>>.

-spec check_var_value(binary(), binary(), any()) -> any().
check_var_value(Namespace, Name, Value) ->
  Var   = find_var(Namespace, Name),
  Value = clj_core:deref(Var).

-spec find_var(binary(), binary()) -> 'clojerl.Var':type().
find_var(Namespace, Name) ->
  Symbol = clj_core:symbol(Namespace, Name),
  case clj_namespace:find_var(Symbol) of
    undefined -> error;
    V -> V
  end.
