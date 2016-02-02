-module(clj_compiler_SUITE).

-export([all/0]).

-export([ compile/1
        , compile_file/1
        , compile_files/1
        , eval/1
        ]).

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-type config() :: list().
-type result() :: {comments, string()}.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec compile(config()) -> result().
compile(_Config) ->
  Opts = options(),

  ct:comment("Compile code and check a var's value by deref'ing it"),
  Env = clj_compiler:compile(<<"(ns src) (def y :hello-world) 1">>, Opts),
  check_var_value(Env, <<"src">>, <<"y">>, 'hello-world'),

  ct:comment("Try to compile invalid code"),
  ok = try
         clj_compiler:compile(<<"(ns hello) (def 42 :forty-two)">>, Opts),
         error
       catch _:_ ->
           ok
       end,

  {comments, ""}.

-spec compile_file(config()) -> result().
compile_file(_Config) ->
  Opts = options(),

  ct:comment("Compile a file and check a var's value by deref'ing it"),
  SimplePath = relative_path(<<"priv/examples/simple.clj">>),
  Env = clj_compiler:compile_file(SimplePath, Opts),
  check_var_value(Env, <<"examples.simple">>, <<"x">>, 1),

  ct:comment("Try to compile an invalid file"),
  ErrorPath = relative_path(<<"priv/examples/error.clj">>),
  ok = try clj_compiler:compile_file(ErrorPath, Opts), error
       catch _:_ -> ok end,

  ct:comment("Try to compile a non-existen file"),
  NotExistsPath = relative_path(<<"priv/examples/abcdef_42.clj">>),
  ok = try clj_compiler:compile_file(NotExistsPath, Opts), error
       catch _:_ -> ok end,

  {comments, ""}.

-spec compile_files(config()) -> result().
compile_files(_Config) ->
  Opts = options(),

  ct:comment("Compile two files and use vars from one and the other"),
  SimplePath = relative_path(<<"priv/examples/simple.clj">>),
  Simple2Path = relative_path(<<"priv/examples/simple-2.clj">>),
  Env = clj_compiler:compile_files([SimplePath, Simple2Path], Opts),

  check_var_value(Env, <<"examples.simple-2">>, <<"x">>, 1),

  {comments, ""}.

-spec eval(config()) -> result().
eval(_Config) ->
  Opts = options(),

  ct:comment("Eval form"),
  {1, _} = clj_compiler:eval(1, Opts),

  DefList = clj_reader:read(<<"(def hello :world)">>),
  {Var, Env} = clj_compiler:eval(DefList, Opts),
  Var = find_var(Env, <<"$user">>, <<"hello">>),
  check_var_value(Env, <<"$user">>, <<"hello">>, world),
  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec relative_path(binary()) -> binary().
relative_path(Path) -> <<"../../", Path/binary>>.

-spec check_var_value(clj_env:env(), binary(), binary(), any()) -> any().
check_var_value(Env, Namespace, Name, Value) ->
  Var   = find_var(Env, Namespace, Name),
  Value = clj_core:deref(Var).

-spec find_var(clj_env:env(), binary(), binary()) -> 'clojerl.Var':type().
find_var(Env, Namespace, Name) ->
  Symbol = clj_core:symbol(Namespace, Name),
  case clj_env:find_var(Env, Symbol) of
    undefined -> error;
    V -> V
  end.

-spec options() -> binary().
options()->
  #{output_dir => relative_path(<<"ebin">>)}.
