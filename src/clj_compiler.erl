-module(clj_compiler).

-export([
         compile_files/2,
         compile_file/2,
         compile/2,
         eval/1,
         eval/2,
         eval/3
        ]).

-export([ast_to_string/1]).

-type options() :: #{ output_dir => binary()
                    , flags      => [atom()]
                    }.

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------

-spec default_options() -> map().
default_options() ->
  #{ output_dir => <<"ebin">>
   , flags      => [debug_info, verbose,report_errors,report_warnings]
   }.

-spec compile_files([file:filename_all()], options()) -> clj_env:env().
compile_files(Files, Opts) when is_list(Files) ->
  compile_files(Files, Opts, clj_env:default()).

-spec compile_files([file:filename_all()], options(), clj_env:env()) ->
  clj_env:env().
compile_files(Files, Opts, Env) when is_list(Files) ->
  Fun = fun(File, EnvAcc) -> compile_file(File, Opts, EnvAcc) end,
  lists:foldl(Fun, Env, Files).

-spec compile_file(file:filename_all(), options()) -> clj_env:env().
compile_file(File, Opts) when is_binary(File) ->
  compile_file(File, Opts, clj_env:default()).

-spec compile_file(file:filename_all(), options(), clj_env:env()) ->
  clj_env:env().
compile_file(File, Opts, Env) when is_binary(File) ->
  case file:read_file(File) of
    {ok, Src} -> compile(Src, Opts, Env);
    Error -> throw(Error)
  end.

-spec compile(binary(), options()) -> clj_env:env().
compile(Src, Opts) when is_binary(Src) ->
  compile(Src, Opts, clj_env:default()).

-spec compile(binary(), options(), clj_env:env()) -> clj_env:env().
compile(Src, Opts0, Env) when is_binary(Src) ->
  Opts = maps:merge(default_options(), Opts0),
  Fun = fun(Form, EnvAcc) ->
            NewEnvAcc = clj_analyzer:analyze(EnvAcc, Form),
            {ModulesForms, Exprs, Env1} = clj_emitter:emit(NewEnvAcc),
            CompileFormsFun = fun(Forms) -> compile_forms(Forms, Opts) end,
            lists:foreach(CompileFormsFun, ModulesForms),
            eval_expressions(Exprs),
            Env1
        end,
  clj_reader:read_fold(Fun, Src, #{}, Env).

-spec compile_forms([erl_parse:abstract_form()], options()) ->
  atom() | undefined.
compile_forms([], _) ->
  undefined;
compile_forms(Forms, Opts) ->
  %% io:format("==== FORMS ====~n~s~n", [ast_to_string(Forms)]),
  Flags = maps:get(flags, Opts),
  case compile:forms(Forms, Flags) of
    {ok, Name, Binary} ->
      OutputDir = maps:get(output_dir, Opts),
      BeamFilename = <<(atom_to_binary(Name, utf8))/binary, ".beam">>,
      BeamPath = filename:join([OutputDir, BeamFilename]),
      ok = file:write_file(BeamPath, Binary),
      {module, Name} = code:load_binary(Name, binary_to_list(BeamPath), Binary),
      Name;
    Error ->
      throw(Error)
  end.

-spec eval_expressions([erl_parse:abstract_expr()]) -> [any()].
eval_expressions([]) ->
  [];
eval_expressions(Expressions) ->
  %% io:format("==== EXPR ====~n~s~n", [ast_to_string(Expressions)]),
  {Values, _} = erl_eval:expr_list(Expressions, []),
  Values.

-spec ast_to_string([erl_syntax:syntaxTree()]) -> string().
ast_to_string(Forms) ->
  erl_prettypr:format(erl_syntax:form_list(Forms)).

-spec eval(any()) -> {any(), clj_env:env()}.
eval(Form) ->
  eval(Form, default_options()).

-spec eval(any(), options()) -> {any(), clj_env:env()}.
eval(Form, Opts) ->
  eval(Form, Opts, clj_env:default()).

-spec eval(any(), options(), clj_env:env()) -> {any(), clj_env:env()}.
eval(Form, Opts0, Env) ->
  Opts = maps:merge(default_options(), Opts0),

  NewEnv = clj_analyzer:analyze(Env, Form),
  {ModulesForms, Exprs, Env1} = clj_emitter:emit(NewEnv),

  CompileFormsFun = fun(Forms) -> compile_forms(Forms, Opts) end,
  lists:foreach(CompileFormsFun, ModulesForms),

  [Value] = eval_expressions(Exprs),
  {Value, Env1}.
