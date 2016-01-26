-module(clj_compiler).

-export([
         compile_files/1,
         compile_file/1,
         compile/1,
         eval/1,
         eval/2
        ]).

-export([ast_to_string/1]).

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------

-spec compile_files([file:filename_all()]) -> clj_env:env().
compile_files(Files) when is_list(Files) ->
  compile_files(Files, clj_env:default()).

-spec compile_files([file:filename_all()], clj_env:env()) -> clj_env:env().
compile_files(Files, Env) when is_list(Files) ->
  lists:foldl(fun compile_file/2, Env, Files).

-spec compile_file(file:filename_all()) -> clj_env:env().
compile_file(File) when is_binary(File) ->
  compile_file(File, clj_env:default()).

-spec compile_file(file:filename_all(), clj_env:env()) -> clj_env:env().
compile_file(File, Env) when is_binary(File) ->
  case file:read_file(File) of
    {ok, Src} -> compile(Src, Env);
    Error -> throw(Error)
  end.

-spec compile(binary()) -> clj_env:env().
compile(Src) when is_binary(Src) ->
  compile(Src, clj_env:default()).

-spec compile(binary(), clj_env:env()) -> clj_env:env().
compile(Src, Env) when is_binary(Src) ->
  Fun = fun(Form, EnvAcc) ->
            NewEnvAcc = clj_analyzer:analyze(EnvAcc, Form),
            {Forms, Exprs, Env1} = clj_emitter:emit(NewEnvAcc),
            compile_forms(Forms),
            eval_expressions(Exprs),
            Env1
        end,
  clj_reader:read_fold(Fun, Src, #{}, Env).

-spec compile_forms([erl_parse:abstract_form()]) -> atom() | undefined.
compile_forms([]) ->
  undefined;
compile_forms(Forms) ->
  %% io:format("==== FORMS ====~n~s~n", [ast_to_string(Forms)]),
  {ok, Name, Binary} = compile:forms(Forms),
  code:load_binary(Name, "", Binary),
  Name.

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
  eval(Form, clj_env:default()).

-spec eval(any(), clj_env:env()) -> {any(), clj_env:env()}.
eval(Form, Env) ->
  NewEnv = clj_analyzer:analyze(Env, Form),
  case clj_emitter:emit(NewEnv) of
    {Forms, [], Env1} ->
      {compile_forms(Forms), Env1};
    {[], Exprs, Env1} ->
      [Value] = eval_expressions(Exprs),
      {Value, Env1}
  end.
