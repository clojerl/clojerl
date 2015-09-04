-module(clj_compiler).

-export([
         compile_files/1,
         compile_file/1,
         compile/1
        ]).

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
  Forms = clj_reader:read_all(Src),
  Fun = fun(Form, EnvAcc) ->
            NewEnvAcc = clj_analyzer:analyze(EnvAcc, Form),
            emit_code(NewEnvAcc)
        end,
  lists:foldl(Fun, Env, Forms).

%%------------------------------------------------------------------------------
%% Code Emission
%%------------------------------------------------------------------------------

-spec emit_code(clj_env:env()) -> ok.
emit_code(Env0) ->
  {Expr, Env} = clj_env:pop_expr(Env0),
  AbstractSyntaxForm = erl_syntax:revert(Expr),
  io:format("~p~n=========================~n", [AbstractSyntaxForm]),
  %% erlang:display(erl_eval:expr_list(AbstractSyntaxForms, [])),
  %% compile:forms(AbstractSyntaxForms),
  Env.
