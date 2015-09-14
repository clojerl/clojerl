-module(clj_compiler).

-export([
         compile_files/1,
         compile_file/1,
         compile/1,
         eval/2
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
  Fun = fun(Form, EnvAcc) ->
            NewEnvAcc = clj_analyzer:analyze(EnvAcc, Form),
            clj_emitter:emit(NewEnvAcc)
        end,
  clj_reader:read_fold(Fun, Src, Env).

-spec eval(any(), clj_env:env()) -> any().
eval(Form, _Env) -> Form.
