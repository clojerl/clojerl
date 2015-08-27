-module(clj_compiler).

-export([
         compile_files/1,
         compile_file/1,
         compile/1
        ]).

-include("include/clj_types.hrl").

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------

-spec compile_files([file:filename_all()]) -> ok.
compile_files(Files) ->
  Env = clj_analyzer:analyze_files(Files),
  emit_code(Env).

-spec compile_file(file:filename_all()) -> ok.
compile_file(File) ->
  Env = clj_analyzer:analyze_file(File),
  emit_code(Env).

-spec compile(binary()) -> clj_env:env().
compile(Src) when is_binary(Src) ->
  Forms = clj_reader:read_all(Src),
  Env = clj_analyzer:analyze(Forms),
  emit_code(Env).

%%------------------------------------------------------------------------------
%% Code Emission
%%------------------------------------------------------------------------------

-spec emit_code(clj_env:env()) -> ok.
emit_code(Env = #{exprs := Exprs}) ->
  _AbstractSyntaxForms = lists:map(fun erl_syntax:revert/1, Exprs),
  %% erlang:display(AbstractSyntaxForms),
  %% erlang:display(erl_eval:expr_list(AbstractSyntaxForms, [])),
  %% compile:forms(AbstractSyntaxForms),
  Env.
