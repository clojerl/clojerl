-module(clj_compiler).

-export([
         compile_files/1,
         compile_file/1,
         compile/1
        ]).

-include("include/clj_types.hrl").

-type state() :: #{current_ns => symbol(),
                   local_bindings => #{symbol() => any()},
                   namespaces => []}.

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------

-spec compile_files([file:filename_all()]) -> ok.
compile_files(Files) ->
  State = clj_analyzer:analyze_files(Files),
  emit_code(State).

-spec compile_file(file:filename_all()) -> ok.
compile_file(File) ->
  State = clj_analyzer:analyze_file(File),
  emit_code(State).

-spec compile(binary()) -> state().
compile(Src) when is_binary(Src) ->
  Forms = clj_reader:read_all(Src),
  State = clj_analyzer:analyze(Forms),
  emit_code(State).

%%------------------------------------------------------------------------------
%% Code Emission
%%------------------------------------------------------------------------------

-spec emit_code(state()) -> ok.
emit_code(State = #{exprs := Exprs}) ->
  _AbstractSyntaxForms = lists:map(fun erl_syntax:revert/1, Exprs),
  %% erlang:display(AbstractSyntaxForms),
  %% erlang:display(erl_eval:expr_list(AbstractSyntaxForms, [])),
  %% compile:forms(AbstractSyntaxForms),
  State.
