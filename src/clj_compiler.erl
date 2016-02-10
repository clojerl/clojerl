-module(clj_compiler).

-export([
         compile_files/1,
         compile_files/2,
         compile_file/1,
         compile_file/2,
         compile/1,
         compile/2,
         eval/1,
         eval/2,
         eval/3
        ]).

-export([ no_warn_dynamic_var_name/1
        , no_warn_symbol_as_erl_fun/1
        ]).

-export([ast_to_string/1]).

-type clj_flag() :: 'no-warn-symbol-as-erl-fun'
                  | 'no-warn-dynamic-var-name'.

-type options() :: #{ output_dir => string()
                    , erl_flags  => [atom()]
                    , clj_flags  => [clj_flag()]
                    }.

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------

-spec default_options() -> map().
default_options() ->
  #{ output_dir => "ebin"
   , erl_flags  => [debug_info, verbose, report_errors, report_warnings]
   , clj_flags  => []
   }.

-spec compile_files([file:filename_all()]) -> clj_env:env().
compile_files(Files) when is_list(Files) ->
  compile_files(Files, default_options()).

-spec compile_files([file:filename_all()], options()) -> clj_env:env().
compile_files(Files, Opts) when is_list(Files) ->
  compile_files(Files, Opts, clj_env:default()).

-spec compile_files([file:filename_all()], options(), clj_env:env()) ->
  clj_env:env().
compile_files(Files, Opts, Env) when is_list(Files) ->
  Fun = fun(File, EnvAcc) -> compile_file(File, Opts, EnvAcc) end,
  lists:foldl(Fun, Env, Files).

-spec compile_file(file:filename_all()) -> clj_env:env().
compile_file(File) when is_binary(File) ->
  compile_file(File, default_options()).

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

-spec compile(binary()) -> clj_env:env().
compile(Src) when is_binary(Src) ->
  compile(Src, default_options()).

-spec compile(binary(), options()) -> clj_env:env().
compile(Src, Opts) when is_binary(Src) ->
  compile(Src, Opts, clj_env:default()).

-spec compile(binary(), options(), clj_env:env()) -> clj_env:env().
compile(Src, Opts0, Env0) when is_binary(Src) ->
  Opts = maps:merge(default_options(), Opts0),
  Env  = clj_env:put(Env0, clj_flags, maps:get(clj_flags, Opts)),
  Env1 = clj_reader:read_fold(fun compile_single_form/2, Src, #{}, Env),
  {ModulesForms, Expressions, Env2} = clj_emitter:remove_state(Env1),

  CompileFormsFun = fun(Forms) -> compile_forms(Forms, Opts) end,
  ensure_output_dir(Opts),

  lists:foreach(CompileFormsFun, ModulesForms),
  eval_expressions(Expressions),

  clj_env:remove(Env2, clj_flags).

-spec eval(any()) -> {any(), clj_env:env()}.
eval(Form) ->
  eval(Form, default_options()).

-spec eval(any(), options()) -> {any(), clj_env:env()}.
eval(Form, Opts) ->
  eval(Form, Opts, clj_env:default()).

-spec eval(any(), options(), clj_env:env()) -> {any(), clj_env:env()}.
eval(Form, Opts0, Env0) ->
  Opts = maps:merge(default_options(), Opts0),
  Env  = clj_env:put(Env0, clj_flags, maps:get(clj_flags, Opts)),
  Env1 = clj_analyzer:analyze(Env, Form),
  Env2 = clj_emitter:emit(Env1),
  {ModulesForms, Exprs, Env3} = clj_emitter:remove_state(Env2),

  CompileFormsFun = fun(Forms) -> compile_forms(Forms, Opts) end,
  lists:foreach(CompileFormsFun, ModulesForms),

  [Value] = eval_expressions(Exprs),
  {Value, clj_env:remove(Env3, clj_flags)}.

%% Flags

-spec no_warn_symbol_as_erl_fun(clj_env:env()) -> boolean().
no_warn_symbol_as_erl_fun(Env) ->
  check_flag('no-warn-symbol-as-erl-fun', Env).

-spec no_warn_dynamic_var_name(clj_env:env()) -> boolean().
no_warn_dynamic_var_name(Env) ->
  check_flag('no-warn-dynamic-var-name', Env).

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec check_flag(clj_flag(), clj_env:env()) -> boolean().
check_flag(Flag, Env) ->
  case clj_env:get(Env, clj_flags) of
    CljFlags when is_list(CljFlags) ->
      lists:member(Flag, CljFlags);
    undefined ->
      false
  end.

-spec ensure_output_dir(options()) -> ok.
ensure_output_dir(Opts) ->
  OutputDir = maps:get(output_dir, Opts),
  ok = filelib:ensure_dir(OutputDir),
  true = filelib:is_dir(OutputDir) orelse file:make_dir(OutputDir) =:= ok,
  true = code:add_path(OutputDir),
  ok.

-spec compile_single_form(any(), clj_env:env()) -> clj_env:env().
compile_single_form(Form, Env) ->
  Env1 = clj_emitter:without_state(Env, fun clj_analyzer:analyze/2, [Form]),
  clj_emitter:emit(Env1).

-spec compile_forms([erl_parse:abstract_form()], options()) ->
  atom() | undefined.
compile_forms([], _) ->
  undefined;
compile_forms(Forms, Opts) ->
  %% io:format("==== FORMS ====~n~s~n", [ast_to_string(Forms)]),
  ErlFlags = maps:get(erl_flags, Opts),
  case compile:forms(Forms, ErlFlags) of
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
