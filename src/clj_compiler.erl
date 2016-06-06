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

-type options() :: #{ output_dir  => string()
                    , erl_flags   => [atom()]
                    , clj_flags   => [clj_flag()]
                    , reader_opts => map()
                    , verbose     => boolean()
                    }.

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------

-spec default_options() -> options().
default_options() ->
  #{ output_dir  => "ebin"
   , erl_flags   => [ debug_info
                    , verbose
                    , report_errors
                    , report_warnings
                    , nowarn_unused_vars
                    , nowarn_shadow_vars
                    ]
   , clj_flags   => []
   , reader_opts => #{}
   , verbose     => false
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
compile_file(File, Opts0, Env) when is_binary(File) ->
  case file:read_file(File) of
    {ok, Src} ->
      Opts        = maps:merge(default_options(), Opts0),
      Filename    = filename:basename(File),
      FilenameStr = binary_to_list(Filename),
      ErlFlags    = maps:get(erl_flags, Opts, []),
      ReaderOpts  = maps:get(reader_opts, Opts, #{}),
      Opts1       = Opts#{ erl_flags   => [{source, FilenameStr} | ErlFlags]
                         , reader_opts => ReaderOpts#{file => Filename}
                         },
      when_verbose(Opts1, <<"Compiling ", File/binary, "\n">>),
      CompileFun = case Opts1 of
                     #{time := true} -> fun timed_compile/3;
                     _               -> fun compile/3
                   end,
      CompileFun(Src, Opts1, Env);
    Error ->
      throw(Error)
  end.

-spec compile(binary()) -> clj_env:env().
compile(Src) when is_binary(Src) ->
  compile(Src, default_options()).

-spec compile(binary(), options()) -> clj_env:env().
compile(Src, Opts) when is_binary(Src) ->
  compile(Src, Opts, clj_env:default()).

-spec timed_compile(binary(), options(), clj_env:env()) -> clj_env:env().
timed_compile(Src, Opts, Env) when is_binary(Src) ->
  clj_utils:time("Finished compiling", fun compile/3, [Src, Opts, Env]).

-spec compile(binary(), options(), clj_env:env()) -> clj_env:env().
compile(Src, Opts, Env) when is_binary(Src) ->
  DoCompile   = fun() -> do_compile(Src, Opts, Env) end,
  {_Pid, Ref} = erlang:spawn_monitor(DoCompile),
  receive
    {'DOWN', Ref, _, _, {shutdown, Result}} ->
      Result;
    {'DOWN', Ref, _, _, {Kind, Error, Stacktrace}} ->
      erlang:raise(Kind, Error, Stacktrace);
    {'DOWN', Ref, _, _, Info} ->
      throw(Info)
  end.

-spec eval(any()) -> {any(), clj_env:env()}.
eval(Form) ->
  eval(Form, default_options()).

-spec eval(any(), options()) -> {any(), clj_env:env()}.
eval(Form, Opts) ->
  eval(Form, Opts, clj_env:default()).

-spec eval(any(), options(), clj_env:env()) -> {any(), clj_env:env()}.
eval(Form, Opts0, Env0) ->
  Opts     = maps:merge(default_options(), Opts0),
  CljFlags = maps:get(clj_flags, Opts),

  EvalFun =
    fun() ->
        Env  = clj_env:put(Env0, clj_flags, CljFlags),
        %% Emit & eval form and keep the resulting value
        Env1 = clj_analyzer:analyze(Env, Form),
        Env2 = clj_emitter:emit(Env1),
        {Exprs, Env3} = clj_emitter:remove_state(Env2),

        lists:foreach(compile_forms_fun(Opts), clj_module:all_forms()),

        [Value] = eval_expressions(Exprs),
        {Value, clj_env:remove(Env3, clj_flags)}
    end,

  clj_module:with_context(EvalFun).

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

-spec do_compile(binary(), options(), clj_env:env()) -> ok.
do_compile(Src, Opts0, Env0) when is_binary(Src) ->
  Opts     = maps:merge(default_options(), Opts0),
  CljFlags = maps:get(clj_flags, Opts),
  RdrOpts  = maps:get(reader_opts, Opts),

  CompileFun =
    fun() ->
        try
          Env  = clj_env:put(Env0, clj_flags, CljFlags),
          Env1 = clj_reader:read_fold(fun emit_eval_form/2, Src, RdrOpts, Env),
          {_, Env2} = clj_emitter:remove_state(Env1),

          %% Compile all modules
          ensure_output_dir(Opts),
          lists:foreach(compile_forms_fun(Opts), clj_module:all_forms()),

          Env3 = clj_env:remove(Env2, clj_flags),
          {shutdown, Env3}
        catch
          Kind:Error ->
             {Kind, Error, erlang:get_stacktrace()}
        end
    end,

  Result = clj_module:with_context(CompileFun),

  exit(Result).

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

-spec emit_eval_form(any(), clj_env:env()) -> clj_env:env().
emit_eval_form(Form, Env) ->
  Env1 = clj_analyzer:analyze(Env, Form),
  Env2 = clj_emitter:emit(Env1),
  {Exprs, Env3} = clj_emitter:remove_state(Env2),
  eval_expressions(Exprs),
  Env3.

-spec compile_forms_fun(options()) -> function().
compile_forms_fun(Opts) ->
  fun(Forms) -> compile_forms(Forms, Opts) end.

-spec compile_forms([erl_parse:abstract_form()], options()) ->
  atom() | undefined.
compile_forms([], _) ->
  undefined;
compile_forms(Forms, Opts) ->
  ok       = maybe_output_erl(Forms, Opts),
  ErlFlags = maps:get(erl_flags, Opts),

  case compile:forms(Forms, ErlFlags) of
    {ok, Name, Binary} ->
      %% io:format("Compiled ~p with ~p forms~n", [Name, length(Forms)]),
      OutputDir    = maps:get(output_dir, Opts),
      NameBin      = atom_to_binary(Name, utf8),
      BeamFilename = <<NameBin/binary, ".beam">>,
      BeamPath     = filename:join([OutputDir, BeamFilename]),
      ok           = file:write_file(BeamPath, Binary),

      BeamPathStr = binary_to_list(BeamPath),
      {module, Name} = code:load_binary(Name, BeamPathStr, Binary),
      Name;
    Error ->
      error(Error)
  end.

-spec eval_expressions([erl_parse:abstract_expr()]) -> [any()].
eval_expressions([]) ->
  [];
eval_expressions(Expressions) ->
  %% io:format("==== EXPR ====~n~s~n", [ast_to_string(Expressions)]),
  CurrentNs     = clj_namespace:current(),
  CurrentNsSym  = clj_namespace:name(CurrentNs),
  CurrentNsAtom = erlang:binary_to_existing_atom(clj_core:str(CurrentNsSym), utf8),
  ReplacedExprs = [clj_module:replace_calls(Expr, CurrentNsAtom, '_')
                   || Expr <- Expressions],
  {Values, _}   = erl_eval:expr_list(ReplacedExprs, []),
  Values.

-spec ast_to_string([erl_parse:abstract_form()]) -> string().
ast_to_string(Forms) -> erl_prettypr:format(erl_syntax:form_list(Forms)).

-spec when_verbose(options(), binary()) -> ok.
when_verbose(#{verbose := true}, Message) ->
  io:format(Message);
when_verbose(_, _) ->
  ok.

-spec maybe_output_erl([erl_parse:abstract_form()], options()) ->
  ok | {error, term()}.
maybe_output_erl(Forms, #{output_erl := Filename}) ->
  Source = ast_to_string(Forms),
  file:write_file(Filename, Source);
maybe_output_erl(_, _) ->
  ok.
