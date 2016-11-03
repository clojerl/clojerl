-module(clj_compiler).

-include("clojerl.hrl").

-export([ compile_files/1
        , compile_files/2
        , compile_file/1
        , compile_file/2
        , compile/1
        , compile/2
        , load_file/1
        , eval/1
        , eval/2
        , eval/3
        , compile_forms/2
        ]).

-export([ no_warn_dynamic_var_name/1
        , no_warn_symbol_as_erl_fun/1
        ]).

-type clj_flag() :: 'no-warn-symbol-as-erl-fun'
                  | 'no-warn-dynamic-var-name'.

-type options() :: #{ output_dir  => string()
                    , erl_flags   => [atom()]
                    , clj_flags   => [clj_flag()]
                    , verbose     => boolean()
                    }.

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------

-spec default_options() -> options().
default_options() ->
  #{ output_dir  => "ebin"
   , erl_flags   => [ verbose
                    %% , debug_info
                    %% Don't use these except for improving generated code
                    %% , report_errors
                    %% , report_warnings
                    , nowarn_unused_vars
                    , nowarn_shadow_vars
                    , nowarn_unused_record
                    ]
   , clj_flags   => []
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
      ReaderOpts  = reader_opts(File),
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

-spec reader_opts(binary()) -> map().
reader_opts(File) ->
  case filename:extension(File) of
    <<".cljc">> -> #{?OPT_READ_COND => allow};
    _ -> #{}
  end.

-spec compile(binary()) -> clj_env:env().
compile(Src) when is_binary(Src) ->
  compile(Src, default_options()).

-spec compile(binary(), options()) -> clj_env:env().
compile(Src, Opts) when is_binary(Src) ->
  compile(Src, Opts, clj_env:default()).

-spec load_file(binary()) -> any().
load_file(Path) ->
  Env = compile_file(Path),
  clj_env:get(Env, eval).

-spec timed_compile(binary(), options(), clj_env:env()) -> clj_env:env().
timed_compile(Src, Opts, Env) when is_binary(Src) ->
  clj_utils:time("Total", fun compile/3, [Src, Opts, Env]).

-spec compile(binary(), options(), clj_env:env()) -> clj_env:env().
compile(Src, Opts, Env) when is_binary(Src) ->
  ProcDict = erlang:get(),
  DoCompile = fun() -> copy_proc_dict(ProcDict), do_compile(Src, Opts, Env) end,
  run_monitored(DoCompile).

-spec eval(any()) -> {any(), clj_env:env()}.
eval(Form) ->
  eval(Form, default_options()).

-spec eval(any(), options()) -> {any(), clj_env:env()}.
eval(Form, Opts) ->
  eval(Form, Opts, clj_env:default()).

-spec eval(any(), options(), clj_env:env()) -> {any(), clj_env:env()}.
eval(Form, Opts, Env) ->
  ProcDict = erlang:get(),
  DoEval   = fun() -> copy_proc_dict(ProcDict), do_eval(Form, Opts, Env) end,
  {Exprs, Forms, Env1} = run_monitored(DoEval),

  lists:foreach(compile_forms_fun(Opts), Forms),
  Value = eval_expressions(Exprs),

  {Value, Env1}.

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

-spec copy_proc_dict([{any(), any()}]) -> ok.
copy_proc_dict(List) ->
  [erlang:put(K, V) || {K, V} <- List].

-spec run_monitored(fun()) -> any().
run_monitored(Fun) ->
  {_Pid, Ref} = erlang:spawn_monitor(Fun),
  receive
    {'DOWN', Ref, _, _, {shutdown, Result}} ->
      Result;
    {'DOWN', Ref, _, _, {Kind, Error, Stacktrace}} ->
      erlang:raise(Kind, Error, Stacktrace);
    {'DOWN', Ref, _, _, Info} ->
      throw(Info)
  end.

-spec do_compile(binary(), options(), clj_env:env()) -> ok.
do_compile(Src, Opts0, Env0) when is_binary(Src) ->
  Opts     = maps:merge(default_options(), Opts0),
  CljFlags = maps:get(clj_flags, Opts),
  RdrOpts  = maps:get(reader_opts, Opts, #{}),
  Env      = clj_env:put(Env0, clj_flags, CljFlags),
  Env1     = clj_env:put(Env, compiler_opts, Opts),
  EmitEval = case Opts0 of
               #{time := true} ->
                 fun() ->
                     clj_utils:time( "Read, Analyze & Emit"
                                   , fun clj_reader:read_fold/4
                                   , [ fun emit_eval_form/2
                                     , Src
                                     , RdrOpts
                                     , Env1
                                     ]
                                   )
                 end;
               _ ->
                 fun () ->
                     clj_reader:read_fold( fun emit_eval_form/2
                                         , Src
                                         , RdrOpts
                                         , Env1
                                         )
                 end
             end,

  CompileFun =
    fun() ->
        try
          Env2 = EmitEval(),
          {_, Env3} = clj_emitter:remove_state(Env2),

          %% Compile all modules
          lists:foreach(compile_forms_fun(Opts), clj_module:all_forms()),

          Env4 = clj_env:remove(Env3, clj_flags),
          {shutdown, Env4}
        catch
          Kind:Error ->
            {Kind, Error, erlang:get_stacktrace()}
        end
    end,

  Result = clj_module:with_context(CompileFun),

  exit(Result).

-spec do_eval(any(), options(), clj_env:env()) -> ok.
do_eval(Form, Opts0, Env0) ->
  Opts     = maps:merge(default_options(), Opts0),
  CljFlags = maps:get(clj_flags, Opts),

  EvalFun =
    fun() ->
        try
          Env  = clj_env:put(Env0, clj_flags, CljFlags),
          %% Emit & eval form and keep the resulting value
          Env1 = clj_analyzer:analyze(Env, Form),
          Env2 = clj_emitter:emit(Env1),
          {Exprs, Env3} = clj_emitter:remove_state(Env2),

          { shutdown
          , { Exprs
            , clj_module:all_forms()
            , clj_env:remove(Env3, clj_flags)
            }
          }
        catch
          Kind:Error ->
            {Kind, Error, erlang:get_stacktrace()}
        end
    end,

  Result = clj_module:with_context(EvalFun),

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
ensure_output_dir(#{output_dir := OutputDir}) ->
  ok   = filelib:ensure_dir(filename:join([OutputDir, "dummy"])),
  true = code:add_path(OutputDir),
  ok;
ensure_output_dir(_) ->
  ok.

-spec emit_eval_form(any(), clj_env:env()) -> clj_env:env().
emit_eval_form(Form, Env) ->
  Env1  = clj_analyzer:analyze(Env, Form),
  Env2  = clj_emitter:emit(Env1),
  {Exprs, Env3} = clj_emitter:remove_state(Env2),
  Value = eval_expressions(Exprs),
  clj_env:put(Env3, eval, Value).

-spec compile_forms_fun(options()) -> function().
compile_forms_fun(Opts) ->
  case Opts of
    #{time := true} ->
      fun(Forms) ->
          clj_utils:time("Compile Forms", fun compile_forms/2, [Forms, Opts])
      end;
    _ ->
      fun(Forms) -> compile_forms(Forms, Opts) end
  end.

-spec compile_forms(cerl:c_module(), options()) -> atom().
compile_forms(Module, Opts) ->
  %% io:format("===== Module ====~n~s~n", [core_pp:format(Module)]),
  ok       = maybe_output_erl(Module, Opts),
  ErlFlags = [from_core, clint, binary | maps:get(erl_flags, Opts, [])],

  {ok, _, BeamBinary} = compile:forms(Module, ErlFlags),

  Name     = cerl:atom_val(cerl:module_name(Module)),
  BeamPath = maybe_output_beam(Name, add_core_code(BeamBinary, Module), Opts),
  {module, Name} = code:load_binary(Name, BeamPath, BeamBinary),
  Name.

-spec add_core_code(binary(), cerl:cerl()) -> binary().
add_core_code(BeamBinary, CoreModule) ->
  CoreAbstract        = erlang:term_to_binary(CoreModule, [compressed]),
  CoreAbstractChunk   = {"Core", CoreAbstract},
  {ok, _, OldChunks}  = beam_lib:all_chunks(BeamBinary),
  {ok, NewBeamBinary} = beam_lib:build_module(OldChunks ++ [CoreAbstractChunk]),
  NewBeamBinary.

-define(CERL_EVAL_MODULE, "cerl_eval").

-spec eval_expressions([cerl:cerl()]) -> [any()].
eval_expressions(Expressions) ->
  CurrentNs     = clj_namespace:current(),
  CurrentNsSym  = clj_namespace:name(CurrentNs),
  CurrentNsAtom = binary_to_existing_atom(clj_core:str(CurrentNsSym), utf8),
  ReplacedExprs = [clj_module:replace_calls(Expr, CurrentNsAtom)
                   || Expr <- Expressions],

  {ModuleName, EvalModule} = eval_module(ReplacedExprs),
  {ok, _, Binary} = compile:forms(EvalModule, [clint, from_core]),
  code:load_binary(ModuleName, "", Binary),
  Value = ModuleName:eval(),
  code:purge(ModuleName),
  code:delete(ModuleName),
  Value.

-spec eval_module([cerl:cerl()]) -> {module(), cerl:c_module()}.
eval_module(Expressions) ->
  FunName = cerl:c_fname(eval, 0),
  FunBody = case Expressions of
                 [] -> cerl:c_nil();
                 [Expr] -> Expr;
                 [FirstExpr | RestExpr] -> cerl:c_seq(FirstExpr, RestExpr)
               end,
  Fun     = cerl:c_fun([], FunBody),

  ModuleName = eval_module_name(),
  {InfoExports, InfoFuns} = clj_module:module_info_funs(ModuleName),

  Exports = [FunName | InfoExports],
  Attrs   = [{cerl:c_atom(clojure), cerl:abstract(true)}],
  Defs    = [{FunName, Fun} | InfoFuns],
  Name    = cerl:c_atom(ModuleName),
  {ModuleName, cerl:c_module(Name, Exports, Attrs, Defs)}.

-spec eval_module_name() -> module().
eval_module_name() ->
  eval_module_name(0).

-spec eval_module_name(non_neg_integer()) -> module().
eval_module_name(N) ->
  Name = list_to_atom(?CERL_EVAL_MODULE ++ integer_to_list(N)),
  case code:is_loaded(Name) of
    false -> Name;
    _     -> eval_module_name(N + 1)
  end.

-spec when_verbose(options(), binary()) -> ok.
when_verbose(#{verbose := true}, Message) ->
  io:format(Message);
when_verbose(_, _) ->
  ok.

-spec maybe_output_erl(cerl:c_module(), options()) ->
  ok | {error, term()}.
maybe_output_erl(Module, #{output_erl := Filename}) ->
  Source = core_pp:format(Module),
  file:write_file(Filename, Source);
maybe_output_erl(_, _) ->
  ok.

-spec maybe_output_beam(atom(), binary(), options()) -> string().
maybe_output_beam(Name, BeamBinary, #{output_dir := OutputDir} = Opts) ->
  ensure_output_dir(Opts),
  NameBin      = atom_to_binary(Name, utf8),
  BeamFilename = <<NameBin/binary, ".beam">>,
  BeamPath     = filename:join([OutputDir, BeamFilename]),
  ok           = file:write_file(BeamPath, BeamBinary),
  binary_to_list(BeamPath);
maybe_output_beam(_, _, _) -> "".
