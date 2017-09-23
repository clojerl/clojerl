-module(clj_compiler).

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-dialyzer(no_return).

-export([ compile_files/1
        , compile_files/2
        , compile_file/1
        , compile_file/2
        , compile/1
        , compile/2
        , load_file/1
        , load_string/1
        , eval/1
        , eval/2
        , eval/3
        , eval_expressions/1
        , eval_expressions/2
        , compile_module/2
        ]).

-export([ no_warn_dynamic_var_name/1
        , no_warn_symbol_as_erl_fun/1
        ]).

-type clj_flag() :: 'no-warn-symbol-as-erl-fun'
                  | 'no-warn-dynamic-var-name'.

-type options() :: #{ erl_flags   => [atom()]     %% erlang compilation flags
                    , clj_flags   => [clj_flag()] %% clojerl compilation flags
                    , verbose     => boolean()    %% Output verbose messages
                    , reader_opts => map()        %% Options for the reader
                    , time        => boolean()    %% Measure and show
                                                  %% compilation times
                    , output_core => binary() | false %% Output .core code
                    , fake        => boolean()    %% Fake modules being compiled
                    }.

-export_type([options/0]).

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------

-spec default_options() -> options().
default_options() ->
  #{ erl_flags   => [ verbose
                    %% , debug_info
                    %% Don't use these except for improving generated code
                    , nowarn_unused_vars
                    , nowarn_shadow_vars
                    , nowarn_unused_record
                    ]
   , clj_flags   => []
   , verbose     => false
   , reader_opts => #{}
   , time        => false
   , output_core => false
   , fake        => false
   }.

-spec compile_files([file:filename_all()]) -> clj_env:env().
compile_files(Files) when is_list(Files) ->
  compile_files(Files, default_options()).

-spec compile_files([file:filename_all()], options()) -> clj_env:env().
compile_files(Files, Opts) when is_list(Files) ->
  compile_files(Files, Opts, clj_env:default()).

-spec compile_files([file:filename_all()], options(), clj_env:env()) ->
  clj_env:env().
compile_files(Files, Opts, Env0) when is_list(Files) ->
  Fun = fun(File, EnvAcc) -> compile_file(File, Opts, EnvAcc) end,
  lists:foldl(Fun, Env0, Files).

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
      Opts       = maps:merge(default_options(), Opts0),
      FileStr    = binary_to_list(File),
      ErlFlags   = maps:get(erl_flags, Opts, []),
      ReaderOpts = reader_opts(File),
      Opts1      = Opts#{ erl_flags   => [{source, FileStr} | ErlFlags]
                        , reader_opts => ReaderOpts#{file => File}
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

-spec load_string(binary()) -> any().
load_string(Path) ->
  Env = compile(Path),
  clj_env:get(eval, Env).

-spec load_file(binary()) -> any().
load_file(Path) ->
  Env = compile_file(Path),
  clj_env:get(eval, Env).

-spec timed_compile(binary(), options(), clj_env:env()) -> ok.
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
  DoSymbol = clj_rt:symbol(<<"do">>),
  case
    clj_rt:'seq?'(Form)
    andalso clj_rt:equiv(clj_rt:first(Form), DoSymbol)
  of
    true ->
      Forms = clj_rt:to_list(clj_rt:rest(Form)),
      FoldFun = fun(F, {_, EnvAcc}) ->
                    eval1(F, Opts, EnvAcc)
                end,
      lists:foldl(FoldFun, {?NIL, Env}, Forms);
    false ->
      eval1(Form, Opts, Env)
  end.

-spec eval1(any(), options(), clj_env:env()) -> {any(), clj_env:env()}.
eval1(Form, Opts, Env) ->
  ProcDict = erlang:get(),
  DoEval   = fun() -> copy_proc_dict(ProcDict), do_eval(Form, Opts, Env) end,
  {Exprs, Modules, Env1} = run_monitored(DoEval),

  lists:foreach(compile_module_fun(Opts), Modules),
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
  [erlang:put(K, V) || {K, V} <- List],
  ok.

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

-spec do_compile(binary(), options(), clj_env:env()) -> no_return().
do_compile(Src, Opts0, Env0) when is_binary(Src) ->
  Opts     = maps:merge(default_options(), Opts0),
  CljFlags = maps:get(clj_flags, Opts),
  RdrOpts  = maps:get(reader_opts, Opts, #{}),
  File     = maps:get(file, RdrOpts, ?NIL),
  Mapping  = #{ clj_flags     => CljFlags
              , compiler_opts => Opts
              , eval          => ?NIL
              , location      => #{file => File}
              },
  Env1     = clj_env:push(Mapping, Env0),

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
          State1 = EmitEval(),
          %% Compile all modules
          lists:foreach(compile_module_fun(Opts), clj_module:all_modules()),
          {shutdown, State1}
        catch
          Kind:Error ->
            {Kind, Error, erlang:get_stacktrace()}
        end
    end,

  Result = clj_module:with_context(CompileFun),

  exit(Result).

-spec do_eval(any(), options(), clj_env:env()) -> no_return().
do_eval(Form, Opts0, Env0) ->
  Opts     = maps:merge(default_options(), Opts0),
  CljFlags = maps:get(clj_flags, Opts),

  EvalFun =
    fun() ->
        try
          Env  = clj_env:push(#{clj_flags => CljFlags}, Env0),
          %% Emit & eval form and keep the resulting value
          Env1 = clj_analyzer:analyze(Form, Env),
          {Exprs, Env2} = clj_emitter:emit(Env1),

          { shutdown
          , { Exprs
            , clj_module:all_modules()
            , clj_env:pop(Env2)
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
  case clj_env:get(clj_flags, Env) of
    CljFlags when is_list(CljFlags) ->
      lists:member(Flag, CljFlags);
    ?NIL ->
      false
  end.

-spec emit_eval_form(any(), clj_env:env()) -> clj_env:env().
emit_eval_form(Form, Env) ->
  Env1          = clj_analyzer:analyze(Form, Env),
  {Exprs, Env2} = clj_emitter:emit(Env1),
  Value         = eval_expressions(Exprs),
  clj_env:update(eval, Value, Env2).

-spec compile_module_fun(options()) -> function().
compile_module_fun(#{time := true} = Opts) ->
  fun(Forms) ->
      clj_utils:time("Compile Forms", fun compile_module/2, [Forms, Opts])
  end;
compile_module_fun(Opts) ->
  fun(Forms) -> compile_module(Forms, Opts) end.

-spec compile_module(cerl:c_module(), options()) -> atom().
compile_module(Module, Opts) ->
  ok       = maybe_output_core(Module, Opts),
  ErlFlags = [ from_core, clint, binary, return_errors, return_warnings
             | maps:get(erl_flags, Opts, [])
             ],

  %% io:format("===== Module ====~n~s~n", [core_pp:format(Module)]),
  case compile:noenv_forms(Module, ErlFlags) of
    {ok, _, Beam, _Warnings} ->
      Name     = cerl:atom_val(cerl:module_name(Module)),
      BeamCore = clj_utils:add_core_to_binary(Beam, Module),
      BeamPath = maybe_output_beam(Name, BeamCore, Opts),
      {module, Name} = code:load_binary(Name, BeamPath, Beam),
      Name;
    {error, Errors, Warnings} ->
      error({Errors, Warnings})
  end.

-define(CERL_EVAL_MODULE, "cerl_eval").

-spec eval_expressions([cerl:cerl()]) -> [any()].
eval_expressions(Expressions) ->
  eval_expressions(Expressions, true).

-spec eval_expressions([cerl:cerl()], boolean()) -> [any()].
eval_expressions(Expressions, ReplaceCalls) ->
  CurrentNs     = 'clojerl.Namespace':current(),
  CurrentNsSym  = 'clojerl.Namespace':name(CurrentNs),
  CurrentNsBin  = 'clojerl.Symbol':str(CurrentNsSym),
  CurrentNsAtom = binary_to_existing_atom(CurrentNsBin, utf8),
  ReplacedExprs = case ReplaceCalls of
                    true  -> [ clj_module:replace_calls(Expr, CurrentNsAtom)
                               || Expr <- Expressions
                             ];
                    false -> Expressions
                  end,

  {ModuleName, EvalModule} = eval_module(ReplacedExprs),
  %% io:format("===== Eval Module ====~n~s~n", [core_pp:format(EvalModule)]),
  Opts = [clint, from_core, return_errors, return_warnings],
  case compile:noenv_forms(EvalModule, Opts) of
    {ok, _, Beam, _Warnings} ->
      code:load_binary(ModuleName, "", Beam),
      ModuleName:eval();
    {error, Errors, Warnings} ->
      error({Errors, Warnings})
  end.

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
  Attrs   = [{cerl:c_atom(clojure), cerl:abstract([true])}],
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

-spec maybe_output_core(cerl:c_module(), options()) ->
  ok | {error, term()}.
maybe_output_core(Module, #{output_core := Path}) when is_binary(Path) ->
  Source = core_pp:format(Module),
  file:write_file(Path, Source);
maybe_output_core(_, _) ->
  ok.

-spec maybe_output_beam(atom(), binary(), options()) -> string().
maybe_output_beam(_Name, _BeamBinary, #{fake := true}) ->
  ?NO_SOURCE;
maybe_output_beam(Name, BeamBinary, _Opts) ->
  CompileFiles = 'clojure.core':'*compile-files*__val'(),
  case CompileFiles of
    true  ->
      output_beam(Name, BeamBinary);
    false ->
      clj_utils:store_binary(Name, BeamBinary),
      ?NO_SOURCE
  end.

-spec output_beam(atom(), binary()) -> string().
output_beam(Name, BeamBinary) ->
  CompilePath  = 'clojure.core':'*compile-path*__val'(),
  ok           = ensure_path(CompilePath),
  NameBin      = atom_to_binary(Name, utf8),
  BeamFilename = <<NameBin/binary, ".beam">>,
  BeamPath     = filename:join([CompilePath, BeamFilename]),
  ok           = file:write_file(BeamPath, BeamBinary),
  binary_to_list(BeamPath).

-spec ensure_path(binary()) -> ok.
ensure_path(Path) when is_binary(Path) ->
  ok   = filelib:ensure_dir(filename:join([Path, "dummy"])),
  true = code:add_path(binary_to_list(Path)),
  ok.
