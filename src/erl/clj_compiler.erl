-module(clj_compiler).

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-dialyzer(no_return).

-export([ compile_file/1
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
                    , output_core => boolean()    %% Output .core code
                    , fake        => boolean()    %% Fake modules being compiled
                    }.

-type compiled_modules() :: [file:filename_all()].
-type return_type()      :: compiled_modules | env.

-export_type([options/0]).

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------

-spec default_options() -> options().
default_options() ->
  #{ erl_flags   => [ verbose
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

-spec compile_file(file:filename_all()) -> compiled_modules().
compile_file(File) when is_binary(File) ->
  compile_file(File, default_options()).

-spec compile_file(file:filename_all(), options()) -> compiled_modules().
compile_file(File, Opts) when is_binary(File) ->
  compile_file(File, Opts, clj_env:default(), compiled_modules).

-spec compile_file( file:filename_all()
                  , options()
                  , clj_env:env()
                  , return_type()
                  ) -> compiled_modules() | clj_env:env().
compile_file(File, Opts0, Env0, ReturnType) when is_binary(File) ->
  ?ERROR_WHEN( not filelib:is_regular(File)
             , [<<"File '">>, File, <<"' does not exist">>]
             ),
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
      Env1 = CompileFun(Src, Opts1, Env0),
      case ReturnType of
        compiled_modules -> clj_env:get(compiled_modules, Env1);
        env -> Env1
      end;
    Error ->
      error(Error)
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
  Env = compile_file(Path, default_options(), clj_env:default(), env),
  clj_env:get(eval, Env).

-spec timed_compile(binary(), options(), clj_env:env()) ->
  compiled_modules().
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
eval(Form, Opts, Env0) ->
  Fun  = fun(F, EnvAcc) -> eval1(F, Opts, EnvAcc) end,
  Env1 = clj_env:push(#{}, Env0),
  Env2 = check_top_level_do(Fun, Form, Env1),
  {clj_env:get(eval, Env2), Env2}.

-spec eval1(any(), options(), clj_env:env()) -> clj_env:env().
eval1(Form, Opts, Env) ->
  ProcDict = erlang:get(),
  DoEval   = fun() -> copy_proc_dict(ProcDict), do_eval(Form, Opts, Env) end,
  {Exprs, Modules, Env1} = run_monitored(DoEval),

  lists:foreach(compile_module_fun(Opts), Modules),
  Value = eval_expressions(Exprs),
  clj_env:put(eval, Value, Env1).

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

  #{ clj_flags   := CljFlags
   , reader_opts := RdrOpts
   , time        := Time
   } = Opts,

  File     = maps:get(file, RdrOpts, ?NIL),
  Mapping  = #{ clj_flags     => CljFlags
              , compiler_opts => Opts
              , eval          => ?NIL
              , location      => #{file => File}
              },
  Env1     = clj_env:push(Mapping, Env0),

  AnnEmitEval = case Time of
                  true  -> fun timed_analyze_emit_eval/2;
                  false -> fun analyze_emit_eval/2
                end,

  CompileFun =
    fun() ->
        try
          Env2  = clj_reader:read_fold(AnnEmitEval, Src, RdrOpts, Time, Env1),
          %% Maybe report time
          Time andalso report_time(Env2),
          %% Compile all modules
          Fun   = compile_module_fun(Opts),
          Beams = [Fun(M) || M <- clj_module:all_modules()],
          Env3  = clj_env:put(compiled_modules, Beams, Env2),
          {shutdown, Env3}
        catch ?WITH_STACKTRACE(Kind, Error, Stacktrace)
            {Kind, Error, Stacktrace}
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
        catch ?WITH_STACKTRACE(Kind, Error, Stacktrace)
            {Kind, Error, Stacktrace}
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

-spec report_time(clj_env:env()) -> ok.
report_time(Env) ->
  Times = clj_env:time(Env),
  [ io:format("~s: ~p ms~n", [K, erlang:trunc(V / 1000)])
    || {K, V} <- maps:to_list(Times)
  ],
  ok.

-spec timed_analyze_emit_eval(any(), clj_env:env()) -> clj_env:env().
timed_analyze_emit_eval(Form, Env) ->
  check_top_level_do(fun do_timed_analyze_emit_eval/2, Form, Env).

-spec do_timed_analyze_emit_eval(any(), clj_env:env()) -> clj_env:env().
do_timed_analyze_emit_eval(Form, Env0) ->
  {TimeAnn, Env1} = timer:tc(clj_analyzer, analyze, [Form, Env0]),
  Env2 = clj_env:time("Analyzer", TimeAnn, Env1),

  {TimeEmit, {Exprs, Env3}} = timer:tc(clj_emitter, emit, [Env2]),
  Env4 = clj_env:time("Emitter", TimeEmit, Env3),

  {TimeEval, Value} = timer:tc(fun() -> eval_expressions(Exprs) end),
  Env5 = clj_env:time("Eval", TimeEval, Env4),

  clj_env:update(eval, Value, Env5).

-spec analyze_emit_eval(any(), clj_env:env()) -> clj_env:env().
analyze_emit_eval(Form, Env) ->
  check_top_level_do(fun do_analyze_emit_eval/2, Form, Env).

-spec do_analyze_emit_eval(any(), clj_env:env()) -> clj_env:env().
do_analyze_emit_eval(Form, Env) ->
  Env1          = clj_analyzer:analyze(Form, Env),
  {Exprs, Env2} = clj_emitter:emit(Env1),
  Value         = eval_expressions(Exprs),
  clj_env:update(eval, Value, Env2).

-spec check_top_level_do(function(), any(), clj_env:env()) -> clj_env:env().
check_top_level_do(Fun, Form, Env) ->
  Expanded = clj_analyzer:macroexpand(Form, Env),
  case
    clj_rt:'seq?'(Expanded)
    andalso clj_rt:equiv(clj_rt:first(Expanded), clj_rt:symbol(<<"do">>))
  of
    true ->
      Rest = clj_rt:rest(Expanded),
      lists:foldl(Fun, Env, clj_rt:to_list(Rest));
    false ->
      Fun(Expanded, Env)
  end.

-spec compile_module_fun(options()) -> function().
compile_module_fun(#{time := true} = Opts) ->
  fun(Forms) ->
      clj_utils:time("Compile Forms", fun compile_module/2, [Forms, Opts])
  end;
compile_module_fun(Opts) ->
  fun(Forms) -> compile_module(Forms, Opts) end.

-spec compile_module(cerl:c_module(), options()) -> binary().
compile_module(Module, Opts) ->
  ok       = maybe_output_core(Module, Opts),
  ErlFlags = [ from_core, clint, binary, return_errors, return_warnings
             | maps:get(erl_flags, Opts, [])
             ],

  %% io:format("===== Module ====~n~s~n", [core_pp:format(Module)]),
  case compile:noenv_forms(Module, ErlFlags) of
    {ok, _, Beam0, _Warnings} ->
      Name           = cerl:atom_val(cerl:module_name(Module)),
      Beam1          = clj_utils:add_core_to_binary(Beam0, Module),
      Beam2          = maybe_replace_compile_info(Beam1, Module),
      BeamPath       = maybe_output_beam(Name, Module, Beam2, Opts),
      {module, Name} = code:load_binary(Name, BeamPath, Beam2),
      list_to_binary(BeamPath);
    {error, Errors, Warnings} ->
      error({Errors, Warnings})
  end.

-define(CERL_EVAL_MODULE, "cerl_eval").

-spec eval_expressions([cerl:cerl()]) -> [any()].
eval_expressions(Expressions) ->
  eval_expressions(Expressions, true).

-spec eval_expressions([cerl:cerl()], boolean()) -> [any()].
eval_expressions(Expressions, true) ->
  CurrentNs     = 'clojerl.Namespace':current(),
  CurrentNsSym  = 'clojerl.Namespace':name(CurrentNs),
  CurrentNsBin  = 'clojerl.Symbol':str(CurrentNsSym),
  CurrentNsAtom = binary_to_existing_atom(CurrentNsBin, utf8),
  ReplacedExprs = [ clj_module:replace_calls(Expr, CurrentNsAtom)
                    || Expr <- Expressions
                  ],
  eval_expressions(ReplacedExprs, false);
eval_expressions(Expressions, false) ->
  {ModuleName, EvalModule} = eval_module(Expressions),
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
maybe_output_core(Module, #{output_core := true}) ->
  Source = core_pp:format(Module),
  Name   = cerl:concrete(cerl:module_name(Module)),
  Path   = atom_to_list(Name) ++ ".core",
  file:write_file(Path, Source);
maybe_output_core(_, _) ->
  ok.

%% Keep compile_info information for modules that were originally compiled
%% as Erlang modules (e.g. protocol modules).
%% This is to avoid rebar3 re-compiling the modules because it detects
%% their `compile_info` options were changed.

-spec maybe_replace_compile_info(binary(), cerl:c_module()) -> binary().
maybe_replace_compile_info(Beam, Module) ->
  Name = cerl:concrete(cerl:module_name(Module)),
  code:ensure_loaded(Name),
  case original_compile_info(Name) of
    undefined   -> Beam;
    CompileInfo -> clj_utils:add_compile_info_to_binary(Beam, CompileInfo)
  end.

-spec original_compile_info(module()) -> [any()] | undefined.
original_compile_info(Name) ->
  case code:which(Name) of
    Filename when is_list(Filename) ->
      compile_info(Filename);
    _ -> undefined
  end.

-spec compile_info(list()) -> [any()] | undefined.
compile_info(Target) ->
  case beam_lib:chunks(Target, [compile_info]) of
    {ok, {_mod, Chunks}} ->
      proplists:get_value(compile_info, Chunks, []);
    {error, beam_lib, _} ->
      undefined
  end.

-spec maybe_output_beam(module(), cerl:c_module(), binary(), options()) ->
  string().
maybe_output_beam(_Name, _Module, _BeamBinary, #{fake := true}) ->
  ?NO_SOURCE;
maybe_output_beam(Name, Module, BeamBinary, _Opts) ->
  CompileFiles = 'clojure.core':'*compile-files*__val'(),
  case CompileFiles of
    true  ->
      IsProtocol = clj_module:is_protocol(Module),
      output_beam(Name, IsProtocol, BeamBinary);
    false ->
      clj_utils:store_binary(Name, BeamBinary),
      ?NO_SOURCE
  end.

-spec output_beam(module(), boolean(), binary()) -> string().
output_beam(Name, IsProtocol, BeamBinary) ->
  CompilePath  = compile_path(IsProtocol),
  ?ERROR_WHEN(CompilePath =:= ?NIL, <<"*compile-path* not set">>),
  ok           = ensure_path(CompilePath),
  NameBin      = atom_to_binary(Name, utf8),
  BeamFilename = <<NameBin/binary, ".beam">>,
  BeamPath     = filename:join([CompilePath, BeamFilename]),
  ok           = file:write_file(BeamPath, BeamBinary),
  binary_to_list(BeamPath).

-spec compile_path(boolean()) -> binary() | ?NIL.
compile_path(true) ->
  case 'clojure.core':'*compile-protocols-path*__val'() of
    ?NIL ->
      ?WARN(<<"*compile-protocols-path* not set, using *compile-path*">>),
      'clojure.core':'*compile-path*__val'();
    Path ->
      Path
  end;
compile_path(false) ->
  'clojure.core':'*compile-path*__val'().

-spec ensure_path(binary()) -> ok.
ensure_path(Path) when is_binary(Path) ->
  ok   = filelib:ensure_dir(filename:join([Path, "dummy"])),
  true = code:add_path(binary_to_list(Path)),
  ok.
