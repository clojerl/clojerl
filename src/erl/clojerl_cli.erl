-module(clojerl_cli).

-export([start/0]).

-type options() :: #{ compile_path => string()
                    , compile      => boolean()
                    , verbose      => boolean()
                    , time         => boolean()
                    , code_paths   => [string()]
                    , files        => [string()]
                    }.

-spec start() -> ok.
start() ->
  Args  = init:get_plain_arguments(),
  Opts  = parse_args(Args),
  ok    = process_options(Opts),
  ok    = clojerl:start(),

  run_commands(Opts),

  erlang:halt(0).

-spec compile_file(string(), options()) -> term().
compile_file(Path, Opts) ->
  PathBin = list_to_binary(Path),
  clj_compiler:compile_file(PathBin, Opts).

-spec default_options() -> options().
default_options() ->
  #{ compile_path => "ebin"
   , compile      => false
   , code_paths   => []
   , files        => []
   , time         => false
   , verbose      => false
   }.

-spec parse_args([string()]) -> options().
parse_args(Args) ->
  parse_args(Args, default_options()).

-spec parse_args([string()], options()) -> options().
parse_args([], Opts) ->
  Opts;
parse_args(["-o", CompilePath | Rest], Opts) ->
  parse_args(Rest, Opts#{compile_path => CompilePath});
parse_args(["--compile" | Rest], Opts) ->
  parse_args(Rest, Opts#{compile => true});
parse_args(["-pa", CodePath | Rest], Opts = #{code_paths := CodePaths}) ->
  parse_args(Rest, Opts#{code_paths => [CodePath | CodePaths]});
parse_args([TimeOpt | Rest], Opts)
  when TimeOpt =:= "-t"; TimeOpt =:= "--time" ->
  parse_args(Rest, Opts#{time => true});
parse_args([VerboseOpt | Rest], Opts)
  when VerboseOpt =:= "-vv"; VerboseOpt =:= "--verbose" ->
  parse_args(Rest, Opts#{verbose => true});
parse_args(["--clojure.main" | Args], Opts0) ->
  Opts1   = Opts0#{ clojure_main      => true
                  , clojure_main_args => Args
                  },
  parse_args([], Opts1);
parse_args([File | Rest], Opts = #{files := Files, compile := true}) ->
  parse_args(Rest, Opts#{files => [File | Files]});
parse_args([Unknown | _], _Opts) ->
  io:format("Unknown argument: ~s", [Unknown]),
  erlang:halt(0).

-spec process_options(options()) -> ok.
process_options(Opts) ->
  #{ compile_path := CompilePath
   , code_paths   := CodePaths
   } = Opts,
  ok = code:add_paths([CompilePath | CodePaths]).

-spec run_commands(options()) -> ok.
run_commands(#{ compile      := true
              , files        := Files
              , compile_path := CompilePath
              } = Opts) ->
  CompilePathBin = list_to_binary(CompilePath),
  Bindings       = #{ <<"#'clojure.core/*compile-path*">>  => CompilePathBin
                    , <<"#'clojure.core/*compile-files*">> => true
                    },
  try
    ok = 'clojerl.Var':push_bindings(Bindings),
    ok = lists:foreach(fun(X) -> compile_file(X, Opts) end, Files)
  after
    ok = 'clojerl.Var':pop_bindings()
  end,
  run_commands(Opts#{compile := false});
run_commands(#{clojure_main := true, clojure_main_args := Args} = Opts) ->
  ArgsBin = [erlang:list_to_binary(Arg) || Arg <- Args],
  'clojure.main':main(ArgsBin),
  run_commands(Opts#{clojure_main := false});
run_commands(_) ->
  ok.
