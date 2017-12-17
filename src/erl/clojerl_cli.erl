-module(clojerl_cli).

-export([start/0]).

-type options() :: #{ compile_path      => string()
                    , compile           => boolean()
                    , compile_opts      => clj_compiler:options()
                    , files             => [string()]
                    , clojure_main      => boolean()
                    , clojure_main_args => [string()]
                    , version           => boolean()
                    }.

-spec start() -> no_return().
start() ->
  Args = init:get_plain_arguments(),
  Opts = parse_args(Args),
  ok   = clojerl:start(),
  ok   = run_commands(Opts),

  erlang:halt(0).

-spec default_options() -> options().
default_options() ->
  #{ compile_path => "ebin"
   , compile      => false
   , files        => []
   , compile_opts => #{ time    => false
                      , verbose => false
                      }
   }.

-spec parse_args([string()]) -> options().
parse_args(Args) ->
  parse_args(Args, default_options()).

-spec parse_args([string()], options()) -> options().
parse_args([], Opts) ->
  Opts;
parse_args(["-v" | Rest], Opts) ->
  parse_args(Rest, Opts#{version => true});
parse_args(["-o", CompilePath | Rest], Opts) ->
  parse_args(Rest, Opts#{compile_path => CompilePath});
parse_args(["--compile" | Rest], Opts) ->
  parse_args(Rest, Opts#{compile => true});
parse_args([TimeOpt | Rest], #{compile_opts := CompileOpts} = Opts)
  when TimeOpt =:= "-t"; TimeOpt =:= "--time" ->
  parse_args(Rest, Opts#{compile_opts := CompileOpts#{time := true}});
parse_args([VerboseOpt | Rest], #{compile_opts := CompileOpts} = Opts)
  when VerboseOpt =:= "-vv"; VerboseOpt =:= "--verbose" ->
  parse_args(Rest, Opts#{compile_opts := CompileOpts#{verbose := true}});
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

-spec run_commands(options()) -> ok.
run_commands(#{version := true}) ->
  ErlangVersion  = erlang:system_info(system_version),
  ClojerlVersion = 'clojure.core':'clojure-version'(),
  io:format("~s~nClojerl ~s~n", [ErlangVersion, ClojerlVersion]),
  erlang:halt(0);
run_commands(#{ compile      := true
              , files        := Files
              , compile_path := CompilePath
              , compile_opts := CompileOpts
              } = Opts) ->

  CompilePathBin = list_to_binary(CompilePath),
  Bindings       = #{ <<"#'clojure.core/*compile-path*">>  => CompilePathBin
                    , <<"#'clojure.core/*compile-files*">> => true
                    },
  FilesBin       = [list_to_binary(F) || F <- Files],
  try
    ok = 'clojerl.Var':push_bindings(Bindings),
    [clj_compiler:compile_file(F, CompileOpts) || F <- FilesBin]
  catch Type:Reason ->
      io:format( "[~p] ~s~n~p"
               , [Type, clj_rt:str(Reason), erlang:get_stacktrace()]
               ),
      erlang:halt(1)
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
