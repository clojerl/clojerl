-module(clojerl).

-behavior(application).

-export([start/0]).

-export([start/2, stop/1]).

-spec start() -> ok.
start() ->
  {ok, _} = application:ensure_all_started(clojerl, permanent),
  ok.

-spec start(any(), any()) -> {ok, pid()} | {ok, pid(), any()} | {error, any()}.
start(_Type, _Args) ->
  erlang:system_flag(backtrace_depth, 20),
  {ok, _} = clojerl_sup:start_link(),
  ok = ensure_modules(),
  {ok, self()}.

-spec stop(any()) -> ok.
stop(_State) -> ok.

-spec ensure_modules() -> ok.
ensure_modules() ->
  Filter = "/clojerl*.beam",
  [ensure_loaded(File) || Path <- code:get_path(),
                          File <- filelib:wildcard(Path ++ Filter)],
  ok.

-spec ensure_loaded(binary()) -> ok.
ensure_loaded(File) ->
  Module = list_to_atom(filename:rootname(filename:basename(File))),
  code:ensure_loaded(Module).
