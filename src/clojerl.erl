-module(clojerl).

-behavior(application).

-export([start/0]).

-export([start/2, stop/1]).

-export([ensure_modules/0]).

-spec start() -> ok.
start() ->
  application:ensure_all_started(clojerl).

-spec start(any(), any()) -> {ok, pid()} | {ok, pid(), any()} | {error, any()}.
start(_Type, _Args) ->
  {ok, _} = clojerl_sup:start_link(),
  spawn(fun ensure_modules/0),
  {ok, self()}.

-spec stop(any()) -> ok.
stop(_State) -> ok.

-spec ensure_modules() -> ok.
ensure_modules() ->
  Filter = "/clojerl*.beam",
  [ensure_loaded(File) || Path <- code:get_path(),
                          File <- filelib:wildcard(Path ++ Filter)],

  code:ensure_loaded('clojure.core'),
  ok.

-spec ensure_loaded(binary()) -> ok.
ensure_loaded(File) ->
  Module = list_to_atom(filename:rootname(filename:basename(File))),
  code:ensure_loaded(Module).
