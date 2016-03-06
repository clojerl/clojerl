-module(clojerl).

-behavior(application).

-export([start/0]).

-export([start/2, stop/1]).

-spec start() -> ok.
start() ->
  application:ensure_all_started(clojerl).

-spec start(any(), any()) -> {ok, pid()} | {ok, pid(), any()} | {error, any()}.
start(_Type, _Args) ->
  {ok, _} = clojerl_sup:start_link(),
  load_modules(),
  {ok, self()}.

-spec stop(any()) -> ok.
stop(_State) -> ok.

-spec load_modules() -> pid().
load_modules() ->
  Filter = "/clojerl*.beam",
  LoadFun = fun() ->
           [ensure_loaded(File)
            || Path <- code:get_path(),
               File <- filelib:wildcard(Path ++ Filter)]
       end,
  spawn(LoadFun).

-spec ensure_loaded(binary()) -> ok.
ensure_loaded(File) ->
  Module = list_to_atom(filename:rootname(filename:basename(File))),
  code:ensure_loaded(Module).
