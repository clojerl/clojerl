-module(clojerl).

-behavior(application).

-export([start/0]).

-export([start/2, stop/1]).

-spec start() -> ok.
start() ->
  application:ensure_all_started(clojerl).

-spec start(any(), any()) -> {ok, pid()} | {ok, pid(), any()} | {error, any()}.
start(_Type, _Args) ->
  load_modules(),
  {ok, self()}.

-spec stop(any()) -> ok.
stop(_State) -> ok.

load_modules() ->
  LP = fun() ->
           [code:ensure_loaded(list_to_atom(filename:rootname(filename:basename(F))))
            || P <- code:get_path(), F <- filelib:wildcard(P ++ "/clojerl*.beam")]
       end,
  spawn(LP).
