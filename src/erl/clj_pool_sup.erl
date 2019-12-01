-module(clj_pool_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link(?MODULE, []).

-spec init(any()) ->
  {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_) ->
  SupFlags  = #{ strategy  => simple_one_for_one
               , intensity => 5
               , period    => 5
               },
  ChildSpec = #{ id       => clj_pool_worker
               , start    => {clj_pool_worker, start_link, []}
               , restart  => permanent
               , shutdown => brutal_kill
               , type     => worker
               },
  {ok, {SupFlags, [ChildSpec]}}.
