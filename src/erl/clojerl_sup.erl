-module(clojerl_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

-spec start_link() -> ignore | {error, term()} | {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(term()) ->
  {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
init(_Args) ->
  SupFlags = #{strategy => one_for_one},
  Schedulers = erlang:system_info(schedulers_online),
  Specs    = [ #{ id    => 'clojerl.Namespace'
                , start => {'clojerl.Namespace', start_link, []}
                }
             , #{ id    => clj_module
                , start => {clj_module, start_link, []}
                }
             , #{ id    => clj_cache
                , start => {clj_cache, start_link, []}
                }
             , #{ id    => 'clojerl.Agent.Server'
                , start => {'clojerl.Agent.Server', start_link, []}
                }
             , #{ id    => 'clojerl.Atom'
                , start => {'clojerl.Atom', start_link, []}
                }
             , #{ id    => 'clojerl.Delay'
                , start => {'clojerl.Delay', start_link, []}
                }
             , #{ id    => 'clojure.reducers'
                , start => { clj_pool
                           , start_link
                           , ['clojure.reducers', #{workers => Schedulers}]
                           }
                }
             ],
  {ok, {SupFlags, Specs}}.
