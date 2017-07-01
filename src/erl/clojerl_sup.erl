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
  Specs    = [ #{ id    => 'clojerl.Namespace'
                , start => {'clojerl.Namespace', start_link, []}
                }
             , #{ id    => clj_module
                , start => {clj_module, start_link, []}
                }
             , #{ id    => clj_cache
                , start => {clj_cache, start_link, []}
                }
             , #{ id    => 'clojerl.MultiFn'
                , start => {'clojerl.MultiFn', start_link, []}
                }
             ],
  {ok, {SupFlags, Specs}}.
