-module(clojerl_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

-spec start_link() -> ok.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(term()) -> term().
init(_Args) ->
  SupFlags = #{strategy => one_for_one},
  Specs    = [ #{ id    => clj_module
                , start => {clj_module, start_link, []}
                }
             ],
  {ok, {SupFlags, Specs}}.
