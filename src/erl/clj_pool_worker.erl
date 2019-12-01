-module(clj_pool_worker).

-behaviour(gen_server).

-export([ start_link/1
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec start_link(pid()) -> {ok, pid()}.
start_link(Manager) ->
  gen_server:start_link(?MODULE, Manager, []).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

-spec init(any()) -> {ok, any()}.
init(Manager) ->
  {ok, Manager}.

-spec handle_call(any(), any(), any()) -> {reply, any(), any()}.
handle_call({M, F, Args}, _From, State) ->
  Reply = try {ok, erlang:apply(M, F, Args)}
          catch _:Reason -> {error, Reason}
          end,
  {reply, Reply, State}.

-spec handle_cast(any(), any()) -> {noreply, any()}.
handle_cast({call, From, {M, F, Args}}, Manager) ->
  Reply = try {ok, erlang:apply(M, F, Args)}
          catch _:Reason -> {error, Reason}
          end,
  gen_server:reply(From, Reply),
  notify_available(Manager),
  {noreply, Manager};
handle_cast({cast, {M, F, Args}}, Manager) ->
  try erlang:apply(M, F, Args)
  catch _:_ -> ok
  end,
  notify_available(Manager),
  {noreply, Manager}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec notify_available(pid()) -> ok.
notify_available(Manager) ->
  erlang:send(Manager, {worker, self()}).
