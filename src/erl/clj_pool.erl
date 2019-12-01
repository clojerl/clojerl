-module(clj_pool).

-behaviour(gen_server).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).

-export([ start_link/2
        , call/4
        , cast/4
        ]).

-type name()    :: atom().
-type options() :: #{workers => integer()}.
-type state()   :: #{ supervisor => pid()
                    , available  => queue:queue()
                    , pending    => queue:queue()
                    }.

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec start_link(name(), options()) -> {ok, pid()}.
start_link(Name, Options) ->
  gen_server:start_link({local, Name}, ?MODULE, Options, []).

-spec call(name(), module(), atom(), [any()]) -> any().
call(Name, M, F, Args) ->
  gen_server:call(Name, {M, F, Args}).

-spec cast(name(), module(), atom(), [any()]) -> any().
cast(Name, M, F, Args) ->
  gen_server:cast(Name, {M, F, Args}).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

-spec init(any()) -> {ok, state()}.
init(Options) ->
  WorkersCount = maps:get(workers, Options, 10),
  {ok, Sup}    = clj_pool_sup:start_link(),
  Workers = [ begin
                {ok, Child} = supervisor:start_child(Sup, [self()]),
                Child
              end
              || _ <- lists:seq(1, WorkersCount)
            ],
  State = #{ supervisor => Sup
           , available  => queue:from_list(Workers)
           , pending    => queue:new()
           },
  {ok, State}.

-spec handle_call(any(), any(), any()) -> {reply, any(), any()}.
handle_call({_M, _F, _Args} = MFA, From, State) ->
  #{ available := Available0
   , pending   := Pending
   } = State,
  Work = {call, From, MFA},
  case queue:out(Available0) of
    {empty, _} ->
      {noreply, State#{pending => queue:cons(Work, Pending)}};
    {{value, Worker}, Available1} ->
      ok = gen_server:cast(Worker, Work),
      {noreply, State#{available => Available1}}
  end.

-spec handle_cast(any(), any()) -> {noreply, any()}.
handle_cast({_M, _F, _Args} = MFA, State) ->
  #{ available := Available0
   , pending   := Pending
   } = State,
  Work = {cast, MFA},
  case queue:out(Available0) of
    {empty, _} ->
      {noreply, State#{pending => queue:cons(Work, Pending)}};
    {{value, Worker}, Available1} ->
      ok = gen_server:cast(Worker, Work),
      {noreply, State#{available => Available1}}
  end.

-spec handle_info({worker, pid()}, state()) -> {noreply, state()}.
handle_info({worker, Worker}, State) ->
  #{ available := Available
   , pending   := Pending0
   } = State,
  case queue:out(Pending0) of
    {empty, _} ->
      {noreply, State#{available => queue:cons(Worker, Available)}};
    {{value, Work}, Pending1}->
      ok = gen_server:cast(Worker, Work),
      {noreply, State#{pending => Pending1}}
  end.
