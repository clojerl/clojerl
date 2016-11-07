-module(clj_cache).

-behavior(gen_server).

-export([ get/1
        , put/2
        ]).

%% gen_server callbacks
-export([ start_link/0
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------

-spec get(any()) -> undefined | {ok, any()}.
get(Key) ->
  case ets:lookup(?MODULE, Key) of
    [] -> undefined;
    [{Key, Value}] -> {ok, Value}
  end.

-spec put(any(), any()) -> ok.
put(Key, Value) ->
  gen_server:call(?MODULE, {put, Key, Value}).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(?MODULE, [named_table, set, protected, {keypos, 1}]),
  {ok, undefined}.

handle_call({put, Key, Value}, _From, State) ->
  true = ets:insert(?MODULE, {Key, Value}),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {ok, State}.

handle_info(_Msg, State) ->
  {ok, State}.

terminate(_Msg, State) ->
  {ok, State}.

code_change(_Msg, _From, State) ->
  {ok, State}.
