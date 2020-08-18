%% @doc Generic cache module.
%%
%% Implements the `gen_server' behaviour and on initialization creates
%% an in-memory ETS table where to keep the cached values.
%%
%% This cache is used for optimizing access to certain values. For
%% example to avoid recalculating certain information regarding a
%% module. Or to fetch the binary BEAM from memory instead of looking
%% for it in a file's chunks.
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

%% @doc Gets the value for `Key' or `undefined' if not found.
-spec get(any()) -> undefined | {ok, any()}.
get(Key) ->
  case ets:lookup(?MODULE, Key) of
    [] -> undefined;
    [{Key, Value}] -> {ok, Value}
  end.

%% @doc Adds or updates the `Value' for `Key'.
-spec put(any(), any()) -> ok.
put(Key, Value) ->
  gen_server:call(?MODULE, {put, Key, Value}).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

%% @private
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
init([]) ->
  ets:new(?MODULE, [named_table, set, protected, {keypos, 1}]),
  {ok, undefined}.

%% @private
handle_call({put, Key, Value}, _From, State) ->
  true = ets:insert(?MODULE, {Key, Value}),
  {reply, ok, State}.

%% @private
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(_Msg, State) ->
  {noreply, State}.

%% @private
terminate(_Msg, State) ->
  {ok, State}.

%% @private
code_change(_Msg, _From, State) ->
  {ok, State}.
