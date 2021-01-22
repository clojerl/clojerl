%% @doc Server process that creates and maintains an ETS table with
%% the Clojure Agents information.
%% @private
-module('clojerl.Agent.Server').

-include("clojerl_int.hrl").

-behaviour(gen_server).

-export([create/3]).

-export([start_link/0]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-spec create(pid(), binary(), any()) -> {ok, pid()}.
create(Owner, Id, Value) ->
  gen_server:call(?MODULE, {create, Owner, Id, Value}).

-spec start_link() -> ignore | {error, term()} | {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

-spec init(term()) -> {ok, #{}}.
init([]) ->
  process_flag(trap_exit, true),
  ?AGENT_TABLE = ets:new(?AGENT_TABLE, [named_table, set, public, {keypos, 1}]),
  {ok, #{}}.

handle_call({create, Owner, Id, Value}, _From, State0) ->
  Reply = 'clojerl.Agent':start_link(Owner, Id, Value),
  State1 = case Reply of
            {ok, Pid} when is_pid(Pid) -> State0#{Pid => Id};
             _ -> State0
          end,
  {reply, Reply, State1}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, State0) when is_pid(Pid) ->
  %% Cleanup table
  Id     = maps:get(Pid, State0, undefined),
  true   = ets:delete(?AGENT_TABLE, Id),
  %% Remove process
  State1 = maps:remove(Pid, State0),
  {noreply, State1};
handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Msg, State) ->
  {ok, State}.

code_change(_Msg, _From, State) ->
  {ok, State}.
