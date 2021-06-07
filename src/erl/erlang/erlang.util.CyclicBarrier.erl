%% @doc A very simple and basic implementation of a synchronization
%% aid that allows a set of threads to all wait for each other to
%% reach a common barrier point.

-module('erlang.util.CyclicBarrier').

-include("clojerl.hrl").

-behaviour(gen_server).

-export([ ?CONSTRUCTOR/1
        , await/1
        , await/2
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-type state() :: #{ count   => integer()
                  , cycle   => integer()
                  , waiting => [any()]
                  }.

-type type()  :: #{ ?TYPE => ?M
                  , pid   => pid()
                  }.

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec ?CONSTRUCTOR(pos_integer()) -> type().
?CONSTRUCTOR(N) when N > 0 ->
  {ok, Pid} = gen_server:start_link(?MODULE, N, []),
  #{ ?TYPE => ?M
   , pid   => Pid
   }.

-spec await(type()) -> non_neg_integer() | timeout.
await(#{?TYPE := ?M} = Barrier) ->
  await(Barrier, infinity).

-spec await(type(), timeout()) -> non_neg_integer() | timeout.
await(#{?TYPE := ?M, pid := Pid}, Timeout) ->
  gen_server:call(Pid, await, Timeout).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

%% @private
-spec init(any()) -> {ok, state()}.
init(N) ->
  State = #{ count   => N
           , cycle   => N
           , waiting => []
           },
  {ok, State}.

%% @private
-spec handle_call(any(), any(), state()) -> {noreply, state()}.
handle_call( await
           , From
           , #{count := N, cycle := 1, waiting := Waiting} = State
           ) ->
  notify([From | Waiting]),
  {noreply, State#{cycle := N, waiting := []}};
handle_call(await, From, #{cycle := C, waiting := Waiting} = State) ->
  {noreply, State#{cycle := C - 1, waiting := [From | Waiting]}}.

%% @private
-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

%% @private
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Msg, State) ->
  {noreply, State}.

%% @private
-spec terminate(any(), state()) -> {ok, state()}.
terminate(_Msg, State) ->
  {ok, State}.

%% @private
-spec code_change(any(), any(), state()) -> {ok, state()}.
code_change(_Msg, _From, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec notify([{pid(), any()}]) -> ok.
notify([]) ->
  ok;
notify([From | Rest]) ->
  gen_server:reply(From, ok),
  notify(Rest).
