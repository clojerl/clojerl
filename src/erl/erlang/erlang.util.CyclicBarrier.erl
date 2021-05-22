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

-spec init(any()) -> {ok, state()}.
init(N) ->
  State = #{ count   => N
           , cycle   => N
           , waiting => []
           },
  {ok, State}.

-spec handle_call(any(), any(), state()) -> {noreply, state()}.
handle_call( await
           , From
           , #{count := N, cycle := 1, waiting := Waiting} = State
           ) ->
  notify([From | Waiting]),
  {noreply, State#{cycle := N, waiting := []}};
handle_call(await, From, #{cycle := C, waiting := Waiting} = State) ->
  {noreply, State#{cycle := C - 1, waiting := [From | Waiting]}}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec notify([{pid(), any()}]) -> ok.
notify([]) ->
  ok;
notify([From | Rest]) ->
  gen_server:reply(From, ok),
  notify(Rest).
