-module('erlang.util.CountDownLatch').

-include("clojerl.hrl").

-export([ ?CONSTRUCTOR/1
        , count_down/1
        , await/1
        , await/2
        ]).

-export([loop/1]).

-type state() :: #{ count   => integer()
                  , waiting => [{pid(), reference()}]
                  }.

-type type()  :: #{ ?TYPE => ?M
                  , pid   => pid()
                  }.

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec ?CONSTRUCTOR(pos_integer()) -> type().
?CONSTRUCTOR(N) when N > 0 ->
  State = #{ count   => N
           , waiting => []
           },
  Pid   = proc_lib:spawn(?MODULE, loop, [State]),
  #{ ?TYPE => ?M
   , pid   => Pid
   }.

-spec count_down(type()) -> ?NIL.
count_down(#{?TYPE := ?M, pid := Pid}) ->
  Pid ! count_down,
  ?NIL.

-spec await(type()) -> boolean().
await(#{?TYPE := ?M} = Latch) ->
  await(Latch, infinity).

-spec await(type(), timeout()) -> boolean().
await(#{?TYPE := ?M, pid := Pid}, Timeout) ->
  case erlang:is_process_alive(Pid) of
    true ->
      Ref = erlang:make_ref(),
      Pid ! {await, erlang:self(), Ref},
      receive
        {await, Ref} -> true
      after
        Timeout ->
          Pid ! {remove, erlang:self(), Ref},
          false
      end;
    false ->
      true
  end.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec loop(state()) -> ok.
loop(State0) ->
  receive
    count_down ->
      case maps:get(count, State0) - 1 of
        0     -> notify(maps:get(waiting, State0));
        Count -> loop(State0#{count => Count})
      end;
    {await, Pid, Ref} ->
      Waiting = maps:get(waiting, State0),
      State1  = State0#{waiting => [{Pid, Ref} | Waiting]},
      loop(State1);
    {remove, Pid, Ref} ->
      Waiting = maps:get(waiting, State0),
      State1  = State0#{waiting => lists:delete({Pid, Ref}, Waiting)},
      loop(State1)
  end.

-spec notify([{pid(), reference()}]) -> ok.
notify([]) ->
  ok;
notify([{Pid, Ref} | Rest]) ->
  Pid ! {await, Ref},
  notify(Rest).
