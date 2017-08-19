-module('clojerl.Atom').

-include("clojerl.hrl").

-behaviour(gen_server).
-behavior('clojerl.IDeref').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.IStringable').

-export([ ?CONSTRUCTOR/1
        , swap/2
        , swap/3
        , swap/4
        , swap/5
        , reset/2
        , compare_and_set/3
        ]).

-export([deref/1]).
-export([equiv/2]).
-export([hash/1]).
-export([ meta/1
        , with_meta/2
        ]).
-export([str/1]).

%% gen_server callbacks
-export([ start_link/0
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-type type() :: #{ ?TYPE => ?M
                 , id    => binary()
                 , value => any()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(any()) -> type().
?CONSTRUCTOR(Value) ->
  UUID = 'erlang.util.UUID':random(),
  Id   = 'erlang.util.UUID':str(UUID),
  #{ ?TYPE => ?M
   , id    => Id
   , value => Value
   , meta  => ?NIL
   }.

-spec swap(type(), any()) -> any().
swap(#{?TYPE := ?M, id := Id, value := Value}, Fun) ->
  do_swap(Id, Value, Fun, []).

-spec swap(type(), any(), any()) -> any().
swap(#{?TYPE := ?M, id := Id, value := Value}, Fun, X) ->
  do_swap(Id, Value, Fun, [X]).

-spec swap(type(), any(), any(), any()) -> any().
swap(#{?TYPE := ?M, id := Id, value := Value}, Fun, X, Y) ->
  do_swap(Id, Value, Fun, [X, Y]).

-spec swap(type(), any(), any(), any(), any()) -> any().
swap(#{?TYPE := ?M, id := Id, value := Value}, Fun, X, Y, Args) ->
  do_swap(Id, Value, Fun, [X, Y | clj_rt:to_list(Args)]).

-spec reset(type(), any()) -> any().
reset(#{?TYPE := ?M, id := Id, value := Initial}, Value) ->
  do_reset(Id, Initial, Value).

-spec compare_and_set(type(), any(), any()) -> any().
compare_and_set(#{?TYPE := ?M, id := Id}, Old, New) ->
  case do_compare_and_set(Id, Old, New) of
    {ok, New} -> true;
    not_set   -> false
  end.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec do_swap(binary(), any(), any(), [any()]) -> any().
do_swap(AtomId, Initial, Fun, Args) ->
  Current = current(AtomId, Initial),
  New = clj_rt:apply(Fun, [Current | Args]),
  case do_compare_and_set(AtomId, Current, New) of
    {ok, New} -> New;
    not_set -> do_swap(AtomId, Initial, Fun, Args)
  end.

-spec do_reset(binary(), any(), any()) -> any().
do_reset(AtomId, Initial, New) ->
  Current = current(AtomId, Initial),
  case do_compare_and_set(AtomId, Current, New) of
    {ok, New} -> New;
    not_set -> do_reset(AtomId, Initial, New)
  end.

-spec do_compare_and_set(binary(), any(), any()) -> {ok, any()} | not_set.
do_compare_and_set(AtomId, Old, New) ->
  gen_server:call(?MODULE, {compare_and_set, AtomId, Old, New}).

-spec current(binary(), any()) -> any().
current(AtomId, Initial) ->
  case clj_utils:ets_get(?MODULE, AtomId) of
    {AtomId, Value} -> Value;
    ?NIL -> Initial
  end.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

str(#{?TYPE := ?M, id := Id}) ->
  <<"#<clojerl.Atom ", Id/binary, ">">>.

deref(#{?TYPE := ?M, id := Id, value := Initial} = _Atom) ->
  current(Id, Initial).

equiv( #{?TYPE := ?M, id := Id1}
     , #{?TYPE := ?M, id := Id2}
     ) ->
  Id1 =:= Id2;
equiv(_, _) ->
  false.

hash(#{?TYPE := ?M, id := Id}) ->
  erlang:phash2(Id).

meta(#{?TYPE := ?M, meta := Metadata}) ->
  Metadata.

with_meta(#{?TYPE := ?M} = Atom, Metadata) ->
  Atom#{meta => Metadata}.

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(?MODULE, [named_table, set, protected, {keypos, 1}]),
  {ok, ?NIL}.

handle_call({compare_and_set, AtomId, Old, New}, _From, State) ->
  Reply = case clj_utils:ets_get(?MODULE, AtomId) of
            {AtomId, Current} when Current =/= Old ->
              not_set;
            _ ->
              %% Otherwise it wasn't even there or it was the same
              clj_utils:ets_save(?MODULE, {AtomId, New}),
              {ok, New}
          end,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Msg, State) ->
  {ok, State}.

code_change(_Msg, _From, State) ->
  {ok, State}.
