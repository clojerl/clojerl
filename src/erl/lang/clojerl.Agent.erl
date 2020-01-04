-module('clojerl.Agent').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behaviour(gen_server).
-behavior('erlang.io.ICloseable').
-behavior('clojerl.IDeref').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.IReference').
-behavior('clojerl.IStringable').

%% API
-export([?CONSTRUCTOR/1]).

-export([ dispatch/3
        , error/1
        , error_handler/1
        , error_handler/2
        , error_mode/1
        , error_mode/2
        , release_pending_sends/0
        , restart/3
        , validator/1
        , validator/2
        ]).

%% Protocols
-export([close/1]).
-export([deref/1]).
-export([equiv/2]).
-export([hash/1]).
-export([ meta/1
        , with_meta/2
        ]).
-export([ alter_meta/3
        , reset_meta/2
        ]).
-export([str/1]).

%% gen_server callbacks
-export([ start_link/3
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-define(PENDING_SENDS,  '__pending_sends__').

-type type()        :: #{ ?TYPE => ?M
                        , id    => binary()
                        , pid   => pid()
                        }.

-type error_mode()  :: continue | fail.

-type agent_state() :: #{ value         => any()
                        , error_handler => any()
                        , error_mode    => error_mode()
                        , error         => any()
                        , validator     => any()
                        , meta          => any()
                        }.

-type state()       :: #{ id      => binary()
                        , owner   => pid()
                        , monitor => reference()
                        , queued  => [{Fn :: any(), Args :: any()}]
                        }.

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec ?CONSTRUCTOR(any()) -> type().
?CONSTRUCTOR(Value) ->
  UUID      = 'erlang.util.UUID':random(),
  Id        = 'erlang.util.UUID':str(UUID),
  {ok, Pid} = 'clojerl.Agent.Server':create(erlang:self(), Id, Value),
  build(Id, Pid).

-spec dispatch(type(), any(), any()) -> type().
dispatch(#{?TYPE := ?M, id :=  Id, pid := Pid} = Agent, Fun, Args) ->
  case is_running_action() of
    true ->
      add_pending_send(Fun, Args),
      Agent;
    false ->
      case field(Id, error) of
        ?NIL ->
          ok = gen_server:cast(Pid, {send, Fun, Args}),
          Agent;
        Error -> erlang:error(Error)
      end
  end.

-spec error(type()) -> any().
error(#{?TYPE := ?M, id := Id}) ->
  field(Id, error).

-spec error_handler(type()) -> function().
error_handler(#{?TYPE := ?M, id := Id}) ->
  field(Id, error_handler).

-spec error_handler(type(), function()) -> any().
error_handler(#{?TYPE := ?M, pid := Pid}, ErrorHandler) ->
  field(Pid, error_handler, ErrorHandler).

-spec error_mode(type()) -> atom().
error_mode(#{?TYPE := ?M, id := Id}) ->
  field(Id, error_mode).

-spec error_mode(type(), atom()) -> any().
error_mode(#{?TYPE := ?M, pid := Pid}, ErrorMode) ->
  field(Pid, error_mode, ErrorMode).

-spec build(binary(), pid()) -> type().
build(Id, Pid) ->
  #{ ?TYPE => ?M
   , id    => Id
   , pid   => Pid
   }.

-spec release_pending_sends() -> non_neg_integer().
release_pending_sends() ->
  Pid = erlang:self(),
  case erlang:get(?PENDING_SENDS) of
    ?NIL -> 0;
    Pending ->
      [ gen_server:cast(Pid, {send, Fun, Args})
        || {Fun, Args} <- lists:reverse(Pending)
      ],
      length(Pending)
  end.

-spec restart(type(), any(), boolean()) -> any().
restart(#{?TYPE := ?M, id := Id, pid := Pid}, NewValue, ClearActions) ->
  case field(Id, error) =:= ?NIL of
    true -> ?ERROR(<<"Agent does not need a restart">>);
    false ->
      case gen_server:call(Pid, {restart, NewValue, ClearActions}) of
        ok -> NewValue;
        {error, Error} -> erlang:error(Error)
      end
  end.

-spec validator(type()) -> any().
validator(#{?TYPE := ?M, id := Id}) ->
  field(Id, validator).

-spec validator(type(), function()) -> any().
validator(#{?TYPE := ?M, pid := Pid}, Validator) ->
  field(Pid, validator, Validator).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICloseable

close(#{?TYPE := ?M, pid := Pid}) ->
  ok = gen_server:stop(Pid),
  ?NIL.

%% clojerl.IDeref

deref(#{?TYPE := ?M, id := Id}) ->
  field(Id, value).

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, id := Id1}
     , #{?TYPE := ?M, id := Id2}
     ) ->
  Id1 =:= Id2;
equiv(_, _) ->
  false.

%% clojerl.IHash

hash(#{?TYPE := ?M, id := Id}) ->
  erlang:phash2(Id).

%% clojerl.IMeta

meta(#{?TYPE := ?M, id := Id}) ->
  field(Id, meta).

with_meta(#{?TYPE := ?M, pid := Pid} = Agent, Meta) ->
  field(Pid, meta, Meta),
  Agent.

%% clojerl.IReference

-spec alter_meta(type(), any(), any()) -> any().
alter_meta(#{?TYPE := ?M, pid := Pid}, Fun, Args) ->
  case gen_server:call(Pid, {alter_meta, Fun, Args}) of
    {ok, Meta} -> Meta;
    {error, {Class, Reason}} -> erlang:raise(Class, Reason, [])
  end.

-spec reset_meta(type(), any()) -> any().
reset_meta(#{?TYPE := ?M, pid := Pid}, Meta) ->
  field(Pid, meta, Meta).

%% clojerl.IStringable

str(#{?TYPE := ?M, id := Id}) ->
  <<"#<clojerl.Agent ", Id/binary, ">">>.

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

-spec start_link(pid(), binary(), any()) -> {ok, pid()} | {error, any()}.
start_link(Owner, Id, Value) ->
  gen_server:start_link(?MODULE, {Owner, Id, Value}, []).

-spec init({pid(), binary(), any()}) -> {ok, state()}.
init({Owner, Id, Value}) ->
  AgentState = default_state(Value),
  _          = clj_utils:ets_save(?AGENT_TABLE, {Id, AgentState}),
  Ref        = erlang:monitor(process, Owner),
  State      = #{ id      => Id
                , owner   => Owner
                , monitor => Ref
                , queued  => []
                },
  {ok, State}.

-spec handle_call({restart, any(), boolean()}, any(), state()) ->
  {reply, any(), state()}.
handle_call({restart, NewValue, ClearActions}, _From, #{id := Id} = State0) ->
  {_, AgentState0} = clj_utils:ets_get(?AGENT_TABLE, Id),
  Reply = try validate(AgentState0, NewValue) of
            AgentState1 ->
              X = {Id, AgentState1#{error => ?NIL}},
              _ = clj_utils:ets_save(?AGENT_TABLE, X),
              ok
          catch _:E -> {error, E}
          end,
  %% Handle queued actions
  State1 = case Reply of
             ok when ClearActions ->
               State0#{queued => []};
             ok ->
               Queued = maps:get(queued, State0),
               %% Re-cast queued actions
               [ gen_server:cast(erlang:self(), {send, Fn, Args})
                 || {Fn, Args} <- lists:reverse(Queued)
               ],
               State0#{queued => []};
             _ ->
               State0
           end,
  {reply, Reply, State1};
handle_call({set_field, Name, Value}, _From, #{id := Id} = State) ->
  {_, AgentState} = clj_utils:ets_get(?AGENT_TABLE, Id),
  _ = clj_utils:ets_save(?AGENT_TABLE, {Id, AgentState#{Name => Value}}),
  {reply, ok, State};
handle_call({alter_meta, Fun, Args}, _From, #{id := Id} = State) ->
  {_, AgentState0}  = clj_utils:ets_get(?AGENT_TABLE, Id),
  #{meta := Meta0} = AgentState0,
  Reply = try
            Meta1 = clj_rt:apply(Fun, clj_rt:cons(Meta0, Args)),
            AgentState1 = AgentState0#{meta => Meta1},
              _ = clj_utils:ets_save(?AGENT_TABLE, {Id, AgentState1}),
            {ok, Meta1}
          catch Class:Reason ->
              {error, {Class, Reason}}
          end,
  {reply, Reply, State}.

-spec handle_cast({send, function(), any()}, state()) ->
  {noreply, state()}.
handle_cast({send, Fun, Args}, #{id := Id} = State0) ->
  {_, AgentState0} = clj_utils:ets_get(?AGENT_TABLE, Id),
  case AgentState0 of
    #{error := ?NIL} ->
      AgentState1 = apply_action(Id, AgentState0, Fun, Args),
      _           = clj_utils:ets_save(?AGENT_TABLE, {Id, AgentState1}),
      {noreply, State0};
    _ ->
      Queued = maps:get(queued, State0),
      State1 = State0#{queued => [{Fun, Args} | Queued]},
      {noreply, State1}
  end.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, _, _, Reason}, #{monitor := Ref} = State) ->
  {stop, Reason, State};
handle_info(_Msg, State) ->
  {noreply, State}.

-spec terminate(any(), state()) -> {ok, state()}.
terminate(_Msg, State) ->
  {ok, State}.

-spec code_change(any(), any(), state()) ->
  {ok, state()}.
code_change(_Msg, _From, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec default_state(any()) -> agent_state().
default_state(Value) ->
  #{ value         => Value
   , error_handler => ?NIL
   , error_mode    => continue
   , error         => ?NIL
   , validator     => ?NIL
   , meta          => ?NIL
   }.

-spec init_pending_sends() -> ok.
init_pending_sends() ->
  erlang:put(?PENDING_SENDS, []),
  ok.

-spec clear_pending_sends() -> ok.
clear_pending_sends() ->
  erlang:erase(?PENDING_SENDS),
  ok.

-spec is_running_action() -> boolean().
is_running_action() ->
  clj_rt:boolean(erlang:get(?PENDING_SENDS)).

-spec add_pending_send(any(), any()) -> ok.
add_pending_send(Fun, Args) ->
  Pending = erlang:get(?PENDING_SENDS),
  erlang:put(?PENDING_SENDS, [{Fun, Args} | Pending]).

-spec apply_action(binary(), any(), function(), any()) -> any().
apply_action(Id, #{value := Value0} = AgentState0, Fun, Args) ->
  try
    init_pending_sends(),
    Value1      = clj_rt:apply(Fun, clj_rt:cons(Value0, Args)),
    AgentState1 = validate(AgentState0, Value1),
    'clojerl.Agent':release_pending_sends(),
    AgentState1
  catch
    _:Error ->
      clear_pending_sends(),
      handle_error(Id, AgentState0, Error),
      handle_error_mode(AgentState0, Error)
  after
      clear_pending_sends()
  end.

-spec validate(any(), any()) -> any().
validate(#{validator := ?NIL} = AgentState, Value) ->
  AgentState#{value => Value};
validate(#{validator := Validator} = AgentState, Value) ->
  case clj_rt:apply(Validator, [Value]) of
    true  -> AgentState#{value => Value};
    false -> ?ERROR(validation_failed)
  end.

-spec handle_error(binary(), any(), any()) -> any().
handle_error(_Id, #{error_handler := ?NIL}, _Error) ->
  ok;
handle_error(Id, #{error_handler := ErrorHandler}, Error) ->
  try
    Agent = build(Id, erlang:self()),
    clj_rt:apply(ErrorHandler, [Agent, Error])
  catch _:_ -> ok %% ignore ErrorHandler errors
  end.

-spec handle_error_mode(any(), any()) -> any().
handle_error_mode(#{error_mode := continue} = AgentState, _Error) ->
  AgentState;
handle_error_mode(#{error_mode := fail} = AgentState, Error) ->
  AgentState#{error => Error}.

-spec field(binary(), atom()) -> any().
field(Id, Name) ->
  {_, AgentState} = clj_utils:ets_get(?AGENT_TABLE, Id),
  maps:get(Name, AgentState, ?NIL).

-spec field(pid(), atom(), any()) -> any().
field(Pid, Name, Value) ->
  ok = gen_server:call(Pid, {set_field, Name, Value}),
  Value.
