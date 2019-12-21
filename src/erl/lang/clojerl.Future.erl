-module('clojerl.Future').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behaviour(gen_server).

-behavior('erlang.io.ICloseable').
-behavior('clojerl.IBlockingDeref').
-behavior('clojerl.IDeref').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IPending').
-behavior('clojerl.IStringable').

-export([ ?CONSTRUCTOR/1
        , cancel/1
        , 'cancelled?'/1
        , 'done?'/1
        ]).

-export([close/1]).
-export([deref/3]).
-export([deref/1]).
-export(['realized?'/1]).
-export([equiv/2]).
-export([hash/1]).
-export([str/1]).

%% eval
-export([ eval/2
        ]).

%% gen_server callbacks
-export([ start_link/2
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-type type() :: #{ ?TYPE => ?M
                 , id    => binary()
                 , pid   => pid()
                 }.

-spec ?CONSTRUCTOR(any()) -> type().
?CONSTRUCTOR(Fn) ->
  UUID      = 'erlang.util.UUID':random(),
  Id        = 'erlang.util.UUID':str(UUID),
  {ok, Pid} = start_link(Id, Fn),
  #{ ?TYPE => ?M
   , id    => Id
   , pid   => Pid
   }.

-spec cancel(type()) -> ok.
cancel(#{?TYPE := ?M, pid := Pid}) ->
  ok = gen_server:stop(Pid).

-spec 'cancelled?'(type()) -> boolean().
'cancelled?'(#{?TYPE := ?M, pid := Pid}) ->
  erlang:is_process_alive(Pid).

-spec 'done?'(type()) -> boolean().
'done?'(#{?TYPE := ?M, pid := Pid}) ->
  gen_server:call(Pid, 'done?').

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICloseable

close(#{?TYPE := ?M, pid := Pid}) ->
  ok = gen_server:stop(Pid).

%% clojerl.IBlockingDeref

deref(#{?TYPE := ?M, pid := Pid}, TimeoutMs, TimeoutVal) ->
  try gen_server:call(Pid, deref, TimeoutMs) of
    {ok, Value}    -> Value;
    {error, Error} -> ?ERROR(Error)
  catch exit:{timeout, _} -> TimeoutVal
  end.

%% clojerl.IDeref

deref(#{?TYPE := ?M, pid := Pid}) ->
  case gen_server:call(Pid, deref, infinity) of
    {ok, Value} -> Value;
    {error, Error} -> ?ERROR(Error)
  end.

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, id := Id}
     , #{?TYPE := ?M, id := Id}
     ) ->
  true;
equiv(_, _) ->
  false.

%% clojerl.IHash

hash(#{?TYPE := ?M, id := Id}) ->
  erlang:phash2(Id).

%% clojerl.IPending

'realized?'(#{?TYPE := ?M, pid := Pid}) ->
  gen_server:call(Pid, deref) =/= ?NIL.

%% clojerl.IStringable

str(#{?TYPE := ?M, id := Id}) ->
  <<"#<clojerl.Future ", Id/binary, ">">>.

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

-type state() :: #{ id      => binary()
                  , fn      => function()
                  , result  => ?NIL | {ok, any()}
                  , pending => queue:queue()
                  }.

start_link(Id, Fn) ->
  gen_server:start_link(?MODULE, {Id, Fn}, []).

-spec init({binary(), function()}) -> {ok, state()}.
init({Id, Fn}) ->
  State = #{ id      => Id
           , fn      => Fn
           , result   => ?NIL
           , pending => queue:new()
           },
  proc_lib:spawn_link(?MODULE, eval, [self(), Fn]),
  {ok, State}.

handle_call(deref, From, #{result := ?NIL, pending := Pending} = State0) ->
  State1 = State0#{pending := queue:cons(From, Pending)},
  {noreply, State1};
handle_call(deref, _From, #{result := Result} = State) ->
  {reply, Result, State};
handle_call('done?', _From, #{result := Result} = State) ->
  {reply, Result =/= ?NIL, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({result, Result}, #{pending := Pending} = State) ->
  [gen_server:reply(From, Result) || From <- queue:to_list(Pending)],
  {noreply, State#{result := Result, pending := queue:new()}}.

terminate(_Msg, State) ->
  {ok, State}.

code_change(_Msg, _From, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec eval(pid(), function()) -> ok.
eval(Pid, Fn) ->
  Result = try {ok, clj_rt:apply(Fn, [])}
           catch _:Error -> {error, Error}
           end,
  Pid ! {result, Result},
  ok.
