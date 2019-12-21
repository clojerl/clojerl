-module('clojerl.Promise').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behaviour(gen_server).

-behavior('erlang.io.ICloseable').
-behavior('clojerl.IBlockingDeref').
-behavior('clojerl.IDeref').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.IPending').
-behavior('clojerl.IStringable').

-export([ ?CONSTRUCTOR/0
        ]).

-export([close/1]).
-export([deref/3]).
-export([deref/1]).
-export(['realized?'/1]).
-export([equiv/2]).
-export([apply/2]).
-export([hash/1]).
-export([str/1]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-type type() :: #{ ?TYPE => ?M
                 , id    => binary()
                 , ref   => reference()
                 , atom  => 'clojerl.Atom':type()
                 , pid   => pid()
                 }.

-spec ?CONSTRUCTOR() -> type().
?CONSTRUCTOR() ->
  UUID      = 'erlang.util.UUID':random(),
  Id        = 'erlang.util.UUID':str(UUID),
  Ref       = erlang:make_ref(),
  Atom      = 'clojerl.Atom':?CONSTRUCTOR(Ref),
  {ok, Pid} = start_link(Id, Atom),
  #{ ?TYPE => ?M
   , id    => Id
   , ref   => Ref
   , atom  => Atom
   , pid   => Pid
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICloseable

close(#{?TYPE := ?M, pid := Pid}) ->
  ok = gen_server:stop(Pid).

%% clojerl.IBlockingDeref

deref(#{?TYPE := ?M, pid := Pid, atom := Atom}, TimeoutMs, TimeoutVal) ->
  try gen_server:call(Pid, deref, TimeoutMs) of
    ok -> 'clojerl.Atom':deref(Atom)
  catch exit:{timeout, _} -> TimeoutVal
  end.

%% clojerl.IDeref

deref(#{?TYPE := ?M, pid := Pid, atom := Atom}) ->
  ok = gen_server:call(Pid, deref, infinity),
  'clojerl.Atom':deref(Atom).

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, id := Id}
     , #{?TYPE := ?M, id := Id}
     ) ->
  true;
equiv(_, _) ->
  false.

%% clojerl.IFn

apply(#{?TYPE := ?M, pid := Pid} = Promise, Args) ->
  case clj_rt:to_list(Args) of
    [Value] ->
      gen_server:cast(Pid, {deliver, Value}),
      Promise;
    _ ->
      CountBin = integer_to_binary(length(Args)),
      ?ERROR(<<"Wrong number of args for promise, got: ", CountBin/binary>>)
  end.

%% clojerl.IHash

hash(#{?TYPE := ?M, id := Id}) ->
  erlang:phash2(Id).

%% clojerl.IPending

'realized?'(#{?TYPE := ?M, ref := Ref, atom := Atom}) ->
  'clojerl.Atom':deref(Atom) =/= Ref.

%% clojerl.IStringable

str(#{?TYPE := ?M, id := Id}) ->
  <<"#<clojerl.Promise ", Id/binary, ">">>.

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

-type state() :: #{ id        => binary()
                  , atom      => 'clojerl.Atom':type()
                  , delivered => boolean()
                  , pending   => queue:queue()
                  }.

start_link(Id, Atom) ->
  gen_server:start_link(?MODULE, {Id, Atom}, []).

-spec init({binary(), 'clojerl.Atom':type()}) -> {ok, state()}.
init({Id, Atom}) ->
  State = #{ id        => Id
           , atom      => Atom
           , delivered => false
           , pending   => queue:new()
           },
  {ok, State}.

handle_call(deref, _From, #{delivered := true} = State) ->
  {reply, ok, State};
handle_call(deref, From, #{delivered := false, pending := Pending} = State) ->
  {noreply, State#{pending := queue:cons(From, Pending)}}.

handle_cast( {deliver, Value}
           , #{delivered := false, pending := Pending, atom := Atom} = State
           ) ->
  'clojerl.Atom':reset(Atom, Value),
  [gen_server:reply(From, ok) || From <- queue:to_list(Pending)],
  {noreply, State#{delivered := true, pending := queue:new()}};
handle_cast( {deliver, _}
           , #{delivered := true, pending := Pending} = State
           ) ->
  [gen_server:reply(From, ok) || From <- queue:to_list(Pending)],
  {noreply, State#{pending := queue:new()}}.

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Msg, State) ->
  {ok, State}.

code_change(_Msg, _From, State) ->
  {ok, State}.
