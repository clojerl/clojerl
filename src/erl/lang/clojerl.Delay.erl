-module('clojerl.Delay').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behaviour(gen_server).

-behavior('erlang.io.ICloseable').
-behavior('clojerl.IDeref').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IPending').
-behavior('clojerl.IStringable').

-export([ ?CONSTRUCTOR/1
        , force/1
        ]).

-export([close/1]).
-export([deref/1]).
-export(['realized?'/1]).
-export([equiv/2]).
-export([hash/1]).
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
                 , fn    => function()
                 }.

-spec ?CONSTRUCTOR(any()) -> type().
?CONSTRUCTOR(Fn) ->
  UUID = 'erlang.util.UUID':random(),
  Id   = 'erlang.util.UUID':str(UUID),
  #{ ?TYPE => ?M
   , id    => Id
   , fn    => Fn
   }.

-spec force(type()) -> any().
force(#{?TYPE := ?M} = X) -> deref(X);
force(X) -> X.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICloseable

close(#{?TYPE := ?M, id := Id}) ->
  true = ets:delete(?MODULE, Id),
  ok.

%% clojerl.IDeref

deref(#{?TYPE := ?M, id := Id, fn := Fn}) ->
  Result = case clj_utils:ets_get(?MODULE, Id) of
             ?NIL -> gen_server:call(?MODULE, {deref, Id, Fn}, infinity);
             {Id, R} -> R
           end,
  case Result of
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

'realized?'(#{?TYPE := ?M, id := Id}) ->
  clj_utils:ets_get(?MODULE, Id) =/= ?NIL.

%% clojerl.IStringable

str(#{?TYPE := ?M, id := Id}) ->
  <<"#<clojerl.Delay ", Id/binary, ">">>.

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(?MODULE, [named_table, set, public, {keypos, 1}]),
  {ok, ?NIL}.

handle_call({deref, Id, Fn}, _From, State) ->
  {Id, Reply} = clj_utils:ets_save(?MODULE, {Id, eval(Fn)}),
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Msg, State) ->
  {ok, State}.

code_change(_Msg, _From, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

eval(Fn) ->
  try {ok, clj_rt:apply(Fn, [])}
  catch _:Error -> {error, Error}
  end.
