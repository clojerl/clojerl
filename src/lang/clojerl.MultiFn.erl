-module('clojerl.MultiFn').
-behaviour(gen_server).

-include("clojerl.hrl").

-behavior('clojerl.Stringable').

-export([create/4]).

-export(['clojerl.Stringable.str'/1]).

%% gen_server callbacks
-export([ start_link/0
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-type type() :: #?TYPE{data :: binary()}.

-record(multifn, { name      :: binary(),
                   dispatch  :: function(),
                   default   :: term(),
                   hierarchy :: term()
                 }).

-type multifn() :: #multifn{}.

-spec create(binary(), function(), term(), term()) -> type().
create(Name, DispatchFn, Default, Hierarchy) ->
  ok = gen_server:call(?MODULE, {create, Name, DispatchFn, Default, Hierarchy}),
  #?TYPE{data = Name}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Stringable.str'(#?TYPE{data = Name}) ->
  <<"#<MultiFn ", Name/binary, ">">>.

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(?MODULE, [named_table, set, protected, {keypos, 2}]),
  {ok, undefined}.

handle_call({create, Name, DispatchFn, Default, Hierarchy}, _From, State) ->
  #multifn{} = new(Name, DispatchFn, Default, Hierarchy),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {ok, State}.

handle_info(_Msg, State) ->
  {ok, State}.

terminate(_Msg, State) ->
  {ok, State}.

code_change(_Msg, _From, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec new(binary(), function(), term(), term()) -> multifn().
new(Name, DispatchFn, Default, Hierarchy) ->
  MultiFn = #multifn{ name      = Name
                    , dispatch  = DispatchFn
                    , default   = Default
                    , hierarchy = Hierarchy
                    },
  save(?MODULE, MultiFn).

-spec save(ets:tid(), term()) -> term().
save(Table, Value) ->
  true = ets:insert(Table, Value),
  Value.
