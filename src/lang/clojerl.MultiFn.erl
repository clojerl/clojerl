-module('clojerl.MultiFn').
-behaviour(gen_server).

-export([ get_method/4
        , add_method/3
        ]).

%% gen_server callbacks
-export([ start_link/0
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(multifn, { id     :: {binary(), any()},
                   name   :: binary(),
                   value  :: any(),
                   method :: any()
                 }).

-type multifn() :: #multifn{}.

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec get_method(binary(), any(), any(), map()) -> any().
get_method(Name, Value, Default, _Hierarchy) ->
  case get(?MODULE, {Name, Value}) of
    undefined ->
      case get(?MODULE, {Name, Default}) of
        undefined -> undefined;
        Method -> Method#multifn.method
      end;
    Method -> Method#multifn.method
  end.

-spec add_method(binary(), any(), any()) -> any().
add_method(Name, Value, Method) ->
  gen_server:call(?MODULE, {add_method, Name, Value, Method}).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(?MODULE, [named_table, set, protected, {keypos, 2}]),
  {ok, undefined}.

handle_call({add_method, Name, Value, Method}, _From, State) ->
  #multifn{} = new_method(Name, Value, Method),
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

-spec new_method(binary(), any(), any()) -> multifn().
new_method(Name, Value, Method) ->
  MultiFn = #multifn{ id     = {Name, Value}
                    , name   = Name
                    , value  = Value
                    , method = Method
                    },
  save(?MODULE, MultiFn).

-spec save(ets:tid(), term()) -> term().
save(Table, Value) ->
  true = ets:insert(Table, Value),
  Value.

-spec get(ets:tid(), term()) -> term().
get(Table, Id) ->
  case ets:lookup(Table, Id) of
    [] -> undefined;
    [Value] -> Value
  end.
