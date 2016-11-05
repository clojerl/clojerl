-module('clojerl.MultiFn').

-include("clojerl.hrl").

-behaviour(gen_server).

-export([ get_method/2
        , get_method/4
        , get_method_table/1
        , add_method/3
        , remove_all/1
        , remove_method/2
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

-define(METHODS, ?MODULE).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec get_method(binary(), any()) -> any().
get_method(Name, Value) ->
  get_method(Name, Value, default, ?NIL).

-spec get_method(binary(), any(), any(), map()) -> any().
get_method(Name, Value, Default, _Hierarchy) ->
  case get(?MODULE, {Name, 'clojerl.IHash':hash(Value)}) of
    ?NIL ->
      case get(?MODULE, {Name, 'clojerl.IHash':hash(Default)}) of
        ?NIL -> ?NIL;
        Method -> Method#multifn.method
      end;
    Method -> Method#multifn.method
  end.

-spec get_method_table(binary()) -> any().
get_method_table(Name) ->
  MultiFns = ets:match(?MODULE, by_name(Name)),
  AddMultiFn = fun([_, V, M], Map) ->
                   maps:put(V, M, Map)
               end,
  lists:foldl(AddMultiFn, #{}, MultiFns).

-spec add_method(binary(), any(), any()) -> any().
add_method(Name, Value, Method) ->
  Hash = 'clojerl.IHash':hash(Value),
  gen_server:call( ?MODULE
                 , {add_method, Name, Value, Hash, Method}
                 ).

-spec remove_all(binary()) -> boolean().
remove_all(Name) ->
  true = gen_server:call(?MODULE, {remove_all, Name}).

-spec remove_method(binary(), any()) -> boolean().
remove_method(Name, Value) ->
  gen_server:call(?MODULE, {remove_method, Name, 'clojerl.IHash':hash(Value)}).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(?METHODS, [named_table, set, protected, {keypos, 2}]),
  {ok, ?NIL}.

handle_call({add_method, Name, Value, Hash, Method}, _From, State) ->
  #multifn{} = new_method(Name, Value, Hash, Method),
  {reply, ok, State};
handle_call({remove_all, Name}, _From, State) ->
  true = ets:match_delete(?METHODS, by_name(Name)),
  {reply, true, State};
handle_call({remove_method, Name, Hash}, _From, State) ->
  true = ets:delete(?METHODS, {Name, Hash}),
  {reply, true, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec new_method(binary(), any(), integer(), any()) -> multifn().
new_method(Name, Value, Hash, Method) ->
  MultiFn = #multifn{ id     = {Name, Hash}
                    , name   = Name
                    , value  = Value
                    , method = Method
                    },
  save(?METHODS, MultiFn).

-spec save(ets:tid(), term()) -> term().
save(Table, Value) ->
  true = ets:insert(Table, Value),
  Value.

-spec get(ets:tid(), term()) -> term().
get(Table, Id) ->
  case ets:lookup(Table, Id) of
    [] -> ?NIL;
    [Value] -> Value
  end.

-spec by_name(binary()) -> multifn().
by_name(Name) ->
  #multifn{ id     = {Name, '_'}
          , name   = '$1'
          , value  = '$2'
          , method = '$3'
          }.
