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
-define(METHODS_HEIR, 'clojerl.MultiFn.Heir').

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
  {Pid, Ref} = init_heir(),
  ok         = init_ets(Pid),
  {ok, Ref}.

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

handle_info({'DOWN', Ref, process, _Pid, _Reason}, Ref) ->
  {NewPid, NewRef} = init_heir(),
  true       = ets:setopts(?METHODS, {heir, NewPid, undefined}),
  {noreply, NewRef};
handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec init_heir() -> ok.
init_heir() ->
  case erlang:whereis(?METHODS_HEIR) of
    undefined ->
      {Pid, Ref}  = spawn_monitor(fun() -> heir_loop(heir) end),
      true = erlang:register(?METHODS_HEIR, Pid),
      {Pid, Ref};
    Pid ->
      {Pid, erlang:monitor(process, Pid)}
  end.

-spec heir_loop(owner | heir) -> ok.
heir_loop(State) ->
  receive
    {'ETS-TRANSFER', ?METHODS, _FromPid, _HeirData} ->
      heir_loop(owner);
    {transfer, Pid} when State =:= owner->
      true = ets:give_away(?METHODS, Pid, undefined),
      heir_loop(heir)
  end.

-spec init_ets(pid()) -> ok.
init_ets(Pid) ->
  case ets:info(?METHODS) of
    undefined ->
      Opts = [named_table, set, protected, {keypos, 2}, {heir, Pid, undefined}],
      ets:new(?METHODS, Opts);
    _ ->
      Pid ! {transfer, self()}
  end,
  ok.

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
