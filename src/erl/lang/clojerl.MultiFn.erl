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

-record(multifn, { key    :: {non_neg_integer(), any()},
                   id     :: non_neg_integer() | '$1',
                   value  :: any(),
                   method :: any()
                 }).

-type multifn() :: #multifn{}.

-define(METHODS, ?MODULE).
-define(METHODS_HEIR, 'clojerl.MultiFn.Heir').

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec get_method('clojerl.Var':type(), any()) -> any().
get_method(Var, Value) ->
  get_method(Var, Value, default, ?NIL).

-spec get_method('clojerl.Var':type(), any(), any(), map() | ?NIL) -> any().
get_method(Var, Value, Default, _Hierarchy) ->
  Id = 'clojerl.Var':hash(Var),
  case clj_utils:ets_get(?MODULE, {Id, clj_rt:hash(Value)}) of
    ?NIL ->
      case clj_utils:ets_get(?MODULE, {Id, clj_rt:hash(Default)}) of
        ?NIL -> ?NIL;
        Method -> Method#multifn.method
      end;
    Method -> Method#multifn.method
  end.

-spec get_method_table('clojerl.Var':type()) -> any().
get_method_table(Var) ->
  Id        = 'clojerl.Var':hash(Var),
  MultiFns  = ets:match(?MODULE, by_name(Id)),
  AddMultiFn = fun([_, V, M], Map) ->
                   maps:put(V, M, Map)
               end,
  lists:foldl(AddMultiFn, #{}, MultiFns).

-spec add_method('clojerl.Var':type(), any(), any()) -> any().
add_method(Var, Value, Method) ->
  Id   = 'clojerl.Var':hash(Var),
  Hash = clj_rt:hash(Value),
  gen_server:call( ?MODULE
                 , {add_method, Id, Value, Hash, Method}
                 ).

-spec remove_all('clojerl.Var':type()) -> boolean().
remove_all(Var) ->
  Id = 'clojerl.Var':hash(Var),
  true = gen_server:call(?MODULE, {remove_all, Id}).

-spec remove_method('clojerl.Var':type(), any()) -> boolean().
remove_method(Var, Value) ->
  Id = 'clojerl.Var':hash(Var),
  gen_server:call(?MODULE, {remove_method, Id, clj_rt:hash(Value)}).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {Pid, Ref} = init_heir(),
  ok         = init_ets(Pid),
  {ok, Ref}.

handle_call({add_method, Id, Value, Hash, Method}, _From, State) ->
  #multifn{} = new_method(Id, Value, Hash, Method),
  {reply, ok, State};
handle_call({remove_all, Id}, _From, State) ->
  true = ets:match_delete(?METHODS, by_name(Id)),
  {reply, true, State};
handle_call({remove_method, Id, Hash}, _From, State) ->
  true = ets:delete(?METHODS, {Id, Hash}),
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

-spec init_heir() -> {pid() | port(), reference()}.
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

-spec new_method(non_neg_integer(), any(), integer(), any()) -> multifn().
new_method(Id, Value, Hash, Method) ->
  MultiFn = #multifn{ key    = {Id, Hash}
                    , id     = Id
                    , value  = Value
                    , method = Method
                    },
  clj_utils:ets_save(?METHODS, MultiFn).

-spec by_name(non_neg_integer()) -> multifn().
by_name(Id) ->
  #multifn{ key    = {Id, '_'}
          , id     = '$1'
          , value  = '$2'
          , method = '$3'
          }.
