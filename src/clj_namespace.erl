-module(clj_namespace).
-behaviour(gen_server).

-export([ current/0
        , current/1

        , all/0
        , resolve/1
        , find/1
        , find_or_create/1
        , remove/1

        , name/1
        , intern/2
        , update_var/1
        , update_var/2
        , find_var/1
        , get_mappings/1

        , refer/3
        , add_alias/3
        , mapping/2
        , alias/2
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

-record(namespace, { id        :: binary(),
                     name      :: 'clojerl.Symbol':type(),
                     mappings  :: ets:tid(),
                     aliases   :: ets:tid()
                   }).

-type namespace() :: #namespace{}.

-spec current() -> namespace().
current() ->
  NsVar = 'clojerl.Var':new(<<"clojure.core">>, <<"*ns*">>),
  clj_core:deref(NsVar).

-spec current(namespace()) -> namespace().
current(#namespace{} = Ns) ->
  NsVar = 'clojerl.Var':new(<<"clojure.core">>, <<"*ns*">>),
  clj_core:'set!'(NsVar, Ns),
  Ns.

-spec all() -> namespace().
all() -> ets:tab2list(?MODULE).

%% @doc Tries to find a namespace by its name symbol or by its alias
%%      in the current namespace if it is there.
-spec resolve('clojerl.Symbol':type()) -> namespace().
resolve(SymNs) ->
  case find(SymNs) of
    undefined ->
      CurrentNs = clj_namespace:current(),
      case alias(CurrentNs, SymNs) of
        undefined -> undefined;
        AliasedNsSym -> find(AliasedNsSym)
      end;
    Ns -> Ns
  end.

%% @doc Tries to get the vars from the module associated to the
%%      namespace. If the module is not found or if it doesn't
%%      have a 'vars' attribute, then undefined is returned.
-spec find('clojerl.Symbol':type()) -> namespace() | undefined.
find(Name) ->
  case get(?MODULE, clj_core:str(Name)) of
    undefined -> gen_server:call(?MODULE, {load, Name});
    Ns -> Ns
  end.

-spec find_var('clojerl.Symbol':type()) ->
  'clojerl.Var':type() | undefined.
find_var(Symbol) ->
  NsStr = clj_core:namespace(Symbol),
  Ns = case NsStr of
         undefined -> clj_namespace:current();
         NsStr     ->
           NsSym = clj_core:symbol(NsStr),
           case clj_namespace:find(NsSym) of
             undefined -> clj_namespace:alias(clj_namespace:current(), NsSym);
             NsTemp    -> NsTemp
           end
       end,
  case Ns of
    undefined -> undefined;
    Ns ->
      NameSym = clj_core:symbol(clj_core:name(Symbol)),
      clj_namespace:mapping(Ns, NameSym)
  end.

-spec find_or_create('clojerl.Symbol':type()) -> namespace().
find_or_create(Name) ->
  Ns = case find(Name) of
         undefined -> gen_server:call(?MODULE, {new, Name});
         X -> X
       end,
  current(Ns).

-spec remove('clojerl.Symbol':type()) -> boolean().
remove(Name) ->
  gen_server:call(?MODULE, {remove, Name}).

-spec name(namespace()) -> 'clojerl.Symbol':type().
name(#namespace{name = Name}) -> Name.

-spec intern(namespace(), 'clojerl.Symbol':type()) -> namespace().
intern(Namespace = #namespace{name = NsName}, Symbol) ->
  clj_utils:throw_when( clj_core:namespace(Symbol) =/= undefined
                      , <<"Can't intern namespace-qualified symbol">>
                      ),

  SymName = clj_core:name(Symbol),
  Var     = 'clojerl.Var':new(clj_core:name(NsName), SymName),
  gen_server:call(?MODULE, {intern, Namespace, Symbol, Var}).

-spec update_var('clojerl.Var':type()) -> namespace().
update_var(Var) ->
  VarNsSym = clj_core:symbol(clj_core:namespace(Var)),
  update_var(find(VarNsSym), Var).

-spec update_var(namespace(), 'clojerl.Var':type()) -> namespace().
update_var(Namespace, Var) ->
  gen_server:call(?MODULE, {update_var, Namespace, Var}).

-spec get_mappings(namespace()) -> map().
get_mappings(#namespace{mappings = Mappings}) ->
  maps:from_list(ets:tab2list(Mappings)).

-spec refer(namespace(), 'clojerl.Symbol':type(), 'clojerl.Var':type()) ->
  namespace().
refer(Ns, Sym, Var) ->
  clj_utils:throw_when( not clj_core:'symbol?'(Sym)
                      , <<"Name for refer var is not a symbol">>
                      ),

  clj_utils:throw_when( clj_core:namespace(Sym) =/= undefined
                      , <<"Can't refer namespace-qualified symbol">>
                      ),

  gen_server:call(?MODULE, {intern, Ns, Sym, Var}).

-spec add_alias(namespace(), 'clojerl.Symbol':type(), namespace()) ->
  namespace().
add_alias(Ns, AliasSym, AliasedNs) ->
  clj_utils:throw_when( not clj_core:'symbol?'(AliasSym)
                      , <<"Name for refer var is not a symbol">>
                      ),

  gen_server:call(?MODULE, {add_alias, Ns, AliasSym, AliasedNs}).

-spec mapping(namespace(), 'clojerl.Symbol':type()) ->
  'clojerl.Var':type() | undefined.
mapping(#namespace{mappings = Mappings}, Symbol) ->
  case get(Mappings, clj_core:str(Symbol)) of
    {_, Var} -> Var;
    undefined -> undefined
  end.

-spec alias(namespace(), 'clojerl.Symbol':type()) ->
  'clojerl.Symbol':type() | undefined.
alias(#namespace{aliases = Aliases}, Symbol) ->
  case get(Aliases, clj_core:str(Symbol)) of
    {_, Var} -> Var;
    undefined -> undefined
  end.

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(?MODULE, [named_table, set, protected, {keypos, 2}]),
  {ok, undefined}.

handle_call({new, Name}, _From, State) ->
  {reply, new(Name), State};
handle_call({load, Name}, _Form, State) ->
  Ns = case load(Name) of
         undefined -> undefined;
         X -> save(?MODULE, X)
       end,
  {reply, Ns, State};
handle_call( {update_var, Ns = #namespace{mappings = Mappings}, Var}
           , _Form
           , State
           ) ->
  save(Mappings, {clj_core:name(Var), Var}),
  {reply, Ns, State};
handle_call( {intern, Ns = #namespace{mappings = Mappings}, Symbol, Var}
           , _From
           , State
           ) ->
  save(Mappings, {clj_core:name(Symbol), Var}),
  {reply, Ns, State};
handle_call( {add_alias, Ns = #namespace{aliases = Aliases}, Symbol, AliasedNs}
           , _From
           , State
           ) ->
  save(Aliases, {clj_core:name(Symbol), AliasedNs}),
  {reply, Ns, State};
handle_call({remove, Name}, _From, State) ->
  Result = ok =:= ets:delete(?MODULE, clj_core:name(Name)),
  {reply, Result, State}.

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


-spec new('clojerl.Symbol':type()) -> namespace().
new(NameSym) ->
  clj_utils:throw_when( not clj_core:'symbol?'(NameSym)
                      , <<"Namespace name must be a symbol">>
                      ),

  TableOpts = [set, protected, {keypos, 1}],
  Ns = #namespace{ id       = clj_core:name(NameSym)
                 , name     = NameSym
                 , mappings = ets:new(mappings, TableOpts)
                 , aliases  = ets:new(aliases, TableOpts)
                 },
  save(?MODULE, Ns).

-spec get(ets:tid(), term()) -> term().
get(Table, Id) ->
  case ets:lookup(Table, Id) of
    [] -> undefined;
    [Value] -> Value
  end.

-spec save(ets:tid(), term()) -> term().
save(Table, Value) ->
  true = ets:insert(Table, Value),
  Value.

-spec load('clojerl.Symbol':type()) -> namespace().
load(Name) ->
  NameStr = clj_core:name(Name),
  Module  = binary_to_atom(NameStr, utf8),

  _ = code:ensure_loaded(Module),

  Vars = case erlang:function_exported(Module, module_info, 1) of
           true ->
             Attrs = Module:module_info(attributes),
             case lists:keyfind(vars, 1, Attrs) of
               {vars, [VarsMap]} -> maps:values(VarsMap);
               false             -> undefined
             end;
           false -> undefined
         end,

  case Vars of
    undefined -> undefined;
    _ ->
      UpdateVarFun = fun(Var, Ns = #namespace{mappings = Mappings}) ->
                         save(Mappings, {clj_core:name(Var), Var}),
                         Ns
                     end,

      lists:foldl(UpdateVarFun, new(Name), Vars)
  end.
