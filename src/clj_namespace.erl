-module(clj_namespace).
-behaviour(gen_server).

-include("clojerl.hrl").

-export([ current/0
        , current/1

        , all/0
        , find/1
        , find_or_create/1
        , remove/1

        , name/1
        , intern/2
        , update_var/1
        , update_var/2
        , find_var/1
        , find_var/2
        , get_mappings/1
        , get_aliases/1

        , refer/3
        , import_type/1
        , unmap/2
        , add_alias/3
        , remove_alias/2
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
  NsVar = 'clojerl.Var':?CONSTRUCTOR(<<"clojure.core">>, <<"*ns*">>),
  clj_core:deref(NsVar).

-spec current(namespace()) -> namespace().
current(#namespace{} = Ns) ->
  NsVar = 'clojerl.Var':?CONSTRUCTOR(<<"clojure.core">>, <<"*ns*">>),
  clj_core:'set!'(NsVar, Ns),
  Ns.

-spec all() -> namespace().
all() -> ets:tab2list(?MODULE).

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
  find_var(current(), Symbol).

-spec find_var(namespace(), 'clojerl.Symbol':type()) ->
  'clojerl.Var':type() | undefined.
find_var(DefaultNs, Symbol) ->
  Ns = case clj_core:namespace(Symbol) of
         undefined -> DefaultNs;
         NsStr     ->
           NsSym = clj_core:symbol(NsStr),
           case find(NsSym) of
             undefined -> alias(current(), NsSym);
             NsTemp    -> NsTemp
           end
       end,
  case Ns of
    undefined -> undefined;
    Ns ->
      NameSym = clj_core:symbol(clj_core:name(Symbol)),
      mapping(Ns, NameSym)
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
  Var     = 'clojerl.Var':?CONSTRUCTOR(clj_core:name(NsName), SymName),
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

-spec get_aliases(namespace()) -> map().
get_aliases(#namespace{aliases = Aliases}) ->
  maps:from_list(ets:tab2list(Aliases)).

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

-spec import_type(binary()) -> namespace().
import_type(TypeName) ->
  SymName = lists:last(binary:split(TypeName, <<".">>, [global])),
  Sym     = clj_core:symbol(SymName),
  TypeSym = clj_core:symbol(TypeName),

  gen_server:call(?MODULE, {intern, current(), Sym, TypeSym}).

-spec unmap(namespace(), 'clojerl.Symbol':type()) -> namespace().
unmap(Ns, Sym) ->
  clj_utils:throw_when( not clj_core:'symbol?'(Sym)
                      , <<"Name for refer var is not a symbol">>
                      ),

  gen_server:call(?MODULE, {unmap, Ns, Sym}).

-spec add_alias(namespace(), 'clojerl.Symbol':type(), namespace()) ->
  namespace().
add_alias(Ns, AliasSym, AliasedNs) ->
  clj_utils:throw_when( not clj_core:'symbol?'(AliasSym)
                      , <<"Name for refer var is not a symbol">>
                      ),

  gen_server:call(?MODULE, {add_alias, Ns, AliasSym, AliasedNs}).

-spec remove_alias(namespace(), 'clojerl.Symbol':type()) ->
  namespace().
remove_alias(Ns, AliasSym) ->
  clj_utils:throw_when( not clj_core:'symbol?'(AliasSym)
                      , <<"Name for refer var is not a symbol">>
                      ),

  gen_server:call(?MODULE, {remove_alias, Ns, AliasSym}).

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
handle_call( {unmap, Ns = #namespace{mappings = Mappings}, Symbol}
           , _From
           , State
           ) ->
  ok = delete(Mappings, clj_core:name(Symbol)),
  {reply, Ns, State};
handle_call( {add_alias, Ns = #namespace{aliases = Aliases}, Symbol, AliasedNs}
           , _From
           , State
           ) ->
  save(Aliases, {clj_core:name(Symbol), AliasedNs}),
  {reply, Ns, State};
handle_call( {remove_alias, Ns = #namespace{aliases = Aliases}, Symbol}
           , _From
           , State
           ) ->
  delete(Aliases, clj_core:name(Symbol)),
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

-spec delete(ets:tid(), term()) -> ok | {error, term()}.
delete(Table, Key) ->
  ets:delete(Table, Key).

-spec load('clojerl.Symbol':type()) -> namespace().
load(Name) ->
  NameStr = clj_core:name(Name),
  Module  = binary_to_atom(NameStr, utf8),

  Vars = case code:ensure_loaded(Module) of
           {module, _} ->
             Attrs = Module:module_info(attributes),
             case lists:keyfind(vars, 1, Attrs) of
               {vars, [VarsMap]} -> maps:values(VarsMap);
               false             -> undefined
             end;
           {error, _} -> undefined
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
