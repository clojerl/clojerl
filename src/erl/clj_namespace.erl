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
        , find_mapping/2
        , get_mappings/1
        , get_aliases/1

        , refer/3
        , import_type/1
        , import_type/2
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
  clj_rt:deref(NsVar).

-spec current(namespace()) -> namespace().
current(#namespace{} = Ns) ->
  NsVar = 'clojerl.Var':?CONSTRUCTOR(<<"clojure.core">>, <<"*ns*">>),
  clj_rt:'set!'(NsVar, Ns),
  Ns.

-spec all() -> [namespace()].
all() -> ets:tab2list(?MODULE).

%% @doc Tries to get the mappings from the module associated to the
%%      namespace. If the module is not found or if it doesn't
%%      have a 'mappings' attribute, then nil is returned.
-spec find('clojerl.Symbol':type()) -> namespace() | ?NIL.
find(Name) ->
  case get(?MODULE, clj_rt:str(Name)) of
    ?NIL -> gen_server:call(?MODULE, {load, Name});
    Ns -> Ns
  end.

-spec find_var('clojerl.Symbol':type()) ->
  'clojerl.Var':type() | ?NIL.
find_var(Symbol) ->
  find_var(Symbol, current()).

-spec find_var('clojerl.Symbol':type(), namespace()) ->
  'clojerl.Var':type() | ?NIL.
find_var(Symbol, Ns) ->
  Var = find_mapping(Symbol, Ns),
  case clj_rt:'var?'(Var) of
    true  -> Var;
    false -> ?NIL
  end.

-spec find_mapping('clojerl.Symbol':type(), namespace()) ->
  'clojerl.Var':type() | 'clojerl.Symbol':type() | ?NIL.
find_mapping(Symbol, DefaultNs) ->
  case resolve_ns(Symbol, DefaultNs) of
    ?NIL -> ?NIL;
    Ns ->
      NameSym = clj_rt:symbol(clj_rt:name(Symbol)),
      mapping(NameSym, Ns)
  end.

-spec resolve_ns('clojerl.Symbol':type(), namespace()) ->
  namespace() | ?NIL.
resolve_ns(Symbol, DefaultNs) ->
  case clj_rt:namespace(Symbol) of
    ?NIL  -> DefaultNs;
    NsStr ->
      NsSym = clj_rt:symbol(NsStr),
      case find(NsSym) of
        ?NIL -> alias(NsSym, DefaultNs);
        Ns   -> Ns
      end
  end.

-spec find_or_create('clojerl.Symbol':type()) -> namespace().
find_or_create(Name) ->
  Ns = case find(Name) of
         ?NIL -> gen_server:call(?MODULE, {new, Name});
         X -> X
       end,
  current(Ns).

-spec remove('clojerl.Symbol':type()) -> boolean().
remove(Name) ->
  gen_server:call(?MODULE, {remove, Name}).

-spec name(namespace()) -> 'clojerl.Symbol':type().
name(#namespace{name = Name}) -> Name.

-spec intern('clojerl.Symbol':type(), namespace()) -> namespace().
intern(Symbol, Namespace = #namespace{name = NsName}) ->
  clj_utils:error_when( clj_rt:namespace(Symbol) =/= ?NIL
                      , <<"Can't intern namespace-qualified symbol">>
                      ),

  SymName = clj_rt:name(Symbol),
  Var     = 'clojerl.Var':?CONSTRUCTOR(clj_rt:name(NsName), SymName),
  gen_server:call(?MODULE, {intern, Namespace, Symbol, Var}).

-spec update_var('clojerl.Var':type()) -> namespace().
update_var(Var) ->
  VarNsSym = clj_rt:symbol(clj_rt:namespace(Var)),
  update_var(Var, find(VarNsSym)).

-spec update_var('clojerl.Var':type(), namespace()) -> namespace().
update_var(Var, Namespace) ->
  gen_server:call(?MODULE, {update_var, Namespace, Var}).

-spec get_mappings(namespace()) -> map().
get_mappings(#namespace{mappings = Mappings}) ->
  maps:from_list(ets:tab2list(Mappings)).

-spec get_aliases(namespace()) -> map().
get_aliases(#namespace{aliases = Aliases}) ->
  maps:from_list(ets:tab2list(Aliases)).

-spec refer('clojerl.Symbol':type(), 'clojerl.Var':type(), namespace()) ->
  namespace().
refer(Sym, Var, Ns) ->
  clj_utils:error_when( not clj_rt:'symbol?'(Sym)
                      , <<"Name for refer var is not a symbol">>
                      ),

  clj_utils:error_when( clj_rt:namespace(Sym) =/= ?NIL
                      , <<"Can't refer namespace-qualified symbol">>
                      ),

  gen_server:call(?MODULE, {intern, Ns, Sym, Var}).

-spec import_type(binary()) -> namespace().
import_type(TypeName) ->
  import_type(TypeName, true).

-spec import_type(binary(), boolean()) -> namespace().
import_type(TypeName, CheckLoaded) ->
  Type = binary_to_atom(TypeName, utf8),
  clj_utils:error_when( CheckLoaded
                        andalso {module, Type} =/= code:ensure_loaded(Type)
                      , [ <<"Type '">>, Type, <<"' could not be loaded. ">>]
                      ),

  SymName = lists:last(binary:split(TypeName, <<".">>, [global])),
  Sym     = clj_rt:symbol(SymName),
  TypeSym = clj_rt:symbol(TypeName),
  Ns      = current(),
  Exists  = mapping(Sym, Ns),

  clj_utils:error_when( Exists =/= ?NIL
                        andalso not clj_rt:equiv(Exists, TypeSym)
                      , [ Sym
                        , <<" already refers to: ">>
                        , Exists
                        , <<" in namespace: ">>
                        , name(Ns)
                        ]
                      ),

  gen_server:call(?MODULE, {intern, Ns, Sym, TypeSym}).

-spec unmap('clojerl.Symbol':type(), namespace()) -> namespace().
unmap(Sym, Ns) ->
  clj_utils:error_when( not clj_rt:'symbol?'(Sym)
                      , <<"Name for refer var is not a symbol">>
                      ),

  gen_server:call(?MODULE, {unmap, Ns, Sym}).

-spec add_alias('clojerl.Symbol':type(), namespace(), namespace()) ->
  namespace().
add_alias(AliasSym, AliasedNs, Ns) ->
  clj_utils:error_when( not clj_rt:'symbol?'(AliasSym)
                      , <<"Name for refer var is not a symbol">>
                      ),

  gen_server:call(?MODULE, {add_alias, Ns, AliasSym, AliasedNs}).

-spec remove_alias('clojerl.Symbol':type(), namespace()) ->
  namespace().
remove_alias(AliasSym, Ns) ->
  clj_utils:error_when( not clj_rt:'symbol?'(AliasSym)
                      , <<"Name for refer var is not a symbol">>
                      ),

  gen_server:call(?MODULE, {remove_alias, Ns, AliasSym}).

-spec mapping('clojerl.Symbol':type(), namespace()) ->
  'clojerl.Var':type() | ?NIL.
mapping(Symbol, #namespace{mappings = Mappings}) ->
  case get(Mappings, clj_rt:str(Symbol)) of
    {_, Var} -> Var;
    ?NIL -> ?NIL
  end.

-spec alias('clojerl.Symbol':type(), namespace()) ->
  'clojerl.Symbol':type() | ?NIL.
alias(Symbol, #namespace{aliases = Aliases}) ->
  case get(Aliases, clj_rt:str(Symbol)) of
    {_, Var} -> Var;
    ?NIL -> ?NIL
  end.

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(?MODULE, [named_table, set, protected, {keypos, 2}]),
  {ok, ?NIL}.

handle_call({new, Name}, _From, State) ->
  {reply, new(Name), State};
handle_call({load, Name}, _Form, State) ->
  Ns = case load(Name) of
         ?NIL -> ?NIL;
         X -> save(?MODULE, X)
       end,
  {reply, Ns, State};
handle_call( {update_var, Ns = #namespace{mappings = Mappings}, Var}
           , _Form
           , State
           ) ->
  save(Mappings, {clj_rt:name(Var), Var}),
  {reply, Ns, State};
handle_call( {intern, Ns = #namespace{mappings = Mappings}, Symbol, Var}
           , _From
           , State
           ) ->
  save(Mappings, {clj_rt:name(Symbol), Var}),
  {reply, Ns, State};
handle_call( {unmap, Ns = #namespace{mappings = Mappings}, Symbol}
           , _From
           , State
           ) ->
  true = delete(Mappings, clj_rt:name(Symbol)),
  {reply, Ns, State};
handle_call( {add_alias, Ns = #namespace{aliases = Aliases}, Symbol, AliasedNs}
           , _From
           , State
           ) ->
  save(Aliases, {clj_rt:name(Symbol), AliasedNs}),
  {reply, Ns, State};
handle_call( {remove_alias, Ns = #namespace{aliases = Aliases}, Symbol}
           , _From
           , State
           ) ->
  true = delete(Aliases, clj_rt:name(Symbol)),
  {reply, Ns, State};
handle_call({remove, Name}, _From, State) ->
  Result = true =:= ets:delete(?MODULE, clj_rt:name(Name)),
  {reply, Result, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Msg, State) ->
  {ok, State}.

code_change(_Msg, _From, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec new('clojerl.Symbol':type()) -> namespace().
new(NameSym) ->
  clj_utils:error_when( not clj_rt:'symbol?'(NameSym)
                      , <<"Namespace name must be a symbol">>
                      ),

  TableOpts = [set, protected, {keypos, 1}],
  Ns = #namespace{ id       = clj_rt:name(NameSym)
                 , name     = NameSym
                 , mappings = ets:new(mappings, TableOpts)
                 , aliases  = ets:new(aliases, TableOpts)
                 },
  save(?MODULE, Ns).

-spec get(atom() | ets:tid(), term()) -> term().
get(Table, Id) ->
  case ets:lookup(Table, Id) of
    [] -> ?NIL;
    [Value] -> Value
  end.

-spec save(ets:tid() | atom(), term()) -> term().
save(Table, Value) ->
  true = ets:insert(Table, Value),
  Value.

-spec delete(ets:tid(), term()) -> true.
delete(Table, Key) ->
  ets:delete(Table, Key).

-spec load('clojerl.Symbol':type()) -> namespace() | ?NIL.
load(Name) ->
  NameStr = clj_rt:name(Name),
  Module  = binary_to_atom(NameStr, utf8),

  Mappings = case code:ensure_loaded(Module) of
               {module, _} ->
                 Attrs = Module:module_info(attributes),
                 case lists:keyfind(mappings, 1, Attrs) of
                   {mappings, [Map]} -> Map;
                   false             -> ?NIL
                 end;
               {error, _} -> ?NIL
             end,

  case Mappings of
    ?NIL -> ?NIL;
    _ ->
      Ns = new(Name),
      SaveFun = fun(K, V) ->
                    save(Ns#namespace.mappings, {K, V})
                end,
      maps:map(SaveFun, Mappings),
      Ns
  end.
