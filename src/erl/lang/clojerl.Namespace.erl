-module('clojerl.Namespace').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behaviour(gen_server).
-behavior('clojerl.IHash').
-behavior('clojerl.IStringable').

-export([hash/1]).
-export([str/1]).

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

-type type() :: #{ ?TYPE    => ?M
                 , id       => any()
                 , name     => 'clojerl.Symbol':type()
                 , mappings => ets:tid()
                 , aliases  => ets:tid()
                 }.

-spec ?CONSTRUCTOR('clojerl.Symbol':type()) -> type().
?CONSTRUCTOR(Name) ->
  ?ERROR_WHEN( not clj_rt:'symbol?'(Name)
             , <<"Namespace name must be a symbol">>
             ),

  Opts = [set, protected, {keypos, 1}],
  Id   = clj_rt:name(Name),
  Ns   = #{ ?TYPE    => ?M
          , id       => Id
          , name     => Name
          , mappings => ets:new(mappings, Opts)
          , aliases  => ets:new(aliases, Opts)
          },
  clj_utils:ets_save(?MODULE, {Id, Ns}),
  Ns.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IHash

hash(Ns = #{?TYPE := ?M}) ->
  erlang:phash2(Ns).

%% clojerl.IStringable

str(#{?TYPE := ?M, name := Name}) ->
  'clojerl.Symbol':name(Name).

%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------

-spec current() -> type().
current() ->
  NsVar = 'clojerl.Var':?CONSTRUCTOR(<<"clojure.core">>, <<"*ns*">>),
  clj_rt:deref(NsVar).

-spec current(type()) -> type().
current(#{?TYPE := ?M} = Ns) ->
  NsVar = 'clojerl.Var':?CONSTRUCTOR(<<"clojure.core">>, <<"*ns*">>),
  clj_rt:'set!'(NsVar, Ns),
  Ns.

-spec all() -> [type()].
all() -> [Ns || {_, Ns} <- ets:tab2list(?MODULE)].

%% @doc Finds the ns for the provided symbol, if it's not loaded
%%      it tries to load it from a compiled module.
-spec find('clojerl.Symbol':type()) -> type() | ?NIL.
find(Name) ->
  case clj_utils:ets_get(?MODULE, clj_rt:str(Name)) of
    ?NIL -> gen_server:call(?MODULE, {load, Name});
    {_, Ns} -> Ns
  end.

-spec find_var('clojerl.Symbol':type()) ->
  'clojerl.Var':type() | ?NIL.
find_var(Symbol) ->
  find_var(current(), Symbol).

-spec find_var(type(), 'clojerl.Symbol':type()) ->
  'clojerl.Var':type() | ?NIL.
find_var(#{?TYPE := ?M} = Ns, Symbol) ->
  Var = find_mapping(Ns, Symbol),
  case clj_rt:'var?'(Var) of
    true  -> Var;
    false -> ?NIL
  end.

-spec find_mapping(type(), 'clojerl.Symbol':type()) ->
  'clojerl.Var':type() | 'clojerl.Symbol':type() | ?NIL.
find_mapping(#{?TYPE := ?M} = DefaultNs, Symbol) ->
  case resolve_ns(DefaultNs, Symbol) of
    ?NIL -> ?NIL;
    Ns ->
      NameSym = clj_rt:symbol(clj_rt:name(Symbol)),
      mapping(Ns, NameSym)
  end.

-spec resolve_ns(type(), 'clojerl.Symbol':type()) ->
  type() | ?NIL.
resolve_ns(#{?TYPE := ?M} = DefaultNs, Symbol) ->
  ?ERROR_WHEN(not clj_rt:'symbol?'(Symbol), <<"Argument must be a symbol">>),

  case clj_rt:namespace(Symbol) of
    ?NIL  -> DefaultNs;
    NsStr ->
      NsSym = clj_rt:symbol(NsStr),
      case find(NsSym) of
        ?NIL -> alias(DefaultNs, NsSym);
        Ns   -> Ns
      end
  end.

-spec find_or_create('clojerl.Symbol':type()) -> type().
find_or_create(Name) ->
  ?ERROR_WHEN(not clj_rt:'symbol?'(Name), <<"Argument must be a symbol">>),
  Ns = case find(Name) of
         ?NIL -> gen_server:call(?MODULE, {new, Name});
         X -> X
       end,
  current(Ns).

-spec remove('clojerl.Symbol':type()) -> boolean().
remove(Name) ->
  ?ERROR_WHEN(not clj_rt:'symbol?'(Name), <<"Argument must be a symbol">>),
  gen_server:call(?MODULE, {remove, Name}).

-spec name(type()) -> 'clojerl.Symbol':type().
name(#{?TYPE := ?M, name := Name}) -> Name.

-spec intern(type(), 'clojerl.Symbol':type()) -> type().
intern(#{?TYPE := ?M, name := NsName} = Ns, Symbol) ->
  ?ERROR_WHEN(not clj_rt:'symbol?'(Symbol), <<"Argument must be a symbol">>),

  ?ERROR_WHEN( clj_rt:namespace(Symbol) =/= ?NIL
             , <<"Can't intern namespace-qualified symbol">>
             ),


  SymName = clj_rt:name(Symbol),
  Var     = 'clojerl.Var':?CONSTRUCTOR(clj_rt:name(NsName), SymName),

  check_if_override(Ns, Symbol, mapping(Ns, Symbol), Var),

  gen_server:call(?MODULE, {intern, Ns, Symbol, Var}).

-spec update_var('clojerl.Var':type()) -> type().
update_var(Var) ->
  VarNsSym = clj_rt:symbol(clj_rt:namespace(Var)),
  update_var(find(VarNsSym), Var).

-spec update_var(type(), 'clojerl.Var':type()) -> type().
update_var(#{?TYPE := ?M} = Ns, Var) ->
  ?ERROR_WHEN( not clj_rt:'var?'(Var)
             , <<"Argument must be a var">>
             ),

  gen_server:call(?MODULE, {update_var, Ns, Var}).

-spec get_mappings(type()) -> map().
get_mappings(#{?TYPE := ?M, mappings := Mappings}) ->
  maps:from_list(ets:tab2list(Mappings)).

-spec get_aliases(type()) -> map().
get_aliases(#{?TYPE := ?M, aliases := Aliases}) ->
  maps:from_list(ets:tab2list(Aliases)).

-spec refer(type(), 'clojerl.Symbol':type(), 'clojerl.Var':type()) ->
  type().
refer(#{?TYPE := ?M} = Ns, Sym, Var) ->
  ?ERROR_WHEN( not clj_rt:'symbol?'(Sym)
             , <<"Name for refer var is not a symbol">>
             ),

  ?ERROR_WHEN( clj_rt:namespace(Sym) =/= ?NIL
             , <<"Can't refer namespace-qualified symbol">>
             ),

  check_if_override(Ns, Sym, mapping(Ns, Sym), Var),

  gen_server:call(?MODULE, {intern, Ns, Sym, Var}).

-spec import_type(binary()) -> type().
import_type(TypeName) ->
  import_type(TypeName, true).

-spec import_type(binary(), boolean()) -> type().
import_type(TypeName, CheckLoaded) ->
  Module = binary_to_atom(TypeName, utf8),
  ?ERROR_WHEN( CheckLoaded
               andalso {module, Module} =/= code:ensure_loaded(Module)
             , [ <<"Type ">>, TypeName, <<" could not be loaded. ">>]
             ),

  SymName = lists:last(binary:split(TypeName, <<".">>, [global])),
  Sym     = clj_rt:symbol(SymName),
  Type    = 'erlang.Type':?CONSTRUCTOR(Module),
  Ns      = current(),
  Exists  = mapping(Ns, Sym),

  ?WARN_WHEN( Exists =/= ?NIL
              andalso not clj_rt:equiv(Exists, Type)
            , [ Sym , <<" already refers to: ">> , Exists
              , <<" in namespace: ">> , name(Ns)
              ]
            ),

  gen_server:call(?MODULE, {intern, Ns, Sym, Type}).

-spec unmap(type(), 'clojerl.Symbol':type()) -> type().
unmap(#{?TYPE := ?M} = Ns, Sym) ->
  ?ERROR_WHEN( not clj_rt:'symbol?'(Sym)
             , <<"Name for refer var is not a symbol">>
             ),

  gen_server:call(?MODULE, {unmap, Ns, Sym}).

-spec add_alias(type(), 'clojerl.Symbol':type(), type()) ->
  type().
add_alias( #{?TYPE := ?M, name := NsName} = Ns
         , AliasSym
         , #{?TYPE := ?M} = AliasedNs
         ) ->
  ?ERROR_WHEN( not clj_rt:'symbol?'(AliasSym)
             , <<"Name for refer var is not a symbol">>
             ),

  case clj_module:in_context() of
    false -> ok;
    true  ->
      NsNameAtom = clj_rt:keyword(NsName),
      clj_module:add_alias(AliasSym, name(AliasedNs), NsNameAtom)
  end,

  gen_server:call(?MODULE, {add_alias, Ns, AliasSym, AliasedNs}).

-spec remove_alias(type(), 'clojerl.Symbol':type()) ->
  type().
remove_alias(#{?TYPE := ?M} = Ns, AliasSym) ->
  ?ERROR_WHEN( not clj_rt:'symbol?'(AliasSym)
             , <<"Name for refer var is not a symbol">>
             ),

  gen_server:call(?MODULE, {remove_alias, Ns, AliasSym}).

-spec mapping(type(), 'clojerl.Symbol':type()) ->
  'clojerl.Var':type() | ?NIL.
mapping(#{?TYPE := ?M, mappings := Mappings}, Symbol) ->
  ?ERROR_WHEN( not clj_rt:'symbol?'(Symbol)
             , <<"Argument must be a symbol">>
             ),

  case clj_utils:ets_get(Mappings, clj_rt:str(Symbol)) of
    {_, Var} -> Var;
    ?NIL -> ?NIL
  end.

-spec alias(type(), 'clojerl.Symbol':type()) ->
  'clojerl.Symbol':type() | ?NIL.
alias(#{?TYPE := ?M, aliases := Aliases}, Symbol) ->
  ?ERROR_WHEN( not clj_rt:'symbol?'(Symbol)
             , <<"Argument must be a symbol">>
             ),

  case clj_utils:ets_get(Aliases, clj_rt:str(Symbol)) of
    {_, Var} -> Var;
    ?NIL -> ?NIL
  end.

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(?MODULE, [named_table, set, protected, {keypos, 1}]),
  {ok, ?NIL}.

handle_call({new, Name}, _From, State) ->
  {reply, ?CONSTRUCTOR(Name), State};
handle_call({load, Name}, _Form, State) ->
  Ns = case load(Name) of
         ?NIL ->
           ?NIL;
         #{id := Id} = X ->
           clj_utils:ets_save(?MODULE, {Id, X}),
           X
       end,
  {reply, Ns, State};
handle_call( { update_var
             , Ns = #{?TYPE := ?M, mappings := Mappings}
             , Var
             }
           , _Form
           , State
           ) ->
  clj_utils:ets_save(Mappings, {clj_rt:name(Var), Var}),
  {reply, Ns, State};
handle_call( { intern
             , Ns = #{?TYPE := ?M, mappings := Mappings}
             , Symbol
             , Var
             }
           , _From
           , State
           ) ->
  clj_utils:ets_save(Mappings, {clj_rt:name(Symbol), Var}),
  {reply, Ns, State};
handle_call( { unmap
             , Ns = #{?TYPE := ?M, mappings := Mappings}
             , Symbol
             }
           , _From
           , State
           ) ->
  true = ets:delete(Mappings, clj_rt:name(Symbol)),
  {reply, Ns, State};
handle_call( { add_alias
             , Ns = #{?TYPE := ?M, aliases := Aliases}
             , Symbol
             , AliasedNs
             }
           , _From
           , State
           ) ->
  clj_utils:ets_save(Aliases, {clj_rt:name(Symbol), AliasedNs}),
  {reply, Ns, State};
handle_call( { remove_alias
             , Ns = #{?TYPE := ?M, aliases := Aliases}
             , Symbol
             }
           , _From
           , State
           ) ->
  true = ets:delete(Aliases, clj_rt:name(Symbol)),
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

-spec check_if_override( type()
                       , 'clojerl.Symbol':type()
                       , ?NIL | 'clojerl.Var':type()
                       , 'clojerl.Var':type()
                       ) ->
  ok.
check_if_override(_, _, ?NIL, _) ->
  ok;
check_if_override(Ns, Sym, Old, New) ->
  NsName   = 'clojerl.Symbol':name(name(Ns)),
  OldVarNs = 'clojerl.Var':namespace(Old),
  NewVarNs = 'clojerl.Var':namespace(New),

  Message  = [Sym, <<" already refers to: ">>, Old, <<" in namespace: ">>, Ns],
  Warn     = OldVarNs =/= NsName andalso NewVarNs =/= <<"clojure.core">>,

  ?ERROR_WHEN(Warn andalso OldVarNs =/= <<"clojure.core">>, Message),

  ?WARN_WHEN(Warn, [<<"WARNING: ">>, Message]).

-spec load('clojerl.Symbol':type()) -> type() | ?NIL.
load(Name) ->
  Module = clj_rt:keyword(Name),
  case code:ensure_loaded(Module) of
    {module, _} ->
      maybe_load_ns(Name, Module);
    {error, _} -> ?NIL
  end.

-spec maybe_load_ns('clojerl.Symbol':type(), module()) -> type() | ?NIL.
maybe_load_ns(Name, Module) ->
  case clj_module:is_clojure(Module) of
    true -> load_ns(Name, Module);
    false -> ?NIL
  end.

-spec load_ns('clojerl.Symbol':type(), module()) -> type().
load_ns(Name, Module) ->
  Attrs    = Module:module_info(attributes),
  Mappings = fetch_attribute(mappings, Attrs),
  Aliases  = fetch_attribute(aliases, Attrs),
  Ns = ?CONSTRUCTOR(Name),
  #{ ?TYPE    := ?M
   , mappings := MappingsId
   , aliases  := AliasesId
   } = Ns,

  MappingsFun = fun(K, V) -> clj_utils:ets_save(MappingsId, {K, V}) end,
  maps:map(MappingsFun, Mappings),

  AliasesFun = fun(K, V) ->
                   LoadedNs = load(V),
                   clj_utils:ets_save(AliasesId, {K, LoadedNs})
               end,
  maps:map(AliasesFun, Aliases),
  Ns.

-spec fetch_attribute(atom(), [any()]) -> map() | ?NIL.
fetch_attribute(Name, Attributes) ->
  case lists:keyfind(Name, 1, Attributes) of
    {Name, [Map]} -> Map;
    false         -> #{}
  end.
