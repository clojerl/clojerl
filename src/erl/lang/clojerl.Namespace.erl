-module('clojerl.Namespace').

-include("clojerl.hrl").

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

-type id()   :: binary().
-type data() :: #{ name     => 'clojerl.Symbol':type()
                 , mappings => ets:tid()
                 , aliases  => ets:tid()
                 }.
-type type() :: #?TYPE{data :: id(), info :: data()}.

-spec ?CONSTRUCTOR('clojerl.Symbol':type()) -> type().
?CONSTRUCTOR(Name) ->
  clj_utils:error_when( not clj_rt:'symbol?'(Name)
                      , <<"Namespace name must be a symbol">>
                      ),

  Opts = [set, protected, {keypos, 1}],
  Info = #{ name     => Name
          , mappings => ets:new(mappings, Opts)
          , aliases  => ets:new(aliases, Opts)
          },
  Ns   = #?TYPE{data = clj_rt:name(Name), info = Info},
  clj_utils:ets_save(?MODULE, Ns).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IHash

hash(Ns = #?TYPE{name = ?M}) ->
  erlang:phash2(Ns).

%% clojerl.IStringable

str(#?TYPE{name = ?M, info = #{name :=  Name}}) ->
  'clojerl.Symbol':name(Name).

%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------

-spec current() -> type().
current() ->
  NsVar = 'clojerl.Var':?CONSTRUCTOR(<<"clojure.core">>, <<"*ns*">>),
  clj_rt:deref(NsVar).

-spec current(type()) -> type().
current(#?TYPE{name = ?M} = Ns) ->
  NsVar = 'clojerl.Var':?CONSTRUCTOR(<<"clojure.core">>, <<"*ns*">>),
  clj_rt:'set!'(NsVar, Ns),
  Ns.

-spec all() -> [type()].
all() -> ets:tab2list(?MODULE).

%% @doc Tries to get the mappings from the module associated to the
%%      namespace. If the module is not found or if it doesn't
%%      have a 'mappings' attribute, then nil is returned.
-spec find('clojerl.Symbol':type()) -> type() | ?NIL.
find(Name) ->
  case clj_utils:ets_get(?MODULE, clj_rt:str(Name)) of
    ?NIL -> gen_server:call(?MODULE, {load, Name});
    Ns -> Ns
  end.

-spec find_var('clojerl.Symbol':type()) ->
  'clojerl.Var':type() | ?NIL.
find_var(Symbol) ->
  find_var(Symbol, current()).

-spec find_var('clojerl.Symbol':type(), type()) ->
  'clojerl.Var':type() | ?NIL.
find_var(Symbol, Ns) ->
  Var = find_mapping(Symbol, Ns),
  case clj_rt:'var?'(Var) of
    true  -> Var;
    false -> ?NIL
  end.

-spec find_mapping('clojerl.Symbol':type(), type()) ->
  'clojerl.Var':type() | 'clojerl.Symbol':type() | ?NIL.
find_mapping(Symbol, DefaultNs) ->
  case resolve_ns(Symbol, DefaultNs) of
    ?NIL -> ?NIL;
    Ns ->
      NameSym = clj_rt:symbol(clj_rt:name(Symbol)),
      mapping(NameSym, Ns)
  end.

-spec resolve_ns('clojerl.Symbol':type(), type()) ->
  type() | ?NIL.
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

-spec find_or_create('clojerl.Symbol':type()) -> type().
find_or_create(Name) ->
  Ns = case find(Name) of
         ?NIL -> gen_server:call(?MODULE, {new, Name});
         X -> X
       end,
  current(Ns).

-spec remove('clojerl.Symbol':type()) -> boolean().
remove(Name) ->
  gen_server:call(?MODULE, {remove, Name}).

-spec name(type()) -> 'clojerl.Symbol':type().
name(#?TYPE{name = ?M, info = #{name := Name}}) -> Name.

-spec intern('clojerl.Symbol':type(), type()) -> type().
intern(Symbol, Namespace = #?TYPE{name = ?M, info = #{name := NsName}}) ->
  clj_utils:error_when( clj_rt:namespace(Symbol) =/= ?NIL
                      , <<"Can't intern namespace-qualified symbol">>
                      ),

  SymName = clj_rt:name(Symbol),
  Var     = 'clojerl.Var':?CONSTRUCTOR(clj_rt:name(NsName), SymName),
  gen_server:call(?MODULE, {intern, Namespace, Symbol, Var}).

-spec update_var('clojerl.Var':type()) -> type().
update_var(Var) ->
  VarNsSym = clj_rt:symbol(clj_rt:namespace(Var)),
  update_var(Var, find(VarNsSym)).

-spec update_var('clojerl.Var':type(), type()) -> type().
update_var(Var, Namespace) ->
  gen_server:call(?MODULE, {update_var, Namespace, Var}).

-spec get_mappings(type()) -> map().
get_mappings(#?TYPE{name = ?M, info = #{mappings := Mappings}}) ->
  maps:from_list(ets:tab2list(Mappings)).

-spec get_aliases(type()) -> map().
get_aliases(#?TYPE{name = ?M, info = #{aliases := Aliases}}) ->
  maps:from_list(ets:tab2list(Aliases)).

-spec refer('clojerl.Symbol':type(), 'clojerl.Var':type(), type()) ->
  type().
refer(Sym, Var, Ns) ->
  clj_utils:error_when( not clj_rt:'symbol?'(Sym)
                      , <<"Name for refer var is not a symbol">>
                      ),

  clj_utils:error_when( clj_rt:namespace(Sym) =/= ?NIL
                      , <<"Can't refer namespace-qualified symbol">>
                      ),

  gen_server:call(?MODULE, {intern, Ns, Sym, Var}).

-spec import_type(binary()) -> type().
import_type(TypeName) ->
  import_type(TypeName, true).

-spec import_type(binary(), boolean()) -> type().
import_type(TypeName, CheckLoaded) ->
  Type = binary_to_atom(TypeName, utf8),
  clj_utils:error_when( CheckLoaded
                        andalso {module, Type} =/= code:ensure_loaded(Type)
                      , [ <<"Type ">>, TypeName, <<" could not be loaded. ">>]
                      ),

  SymName = lists:last(binary:split(TypeName, <<".">>, [global])),
  Sym     = clj_rt:symbol(SymName),
  TypeSym = clj_rt:symbol(TypeName),
  Ns      = current(),
  Exists  = mapping(Sym, Ns),

  clj_utils:warn_when( Exists =/= ?NIL
                       andalso not clj_rt:equiv(Exists, TypeSym)
                     , [ Sym
                       , <<" already refers to: ">>
                       , Exists
                       , <<" in namespace: ">>
                       , name(Ns)
                       ]
                     ),

  gen_server:call(?MODULE, {intern, Ns, Sym, TypeSym}).

-spec unmap('clojerl.Symbol':type(), type()) -> type().
unmap(Sym, Ns) ->
  clj_utils:error_when( not clj_rt:'symbol?'(Sym)
                      , <<"Name for refer var is not a symbol">>
                      ),

  gen_server:call(?MODULE, {unmap, Ns, Sym}).

-spec add_alias('clojerl.Symbol':type(), type(), type()) ->
  type().
add_alias(AliasSym, AliasedNs, Ns) ->
  clj_utils:error_when( not clj_rt:'symbol?'(AliasSym)
                      , <<"Name for refer var is not a symbol">>
                      ),

  gen_server:call(?MODULE, {add_alias, Ns, AliasSym, AliasedNs}).

-spec remove_alias('clojerl.Symbol':type(), type()) ->
  type().
remove_alias(AliasSym, Ns) ->
  clj_utils:error_when( not clj_rt:'symbol?'(AliasSym)
                      , <<"Name for refer var is not a symbol">>
                      ),

  gen_server:call(?MODULE, {remove_alias, Ns, AliasSym}).

-spec mapping('clojerl.Symbol':type(), type()) ->
  'clojerl.Var':type() | ?NIL.
mapping(Symbol, #?TYPE{name = ?M, info = #{mappings := Mappings}}) ->
  case clj_utils:ets_get(Mappings, clj_rt:str(Symbol)) of
    {_, Var} -> Var;
    ?NIL -> ?NIL
  end.

-spec alias('clojerl.Symbol':type(), type()) ->
  'clojerl.Symbol':type() | ?NIL.
alias(Symbol, #?TYPE{name = ?M, info = #{aliases := Aliases}}) ->
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
  ets:new(?MODULE, [named_table, set, protected, {keypos, 3}]),
  {ok, ?NIL}.

handle_call({new, Name}, _From, State) ->
  {reply, ?CONSTRUCTOR(Name), State};
handle_call({load, Name}, _Form, State) ->
  Ns = case load(Name) of
         ?NIL -> ?NIL;
         X -> clj_utils:ets_save(?MODULE, X)
       end,
  {reply, Ns, State};
handle_call( { update_var
             , Ns = #?TYPE{name = ?M, info = #{mappings := Mappings}}
             , Var
             }
           , _Form
           , State
           ) ->
  clj_utils:ets_save(Mappings, {clj_rt:name(Var), Var}),
  {reply, Ns, State};
handle_call( { intern
             , Ns = #?TYPE{name = ?M, info = #{mappings := Mappings}}
             , Symbol
             , Var
             }
           , _From
           , State
           ) ->
  clj_utils:ets_save(Mappings, {clj_rt:name(Symbol), Var}),
  {reply, Ns, State};
handle_call( { unmap
             , Ns = #?TYPE{name = ?M, info = #{mappings := Mappings}}
             , Symbol
             }
           , _From
           , State
           ) ->
  true = ets:delete(Mappings, clj_rt:name(Symbol)),
  {reply, Ns, State};
handle_call( { add_alias
             , Ns = #?TYPE{name = ?M, info = #{aliases := Aliases}}
             , Symbol
             , AliasedNs
             }
           , _From
           , State
           ) ->
  clj_utils:ets_save(Aliases, {clj_rt:name(Symbol), AliasedNs}),
  {reply, Ns, State};
handle_call( { remove_alias
             , Ns = #?TYPE{name = ?M, info = #{aliases := Aliases}}
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

-spec load('clojerl.Symbol':type()) -> type() | ?NIL.
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
      Ns = ?CONSTRUCTOR(Name),
      #?TYPE{info = #{mappings := MappingsId}} = Ns,

      SaveFun = fun(K, V) ->
                    clj_utils:ets_save(MappingsId, {K, V})
                end,
      maps:map(SaveFun, Mappings),
      Ns
  end.
