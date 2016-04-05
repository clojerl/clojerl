-module(clj_namespace).

-export([ new/1
        , load/1
        , load_or_create/1

        , name/1
        , intern/2
        , update_var/2
        , get_mappings/1

        , refer/3
        , mapping/2
        , alias/2
        ]).

-type namespace() ::
        #{ name     => 'clojerl.Symbol':type()
         , mappings => #{binary() => 'clojerl.Var':type()}
         , aliases  => #{binary() => 'clojerl.Symbol':type()}
         }.

-spec new('clojerl.Symbol':type()) -> namespace().
new(NameSym) ->
  clj_utils:throw_when( not clj_core:'symbol?'(NameSym)
                      , <<"Namespace name must be a symbol">>
                      ),
  #{ name     => NameSym
   , mappings => #{}
   , aliases  => #{}
   }.

%% @doc Tries to get the vars from the module associated to the
%%      namespace. If the module is not found or if it doesn't
%%      have a 'vars' attribute, then undefined is returned.
-spec load('clojerl.Symbol':type()) -> namespace() | undefined.
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
    undefined ->
      undefined;
    _ ->
      NewNs = clj_namespace:new(Name),
      lists:foldl(fun(Var, Ns) -> update_var(Ns, Var) end, NewNs, Vars)
  end.

-spec load_or_create('clojerl.Symbol':type()) -> namespace().
load_or_create(Name) ->
  case load(Name) of
    undefined -> clj_namespace:new(Name);
    Ns        -> Ns
  end.

-spec name(namespace()) -> 'clojerl.Symbol':type().
name(_Ns = #{name := Name}) -> Name.

-spec intern(namespace(), 'clojerl.Symbol':type()) -> namespace().
intern( Namespace = #{ name     := NsName
                     , mappings := Mappings
                     }
      , Symbol
      ) ->
  clj_utils:throw_when( clj_core:namespace(Symbol) =/= undefined
                      , <<"Can't intern namespace-qualified symbol">>
                      ),

  Var = 'clojerl.Var':new(clj_core:name(NsName), clj_core:name(Symbol)),
  SymbolBin = clj_core:name(Symbol),
  NewMappings = maps:put(SymbolBin, Var, Mappings),
  Namespace#{mappings => NewMappings}.

-spec update_var(namespace(), 'clojerl.Var':type()) -> namespace().
update_var(Namespace = #{mappings := Mappings}, Var) ->
  VarName     = clj_core:name(Var),
  NewMappings = maps:put(VarName, Var, Mappings),
  Namespace#{mappings => NewMappings}.

-spec get_mappings(namespace()) -> map().
get_mappings(#{mappings := Mappings}) -> Mappings.

-spec refer(namespace(), 'clojerl.Symbol':type(), 'clojerl.Var':type()) ->
  namespace().
refer(#{mappings := Mappings} = Namespace, Sym, Var) ->
  clj_utils:throw_when( not clj_core:'symbol?'(Sym)
                      , <<"Name for refer var is not a symbol">>
                      ),

  clj_utils:throw_when( clj_core:namespace(Sym) =/= undefined
                      , <<"Can't refer namespace-qualified symbol">>
                      ),

  Name = clj_core:name(Sym),
  Namespace#{mappings := Mappings#{Name => Var}}.

-spec mapping(namespace(), 'clojerl.Symbol':type()) ->
  'clojerl.Var':type() | undefined.
mapping(_Namespace = #{mappings := Mappings}, Symbol) ->
  maps:get(clj_core:name(Symbol), Mappings, undefined).

-spec alias(namespace(), 'clojerl.Symbol':type()) ->
  'clojerl.Symbol':type() | undefined.
alias(_Namespace = #{aliases := Aliases}, Symbol) ->
  maps:get(clj_core:name(Symbol), Aliases, undefined).
