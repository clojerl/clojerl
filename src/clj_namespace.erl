-module(clj_namespace).

-export([ new/1
        , load/1
        , load_or_create/1

        , name/1
        , intern/2
        , update_var/2
        , def/2
        , use/2
        , alias/2
        ]).

-type namespace() ::
        #{ name    => 'clojerl.Symbol':type()
         , defs    => #{binary() => 'clojerl.Var':type()}
         , uses    => #{binary() => 'clojerl.Var':type()}
         , aliases => #{binary() => 'clojerl.Symbol':type()}
         }.

-spec new('clojerl.Symbol':type()) -> namespace().
new(Name) ->
  #{ name    => Name
   , defs    => #{}
   , uses    => #{}
   , aliases => #{}
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
intern( Namespace = #{ name := NsName
                     , defs := Defs
                     }
      , Symbol
      ) ->
  case clj_core:namespace(Symbol) of
    undefined ->
      Var = 'clojerl.Var':new(clj_core:name(NsName), clj_core:name(Symbol)),
      SymbolBin = clj_core:str(Symbol),
      NewDefs = maps:put(SymbolBin, Var, Defs),
      Namespace#{defs => NewDefs};
    _ ->
      throw(<<"Can't intern namespace-qualified symbol">>)
  end.

-spec update_var(namespace(), 'clojerl.Var':type()) -> namespace().
update_var(Namespace = #{defs := Defs}, Var) ->
  VarName = clj_core:name(Var),
  NewDefs = maps:put(VarName, Var, Defs),
  Namespace#{defs => NewDefs}.

-spec def(namespace(), 'clojerl.Symbol':type()) ->
  'clojerl.Var':type() | undefined.
def(_Namespace = #{defs := Defs}, Symbol) ->
  maps:get(clj_core:str(Symbol), Defs, undefined).

-spec use(namespace(), 'clojerl.Symbol':type()) ->
  'clojerl.Var':type() | undefined.
use(_Namespace = #{uses := Uses}, Symbol) ->
  maps:get(clj_core:str(Symbol), Uses, undefined).

-spec alias(namespace(), 'clojerl.Symbol':type()) ->
  'clojerl.Symbol':type() | undefined.
alias(_Namespace = #{aliases := Aliases}, Symbol) ->
  maps:get(clj_core:str(Symbol), Aliases, undefined).
