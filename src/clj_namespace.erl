-module(clj_namespace).

-export([
         new/1,
         name/1,
         intern/2,
         update_var/2,
         def/2,
         use/2,
         alias/2
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
