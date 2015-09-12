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

-type namespace() :: #{name => 'clojerl.Symbol':type(),
                       defs => #{'clojerl.Symbol':type() => 'clojerl.Var':type()},
                       uses => #{'clojerl.Symbol':type() => 'clojerl.Var':type()},
                       aliases => #{'clojerl.Symbol':type() => 'clojerl.Symbol':type()},
                       forms => []}.

-spec new('clojerl.Symbol':type()) -> namespace().
new(Name) ->
  #{name => Name,
    defs => #{},
    uses => #{},
    aliases => #{},
    forms => []}.

-spec name(namespace()) -> 'clojerl.Symbol':type().
name(_Ns = #{name := Name}) -> Name.

-spec intern(namespace(), 'clojerl.Symbol':type()) -> namespace().
intern(Namespace = #{name := NsName,
                     defs := Defs},
       Symbol) ->
  case 'clojerl.Symbol':namespace(Symbol) of
    undefined ->
      Var = 'clojerl.Var':new(NsName, Symbol),
      NewDefs = maps:put(Symbol, Var, Defs),
      Namespace#{defs => NewDefs};
    _ ->
      throw(<<"Can't intern namespace-qualified symbol">>)
  end.

-spec update_var(namespace(), 'clojerl.Var':type()) -> namespace().
update_var(Namespace = #{defs := Defs}, Var) ->
  VarNameSym = 'clojerl.Var':name(Var),
  NewDefs = maps:put(VarNameSym, Var, Defs),
  Namespace#{defs => NewDefs}.

-spec def(namespace(), 'clojerl.Symbol':type()) -> 'clojerl.Var':type() | undefined.
def(_Namespace = #{defs := Defs}, Symbol) ->
  maps:get(Symbol, Defs, undefined).

-spec use(namespace(), 'clojerl.Symbol':type()) -> 'clojerl.Var':type() | undefined.
use(_Namespace = #{uses := Uses}, Symbol) ->
  maps:get(Symbol, Uses, undefined).

-spec alias(namespace(), 'clojerl.Symbol':type()) -> 'clojerl.Symbol':type() | undefined.
alias(_Namespace = #{aliases := Aliases}, Symbol) ->
  maps:get(Symbol, Aliases, undefined).
