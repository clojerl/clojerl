-module(clj_namespace).

-export([
         new/1,
         name/1,
         intern/2,
         update_var/2,
         lookup/2
        ]).

-type namespace() :: #{name => 'clojerl.Symbol':type(),
                       mappings => #{'clojerl.Symbol':type() => function()},
                       forms => []}.

-spec new('clojerl.Symbol':type()) -> namespace().
new(Name) ->
  #{name => Name,
    mappings => #{},
    forms => []}.

-spec name(namespace()) -> 'clojerl.Symbol':type().
name(_Ns = #{name := Name}) -> Name.

-spec intern(namespace(), 'clojerl.Symbol':type()) -> namespace().
intern(Namespace = #{name := NsName,
                     mappings := Mappings},
       Symbol) ->
  case 'clojerl.Symbol':namespace(Symbol) of
    undefined ->
      Var = 'clojerl.Var':new(NsName, Symbol),
      NewMappings = maps:put(Symbol, Var, Mappings),
      Namespace#{mappings => NewMappings};
    _ ->
      throw(<<"Can't intern namespace-qualified symbol">>)
  end.

-spec update_var(namespace(), 'clojerl.Var':type()) -> namespace().
update_var(Namespace = #{mappings := Mappings}, Var) ->
  VarNameSym = 'clojerl.Var':name(Var),
  NewMappings = maps:put(VarNameSym, Var, Mappings),
  Namespace#{mappings => NewMappings}.

-spec lookup(namespace(), 'clojerl.Symbol':type()) -> namespace().
lookup(_Namespace = #{mappings := Mappings}, Symbol) ->
  maps:get(Symbol, Mappings, undefined).
