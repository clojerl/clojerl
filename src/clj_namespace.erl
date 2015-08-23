-module(clj_namespace).

-export([
         new/1,
         name/1,
         intern/2,
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

-spec intern('clojerl.Symbol':type(), namespace()) -> namespace().
intern(Symbol, Namespace = #{name := NsName,
                             mappings := Mappings}) ->
  case 'clojerl.Symbol':namespace(Symbol) of
    undefined ->
      Var = 'clojerl.Var':new(NsName, Symbol),
      NewMappings = maps:put(Symbol, Var, Mappings),
      Namespace#{mappings => NewMappings};
    _ ->
      throw(<<"Can't intern namespace-qualified symbol">>)
  end.

-spec lookup('clojerl.Symbol':type(), namespace()) -> namespace().
lookup(Symbol, _Namespace = #{mappings := Mappings}) ->
  maps:get(Symbol, Mappings, undefined).
