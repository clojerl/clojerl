-module(clj_module).

-export([ load/1
        , to_forms/1

        , add_vars/2
        , add_attributes/2
        , add_exports/2
        , add_functions/2

        , is_clojure/1
        ]).

-type var_id() :: binary().

-type function_id() :: {atom(), integer()}.

-type clj_module() ::
        #{ module  => erl_syntax:syntaxTree()
         , vars    => #{var_id() => 'clojerl.Var':type()}
         , attrs   => [erl_syntax:syntaxTree()]
         , exports => [{atom(), non_neg_integer()}]
         , funs    => #{function_id() => erl_syntax:syntaxTree()}
         }.

-export_type([clj_module/0]).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------

-spec load(atom()) -> {ok, clj_module()} | {error, term()}.
load(Module) ->
  case code:ensure_loaded(Module) of
    {module, Module} ->
      from_binary(Module);
    {error, _} ->
      {ok, new([attribute_module(Module)])}
  end.

-spec attribute_module(atom()) -> erl_syntax:syntaxTree().
attribute_module(Name) when is_atom(Name) ->
  ModuleAtom = erl_syntax:atom(module),
  NameAtom = erl_syntax:atom(Name),
  erl_syntax:attribute(ModuleAtom, [NameAtom]).


-spec new([erl_syntax:syntaxTree()]) -> clj_module().
new(Forms) ->
  {[ModuleAttr], Rest} = lists:partition(is_attribute_fun(module), Forms),
  {AllAttrs, Rest1} = lists:partition(fun is_attribute/1, Rest),
  {Funs, Rest2} = lists:partition(fun is_function/1, Rest1),

  [ModuleAst] = erl_syntax:attribute_arguments(ModuleAttr),
  Module      = erl_syntax:concrete(ModuleAst),
  {VarsAttrs, RestAttrs} = lists:partition(is_attribute_fun(vars), AllAttrs),

  clj_utils:throw_when( length(VarsAttrs) > 1
                      , [ <<"The module ">>
                        , atom_to_binary(Module, utf8)
                        , <<" contains more than one 'vars' attributes.">>
                        ]
                      ),
  Vars = case VarsAttrs of
           [] -> #{};
           [VarsAttr] ->
             [V] = erl_syntax:attribute_arguments(VarsAttr),
             erl_syntax:concrete(V)
         end,

  {ExportAttrs, Attrs} = lists:partition(is_attribute_fun(export), RestAttrs),
  Exports = concrete_export(ExportAttrs),

  IndexedFuns = index_functions(Funs),

  #{ module  => ModuleAttr
   , vars    => Vars
   , attrs   => Attrs
   , exports => Exports
   , funs    => IndexedFuns
   , rest    => Rest2
   }.

-spec from_binary(atom()) -> clj_module().
from_binary(ModuleName) when is_atom(ModuleName) ->
  case code:get_object_code(ModuleName) of
    {ModuleName, Binary, _} ->
      case beam_lib:chunks(Binary, [abstract_code]) of
        {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
          {ok, new(Forms)};
        Error ->
          Error
      end;
    _ ->
      clj_utils:throw([ <<"Could not load object code for namespace: ">>
                      , atom_to_binary(ModuleName, utf8)
                      ]
                     )
  end.

-spec to_forms(clj_module()) -> [erl_syntax:syntaxTree()].
to_forms(#{module := Module} = Def) ->
  #{ attrs   := Attrs
   , exports := Exports
   , vars    := Vars
   , funs    := Funs
   , rest    := Rest
   } = Def,

  ClojureAtom = erl_syntax:atom(clojure),
  ClojureAttr = erl_syntax:attribute(ClojureAtom, [erl_syntax:atom(true)]),

  UniqueAttrs = lists:usort(erl_syntax:revert_forms([ClojureAttr | Attrs])),
  UniqueFuns  = maps:values(Funs),

  VarsAtom    = erl_syntax:atom(vars),
  VarsAttr    = erl_syntax:attribute(VarsAtom, [erl_syntax:abstract(Vars)]),

  ExportAttr = export_attribute(Exports),

  [Module, VarsAttr, ExportAttr | UniqueAttrs ++ Rest ++ UniqueFuns].

-spec add_vars(clj_module(), ['clojerl.Var':type()]) -> clj_module().
add_vars(#{vars := Vars} = Module, AddVars) ->
  AddByKeyFun = fun(V, Acc) ->
                    K = clj_core:name(V),
                    Acc#{K => V}
                end,
  Vars1 = lists:foldl(AddByKeyFun, Vars, AddVars),
  Module#{vars => Vars1}.

-spec add_attributes(clj_module(), [erl_syntax:syntaxTree()]) -> clj_module().
add_attributes(#{attrs := Attrs} = Module, AddAttrs) ->
  Module#{attrs => AddAttrs ++ Attrs}.

-spec add_exports(clj_module(), [{atom(), non_neg_integer()}]) ->
  clj_module().
add_exports(#{exports := Exports} = Module, AddExports) ->
  Module#{exports => AddExports ++ Exports}.

-spec add_functions(clj_module(), [erl_syntax:syntaxTree()]) -> clj_module().
add_functions(#{funs := Funs} = Module, AddFuns) ->
  AddByKeyFun = fun(F, Acc) ->
                    K = function_id(F),
                    Acc#{K => F}
                end,
  Funs1 = lists:foldl(AddByKeyFun, Funs, AddFuns),
  Module#{funs => Funs1}.

-spec is_clojure(module()) -> boolean().
is_clojure(Module) ->
  Attrs = Module:module_info(attributes),
  lists:keymember(clojure, 1, Attrs).

%%------------------------------------------------------------------------------
%% Helper Functions
%%------------------------------------------------------------------------------

-spec is_attribute(erl_syntax:syntaxTree()) -> boolean.
is_attribute(Form) ->
  erl_syntax:type(Form) =:= attribute.

-spec is_attribute_fun(atom()) -> function().
is_attribute_fun(Name) ->
  fun(Form) ->
      is_attribute(Form) andalso
        erl_syntax:concrete(erl_syntax:attribute_name(Form)) =:= Name
  end.

-spec is_function(erl_syntax:syntaxTree()) -> boolean.
is_function(Form) ->
  erl_syntax:type(Form) =:= function.

-spec function_name(erl_syntax:syntaxTree()) -> atom().
function_name(Function) ->
  erl_syntax:atom_value(erl_syntax:function_name(Function)).

-spec function_arity(erl_syntax:syntaxTree()) -> integer().
function_arity(Function) ->
  erl_syntax:function_arity(Function).

-spec function_id(erl_syntax:syntaxTree()) -> function_id().
function_id(Function) ->
  {function_name(Function), function_arity(Function)}.

-spec index_functions([erl_syntax:syntaxTree()]) ->
  #{atom() => erl_syntax:syntaxTree()}.
index_functions(Funs) ->
  IndexFun = fun(F, M) ->
                 Key = function_id(F),
                 M#{Key => F}
             end,
  lists:foldl(IndexFun, #{}, Funs).

-spec concrete_export(erl_syntax:syntaxTree()) ->
  [{atom(), non_neg_integer()}].
concrete_export(ExportAttrs) ->
  Exports  = lists:flatmap(fun erl_syntax:attribute_arguments/1, ExportAttrs),
  Exports1 = lists:flatmap(fun erl_syntax:concrete/1, Exports),
  lists:map(fun concrete_arity_qualifier/1, Exports1).

-spec concrete_arity_qualifier(erl_syntax:syntaxTree()) ->
  {atom(), non_neg_integer()}.
concrete_arity_qualifier({NameAst, ArityAst}) ->
  { erl_syntax:concrete(NameAst)
  , erl_syntax:concrete(ArityAst)
  }.

-spec export_attribute([{atom(), integer()}]) ->
  erl_syntax:syntaxTree().
export_attribute(FAs) when is_list(FAs) ->
  ExportAtom = erl_syntax:atom(export),
  Fun  = fun({Function, Arity}) -> arity_qualifier(Function, Arity) end,
  Asts = lists:usort(lists:map(Fun, FAs)),
  ListAst = erl_syntax:list(Asts),
  erl_syntax:attribute(ExportAtom, [ListAst]).

-spec arity_qualifier(atom(), integer()) -> erl_syntax:syntaxTree().
arity_qualifier(Function, Arity) when is_atom(Function),
                                      is_integer(Arity) ->
  FunctionAtomAst = erl_syntax:atom(Function),
  ArityIntegerAst = erl_syntax:integer(Arity),
  erl_syntax:arity_qualifier(FunctionAtomAst, ArityIntegerAst).
