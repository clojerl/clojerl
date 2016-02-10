-module(clj_module).

-export([ load/1
        , to_forms/1

        , add_vars/2
        , add_attributes/2
        , add_functions/2
        ]).

-type clj_module() :: #{ module => erl_syntax:syntaxTree()
                       , attrs  => [erl_syntax:syntaxTree()]
                       , funs   => [erl_syntax:syntaxTree()]
                       }.

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
  {[ModuleAttr], Rest} = lists:partition(fun is_module_attribute/1, Forms),
  {Attrs, Rest1} = lists:partition(fun is_attribute/1, Rest),
  {Funs, Rest2} = lists:partition(fun is_function/1, Rest1),

  [ModuleAst] = erl_syntax:attribute_arguments(ModuleAttr),
  Module = erl_syntax:concrete(ModuleAst),
  {VarsAttrs, RestAttrs} = lists:partition(fun is_vars_attribute/1, Attrs),
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

  IndexedFuns = index_functions(Funs),

  #{ module => ModuleAttr
   , vars   => Vars
   , attrs  => RestAttrs
   , funs   => IndexedFuns
   , rest   => Rest2
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
  #{ attrs := Attrs
   , vars := Vars
   , funs  := Funs
   , rest  := Rest
   } = Def,

  %% TODO: This won't work if the new function or attribute differs
  %%       from the previous one. We need to be able to keep more
  %%       detailed information about functions and attributes, to
  %%       avoid duplicates.
  UniqueAttrs = lists:usort(erl_syntax:revert_forms(Attrs)),
  UniqueFuns  = lists:usort(maps:values(Funs)),
  VarsAtom = erl_syntax:atom(vars),
  VarsAttr = erl_syntax:attribute(VarsAtom, [erl_syntax:abstract(Vars)]),

  [Module, VarsAttr | UniqueAttrs ++ Rest ++ UniqueFuns].

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

-spec add_functions(clj_module(), [erl_syntax:syntaxTree()]) -> clj_module().
add_functions(#{funs := Funs} = Module, AddFuns) ->
  AddByKeyFun = fun(F, Acc) ->
                    K = function_key(F),
                    Acc#{K => F}
                end,
  Funs1 = lists:foldl(AddByKeyFun, Funs, AddFuns),
  Module#{funs => Funs1}.

%%------------------------------------------------------------------------------
%% Helper Functions
%%------------------------------------------------------------------------------

-spec is_module_attribute(erl_syntax:syntaxTree()) -> boolean.
is_module_attribute(Form) ->
  erl_syntax:type(Form) =:= attribute
    andalso
    erl_syntax:concrete(erl_syntax:attribute_name(Form)) =:= module.

-spec is_attribute(erl_syntax:syntaxTree()) -> boolean.
is_attribute(Form) ->
  erl_syntax:type(Form) =:= attribute.

-spec is_vars_attribute(erl_syntax:syntaxTree()) -> boolean.
is_vars_attribute(Form) ->
  erl_syntax:type(Form) =:= attribute
    andalso
    erl_syntax:concrete(erl_syntax:attribute_name(Form)) =:= vars.

-spec is_function(erl_syntax:syntaxTree()) -> boolean.
is_function(Form) ->
  erl_syntax:type(Form) =:= function.

-spec function_name(erl_syntax:syntaxTree()) -> atom().
function_name(Function) ->
  erl_syntax:atom_value(erl_syntax:function_name(Function)).

-spec function_arity(erl_syntax:syntaxTree()) -> integer().
function_arity(Function) ->
  erl_syntax:function_arity(Function).

-spec function_key(erl_syntax:syntaxTree()) -> {atom(), integer()}.
function_key(Function) ->
  {function_name(Function), function_arity(Function)}.

-spec index_functions([erl_syntax:syntaxTree()]) ->
  #{atom() => erl_syntax:syntaxTree()}.
index_functions(Funs) ->
  IndexFun = fun(F, M) ->
                 Key = function_key(F),
                 M#{Key => F}
             end,
  lists:foldl(IndexFun, #{}, Funs).
