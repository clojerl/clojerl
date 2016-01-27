-module(clj_module).

-export([ new/1
        , from_binary/1
        , to_forms/1

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

-spec new([erl_syntax:syntaxTree()]) -> clj_module().
new(Forms) ->
  {[Module], AttrsFuns} = lists:partition(fun is_module_attribute/1, Forms),
  {Attrs, Funs} = lists:partition(fun is_attribute/1, AttrsFuns),

  #{ module => Module
   , attrs  => Attrs
   , funs   => Funs
   }.

-spec from_binary(atom()) -> clj_module().
from_binary(ModuleName) when is_atom(ModuleName) ->
  {ModuleName, Binary, _} = code:get_object_code(ModuleName),
  case beam_lib:chunks(Binary, [abstract_code]) of
    {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
      {ok, new(Forms)};
    Error ->
      Error
  end.

-spec to_forms(clj_module()) -> [erl_syntax:syntaxTree()].
to_forms(#{module := Module} = Def) ->
  Attrs = maps:get(attrs, Def, []),
  Funs  = maps:get(funs, Def, []),

  %% TODO: This won't work if the new function or attribute differs
  %%       from the previous one. We need to be able to keep more
  %%       detailed informationa about functions and attributes, to
  %%       avoid duplicates.
  UniqueAttrs = lists:usort(erl_syntax:revert_forms(Attrs)),
  UniqueFuns  = lists:usort(erl_syntax:revert_forms(Funs)),

  [Module | UniqueAttrs ++ UniqueFuns].

-spec add_attributes(clj_module(), [erl_syntax:syntaxTree()]) -> clj_module().
add_attributes(#{attrs := Attrs} = Module, AddAttrs) ->
  Module#{attrs => AddAttrs ++ Attrs}.

-spec add_functions(clj_module(), [erl_syntax:syntaxTree()]) -> clj_module().
add_functions(#{funs := Funs} = Module, AddFuns) ->
  Module#{funs => AddFuns ++ Funs}.

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
