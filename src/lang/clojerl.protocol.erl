-module('clojerl.protocol').

-export([
         resolve/3,
         'extends?'/2
        ]).

-spec resolve(atom(), atom(), list()) -> any().
resolve(Protocol, FunctionName, Args = [Head | _]) ->
  TypeModule = clj_core:type(Head),
  ImplFunction = impl_function(Protocol, FunctionName),

  IsExported = erlang:function_exported(TypeModule, ImplFunction, length(Args)),

  case IsExported of
    true ->
      apply(TypeModule, ImplFunction, Args);
    false ->
      ImplModule = impl_module(Protocol, TypeModule),
      IsExported2 = erlang:function_exported(ImplModule, FunctionName, length(Args)),
      case IsExported2 of
        true ->
          apply(TypeModule, ImplFunction, Args);
        false ->
          TypeBin = atom_to_binary(TypeModule, utf8),
          FunctionBin = atom_to_binary(FunctionName, utf8),
          ProtocolBin = atom_to_binary(Protocol, utf8),
          throw(<<"Type '", TypeBin/binary, "'"
                  " has no implementation for function '",
                  FunctionBin/binary,
                  "' in protocol '",
                  ProtocolBin/binary, "'">>)
        end
  end.

-spec 'extends?'(atom(), atom()) -> boolean().
'extends?'(Protocol, Type) ->
  (erlang:function_exported(Type, module_info, 1)
   andalso
   lists:keymember([Protocol], 2, Type:module_info(attributes))
  )
    orelse
    code:is_loaded(impl_module(Protocol, Type)) =/= false.

-spec impl_module(atom(), atom()) -> atom().
impl_module(Protocol, Type) when is_atom(Protocol),
                                 is_atom(Type) ->
  list_to_atom(
    atom_to_list(Type)
    ++ "."
    ++ atom_to_list(Protocol)
   ).

-spec impl_function(atom(), atom()) -> atom().
impl_function(Protocol, Function) when is_atom(Protocol),
                                       is_atom(Function) ->
  list_to_atom(
    atom_to_list(Protocol)
    ++ "."
    ++ atom_to_list(Function)
   ).
