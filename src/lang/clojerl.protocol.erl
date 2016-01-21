-module('clojerl.protocol').

-export([
         resolve/3,
         impl_module/2
        ]).

-spec resolve(atom(), atom(), list()) -> any().
resolve(Protocol, FunctionName, Args = [Head | _]) ->
  TypeModule = clj_core:type(Head),
  ImplFunction = impl_function(Protocol, FunctionName),
  ImplModule = impl_module(Protocol, TypeModule),

  IsExported = erlang:function_exported(TypeModule, ImplFunction, length(Args)),

  try
    {Module, Function} = case IsExported of
                           true -> {TypeModule, ImplFunction};
                           false -> {ImplModule, FunctionName}
                         end,
    apply(Module, Function, Args)
  catch
    _:undef ->
      case erlang:function_exported(ImplModule, FunctionName, length(Args)) of
        false ->
          TypeBin = atom_to_binary(TypeModule, utf8),
          FunctionBin = atom_to_binary(FunctionName, utf8),
          ProtocolBin = atom_to_binary(Protocol, utf8),
          throw(<<"Type '", TypeBin/binary, "'"
                  " has no implementation for function '",
                  FunctionBin/binary,
                  "' in protocol '",
                  ProtocolBin/binary, "'">>);
        true -> throw({undef, erlang:get_stacktrace()})
      end
  end.

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
