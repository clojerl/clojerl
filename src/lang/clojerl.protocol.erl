-module('clojerl.protocol').

-export([
         resolve/3,
         impl_module/2
        ]).

-spec resolve(atom(), atom(), list()) -> any().
resolve(Protocol, Function, Args = [Head | _]) ->
  Type = clj_core:type(Head),
  Module = impl_module(Protocol, Type),

  try
    apply(Module, Function, Args)
  catch
    _:undef ->
      case erlang:function_exported(Module, Function, length(Args)) of
        false ->
          TypeBin = atom_to_binary(Type, utf8),
          FunctionBin = atom_to_binary(Function, utf8),
          ProtocolBin = atom_to_binary(Protocol, utf8),
          throw(<<"Type '", TypeBin/binary, "'"
                  " has no implementation for function '",
                  FunctionBin/binary,
                  "' in protocol '",
                  ProtocolBin/binary, "'">>);
        true -> throw(undef)
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
