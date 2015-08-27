-module('clojerl.protocol').

-export([resolve/3]).

-spec resolve(atom(), atom(), list()) -> any().
resolve(ProtocolName, Function, Args = [Head | _]) ->
  Type = clj_core:type(Head),
  Module = list_to_atom(atom_to_list(Type) ++ "." ++ atom_to_list(ProtocolName)),

  try
    erlang:apply(Module, Function, Args)
  catch
    _:undef ->
      TypeBin = atom_to_binary(Type, utf8),
      ProtocolBin = atom_to_binary(ProtocolName, utf8),
      throw(<<"Type '", TypeBin/binary, "'"
              " has no implementation for protocol '",
              ProtocolBin/binary, "'">>)
  end.
