-module('clojerl.erlang.Atom').

-behaviour('clojerl.Stringable').

-export(['clojerl.Stringable.str'/1]).

'clojerl.Stringable.str'(Atom) ->
  AtomBin = atom_to_binary(Atom, utf8),
  <<":", AtomBin/binary>>.
