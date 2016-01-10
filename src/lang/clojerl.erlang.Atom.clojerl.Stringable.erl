-module('clojerl.erlang.Atom.clojerl.Stringable').

-behaviour('clojerl.Stringable').

-export([str/1]).

str(Atom) ->
  AtomBin = atom_to_binary(Atom, utf8),
  <<":", AtomBin/binary>>.
