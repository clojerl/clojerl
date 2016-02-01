-module('clojerl.erlang.Fn').

-behavior('clojerl.IFn').
-behaviour('clojerl.Stringable').

-export(['clojerl.Stringable.str'/1]).
-export(['clojerl.IFn.invoke'/2]).

'clojerl.IFn.invoke'(Fun, Args) ->
  erlang:apply(Fun, Args).

'clojerl.Stringable.str'(Fun) ->
  {module, Module} = erlang:fun_info(Fun, module),
  {name, Name} = erlang:fun_info(Fun, name),
  ModuleBin = atom_to_binary(Module, utf8),
  NameBin = atom_to_binary(Name, utf8),

  <<"#<", ModuleBin/binary, "/", NameBin/binary, ">">>.
