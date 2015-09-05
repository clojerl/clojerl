-module('clojerl.Var.clojerl.IDeref').

-behaviour('clojerl.IDeref').

-export([deref/1]).

deref({'clojerl.Var',
       #{ns := Namespace,
         name := Name}}) ->
  Module = binary_to_atom(clj_core:name(Namespace), utf8),
  Function = binary_to_atom(clj_core:name(Name), utf8),

  case erlang:function_exported(Module, Function, 1) of
    true -> Module:Function();
    false ->
      NsBin = clj_core:name(Namespace),
      NameBin = clj_core:name(Name),
      throw(<<"Could not derefence ",
              NsBin/binary, "/", NameBin/binary, ". "
              "There is no Erlang function "
              "to back it up.">>)
  end.
