-module('erlang.Fn').

-compile({no_auto_import, [{apply, 2}]}).

-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.IStringable').

-export([ apply/2
        , hash/1
        , str/1
        ]).

apply(Fun, Args) when is_list(Args) ->
  erlang:apply(Fun, Args);
apply(Fun, Args) ->
  erlang:apply(Fun, clj_rt:to_list(Args)).

hash(Fun) ->
  erlang:phash2(Fun).

str(Fun) ->
  {module, Module} = erlang:fun_info(Fun, module),
  {name, Name}     = erlang:fun_info(Fun, name),
  ModuleBin        = atom_to_binary(Module, utf8),
  NameBin          = atom_to_binary(Name, utf8),

  <<"#<", ModuleBin/binary, "/", NameBin/binary, ">">>.
