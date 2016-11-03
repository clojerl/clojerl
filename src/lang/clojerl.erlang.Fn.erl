-module('clojerl.erlang.Fn').

-compile({no_auto_import, [{apply, 2}]}).

-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behaviour('clojerl.Stringable').

-export([ apply/2
        , hash/1
        , str/1
        ]).

apply(Fun, Args) when is_function(Fun), is_list(Args) ->
  {module, Module} = erlang:fun_info(Fun, module),

  Args1 = case clj_module:is_clojure(Module) of
            true  -> [Args];
            false -> Args
          end,

  erlang:apply(Fun, Args1);
apply(Fun, Args) when is_function(Fun) ->
  apply(Fun, clj_core:to_list(Args)).

hash(Fun) when is_function(Fun) ->
  erlang:phash2(Fun).

str(Fun) when is_function(Fun) ->
  {module, Module} = erlang:fun_info(Fun, module),
  {name, Name} = erlang:fun_info(Fun, name),
  ModuleBin = atom_to_binary(Module, utf8),
  NameBin = atom_to_binary(Name, utf8),

  <<"#<", ModuleBin/binary, "/", NameBin/binary, ">">>.
