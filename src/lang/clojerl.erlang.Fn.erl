-module('clojerl.erlang.Fn').

-behavior('clojerl.IFn').
-behaviour('clojerl.Stringable').

-export(['clojerl.Stringable.str'/1]).
-export(['clojerl.IFn.invoke'/2]).

'clojerl.IFn.invoke'(Fun, Args) ->
  {module, Module} = erlang:fun_info(Fun, module),
  IsClojure = case Module of
                erl_eval -> is_clj_fun(Fun);
                _ -> clj_module:is_clojure(Module)
              end,

  Args1 = case IsClojure of
            true  -> [Args];
            false -> Args
          end,

  apply(Fun, Args1).

'clojerl.Stringable.str'(Fun) ->
  {module, Module} = erlang:fun_info(Fun, module),
  {name, Name} = erlang:fun_info(Fun, name),
  ModuleBin = atom_to_binary(Module, utf8),
  NameBin = atom_to_binary(Name, utf8),

  <<"#<", ModuleBin/binary, "/", NameBin/binary, ">">>.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec is_clj_fun(function()) -> boolean().
is_clj_fun(Fun) ->
  {env, Env} = erlang:fun_info(Fun, env),
  case hd(Env) of
    {_, _, _, _, _Name} ->
      true;
    _ ->
      false
    end.
