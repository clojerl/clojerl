-module('clojerl.erlang.Fn').

-behavior('clojerl.IFn').
-behaviour('clojerl.Stringable').

-export([prefix/1]).
-export(['clojerl.Stringable.str'/1]).
-export(['clojerl.IFn.invoke'/2]).

-spec prefix('clojerl.Symbol':type()) -> 'clojerl.Symbol':type().
prefix(Symbol) ->
  Ns   = clj_core:namespace(Symbol),
  Name = clj_core:name(Symbol),

  PrefixedName   = clj_utils:binary_join([<<"__clj__">>, Name], <<>>),
  PrefixedSymbol = clj_core:symbol(Ns, PrefixedName),

  Meta = clj_core:meta(Symbol),

  clj_core:with_meta(PrefixedSymbol, Meta).

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

  Name = case hd(Env) of
           {_, _, _, _, N} -> N;
           _               -> undefined
         end,

  case atom_to_binary(Name, utf8) of
    <<"__clj__", _/binary>> -> true;
    _                       -> false
  end.
