-module('clojerl.erlang.Fn').

-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behaviour('clojerl.Stringable').

-export([prefix/1]).
-export(['clojerl.IFn.invoke'/2]).
-export(['clojerl.IHash.hash'/1]).
-export(['clojerl.Stringable.str'/1]).

-spec prefix('clojerl.Symbol':type()) -> 'clojerl.Symbol':type().
prefix(Symbol) ->
  Ns   = clj_core:namespace(Symbol),
  Name = clj_core:name(Symbol),

  PrefixedName   = <<"__clj__", Name/binary>>,
  PrefixedSymbol = clj_core:symbol(Ns, PrefixedName),

  Meta = clj_core:meta(Symbol),

  clj_core:with_meta(PrefixedSymbol, Meta).

'clojerl.IFn.invoke'(Fun, Args) when is_function(Fun), is_list(Args) ->
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

'clojerl.IHash.hash'(Fun) when is_function(Fun) ->
  erlang:phash2(Fun).

'clojerl.Stringable.str'(Fun) when is_function(Fun) ->
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
