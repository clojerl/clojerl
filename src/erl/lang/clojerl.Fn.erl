-module('clojerl.Fn').

-compile({no_auto_import, [{apply, 2}]}).

-include("clojerl.hrl").

-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.IStringable').

-export([ ?CONSTRUCTOR/1
        , fn/1
        ]).

-export([ apply/2
        , hash/1
        , str/1
        ]).

-export_type([type/0]).
-type type() :: #{ ?TYPE => ?M
                 , fn    => function()
                 }.

-spec ?CONSTRUCTOR(function()) -> type().
?CONSTRUCTOR(Fn) ->
  #{ ?TYPE => ?M
   , fn    => Fn
   }.

-spec fn(type()) -> function().
fn(#{?TYPE := ?M, fn := Fn}) ->
  Fn.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

apply(#{?TYPE := ?M, fn := Fn}, Args) when is_list(Args) ->
  Fn(Args);
apply(#{?TYPE := ?M, fn := Fn}, Args) ->
  Fn(clj_rt:to_list(Args)).

hash(#{?TYPE := ?M} = Fun) ->
  erlang:phash2(Fun).

str(#{?TYPE := ?M, fn := Fn})  ->
  {module, Module} = erlang:fun_info(Fn, module),
  {name, Name}     = erlang:fun_info(Fn, name),
  ModuleBin        = atom_to_binary(Module, utf8),
  NameBin          = atom_to_binary(Name, utf8),

  <<"#<", ModuleBin/binary, "/", NameBin/binary, ">">>.
