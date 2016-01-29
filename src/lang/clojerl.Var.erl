-module('clojerl.Var').

-include("clojerl.hrl").

-behavior('clojerl.IDeref').
-behavior('clojerl.Stringable').
-behavior('clojerl.IMeta').
-behavior('clojerl.IFn').

-export([
         new/2,
         namespace/1,
         name/1,
         dynamic/1, dynamic/2,
         is_macro/1
        ]).
-export(['clojerl.Stringable.str'/1]).
-export(['clojerl.IDeref.deref'/1]).
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export(['clojerl.IFn.invoke'/2]).

-record(?M, { ns         = undefined :: 'clojerl.Symbol':type() | undefined
            , name                   :: 'clojerl.Symbol':type()
            , root       = undefined :: any() | undefined
            , meta       = #{}       :: map()
            , is_dynamic = false     :: boolean()
            , is_macro   = false     :: boolean()
            }).

-type type() :: #?TYPE{}.

-spec new('clojerl.Symbol':type(), 'clojerl.Symbol':type()) -> type().
new(NsSym, NameSym) ->
  Data = #?M{ ns         = NsSym
            , name       = NameSym
            },
  #?TYPE{data = Data}.

-spec namespace(type()) -> 'clojerl.Symbol':type().
namespace(#?TYPE{data = #?M{ns = Namespace}}) -> Namespace.

-spec name(type()) -> 'clojerl.Symbol':type().
name(#?TYPE{data = #?M{name = Name}}) -> Name.

-spec dynamic(type()) -> boolean().
dynamic(#?TYPE{data = #?M{is_dynamic = IsDynamic}}) -> IsDynamic.

-spec dynamic(type(), boolean()) -> type().
dynamic(#?TYPE{name = ?M, data = Data} = Var, IsDynamic) ->
  Var#?TYPE{data = Data#?M{is_dynamic = IsDynamic}}.

-spec is_macro(type()) -> boolean().
is_macro(#?TYPE{data = #?M{is_macro = IsMacro}}) -> IsMacro.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Stringable.str'(#?TYPE{data = #?M{ns = NsSym, name = NameSym}}) ->
  <<(clj_core:str(NsSym))/binary
    , "/"
    , (clj_core:str(NameSym))/binary>>.

'clojerl.IDeref.deref'(#?TYPE{data = #?M{ns = Namespace, name = Name}}) ->
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

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'( #?TYPE{name = ?M, info = Info} = Keyword
                         , Metadata
                         ) ->
  Keyword#?TYPE{info = Info#{meta => Metadata}}.

'clojerl.IFn.invoke'(#?TYPE{name =?M, data = Data}, Args) ->
  #?M{ns = Namespace, name = Name} = Data,
  Module = binary_to_atom(clj_core:name(Namespace), utf8),
  Function = binary_to_atom(clj_core:name(Name), utf8),

  erlang:apply(Module, Function, clj_core:seq(Args)).
