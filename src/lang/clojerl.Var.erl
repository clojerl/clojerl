-module('clojerl.Var').

-include("clojerl.hrl").

-behavior('clojerl.IDeref').
-behavior('clojerl.IFn').
-behavior('clojerl.IMeta').
-behavior('clojerl.Named').
-behavior('clojerl.Stringable').

-export([ new/2
        , namespace/1
        , name/1
        , is_dynamic/1
        , is_macro/1
        ]).

-export([ function/1
        , module/1
        , val_function/1
        , process_args/3
        ]).

-export(['clojerl.Stringable.str'/1]).
-export(['clojerl.IDeref.deref'/1]).
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export(['clojerl.IFn.invoke'/2]).
-export([ 'clojerl.Named.name'/1
        , 'clojerl.Named.namespace'/1
        ]).

-record(?M, { ns         = undefined :: 'clojerl.Symbol':type() | undefined
            , name                   :: 'clojerl.Symbol':type()
            , root       = undefined :: any() | undefined
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

-spec is_dynamic(type()) -> boolean().
is_dynamic(#?TYPE{name = ?M, info = Info}) ->
  Meta = maps:get(meta, Info, #{}),
  maps:get(dynamic, Meta, false).

-spec is_macro(type()) -> boolean().
is_macro(#?TYPE{name = ?M, info = Info}) ->
  Meta = maps:get(meta, Info, #{}),
  maps:get(macro, Meta, false).

-spec module(type()) -> atom().
module(#?TYPE{name = ?M} = Var) ->
  Ns = namespace(Var),
  'clojerl.Symbol':to_atom(Ns).

-spec function(type()) -> atom().
function(#?TYPE{name = ?M} = Var) ->
  Name = name(Var),
  'clojerl.Symbol':to_atom(Name).

-spec val_function(type()) -> atom().
val_function(#?TYPE{name = ?M} = Var) ->
  NameSym = name(Var),
  Name = clj_core:name(NameSym),
  binary_to_atom(<<Name/binary, "__val">>, utf8).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Named.name'(#?TYPE{name = ?M, data = #?M{name = NameSym}}) ->
  clj_core:name(NameSym).

'clojerl.Named.namespace'(#?TYPE{name = ?M, data = #?M{ns = NamespaceSym}}) ->
  clj_core:name(NamespaceSym).

'clojerl.Stringable.str'(#?TYPE{data = #?M{ns = NsSym, name = NameSym}}) ->
  <<"#'", (clj_core:str(NsSym))/binary
    , "/", (clj_core:str(NameSym))/binary>>.

'clojerl.IDeref.deref'(#?TYPE{data = #?M{ns = NsSym, name = NameSym}} = Var) ->
  Module = module(Var),
  FunctionVal = val_function(Var),

  case erlang:function_exported(Module, FunctionVal, 0) of
    true -> Module:FunctionVal();
    false ->
      NsBin = clj_core:name(NsSym),
      NameBin = clj_core:name(NameSym),
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

'clojerl.IFn.invoke'(#?TYPE{name =?M, data = Data} = Var, Args) ->
  #?M{ns = Namespace, name = Name} = Data,
  Module = binary_to_atom(clj_core:name(Namespace), utf8),
  Function = binary_to_atom(clj_core:name(Name), utf8),

  Args1 = case clj_core:seq(Args) of
            undefined -> [];
            Seq       -> Seq
          end,

  Args2 = process_args(Var, Args1, fun(X) -> X end),

  erlang:apply(Module, Function, Args2).

-spec process_args(type(), [any()], function()) -> [any()].
process_args(#?TYPE{name =?M} = Var, Args, RestFun) ->
  Meta = 'clojerl.IMeta.meta'(Var),

  IsVariadic    = maps:get('variadic?', Meta, false),
  MaxFixedArity = maps:get(max_fixed_arity, Meta, undefined),
  VariadicArity = maps:get(variadic_arity, Meta, undefined),

  ArgCount = length(Args),
  case IsVariadic of
    true when ArgCount =< MaxFixedArity, MaxFixedArity =/= undefined ->
      Args;
    true when ArgCount >= VariadicArity; MaxFixedArity == undefined ->
      {Args1, Rest} = lists:split(VariadicArity, Args),
      Args1 ++ [RestFun(Rest)];
    _ -> Args
  end.
