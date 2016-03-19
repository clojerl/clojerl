-module('clojerl.Var').

-include("clojerl.hrl").

-behavior('clojerl.IDeref').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IMeta').
-behavior('clojerl.Named').
-behavior('clojerl.Stringable').

-export([ new/2
        , is_dynamic/1
        , is_macro/1
        ]).

-export([ function/1
        , module/1
        , val_function/1
        , process_args/3
        ]).

-export(['clojerl.IDeref.deref'/1]).
-export(['clojerl.IEquiv.equiv'/2]).
-export(['clojerl.IFn.invoke'/2]).
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export([ 'clojerl.Named.name'/1
        , 'clojerl.Named.namespace'/1
        ]).
-export(['clojerl.Stringable.str'/1]).

-type type() :: #?TYPE{}.

-spec new(binary(), binary()) -> type().
new(Ns, Name) ->
  #?TYPE{data = {Ns, Name}}.

-spec is_dynamic(type()) -> boolean().
is_dynamic(#?TYPE{name = ?M, info = #{meta := Meta}}) when is_map(Meta) ->
  maps:get(dynamic, Meta, false);
is_dynamic(#?TYPE{name = ?M}) ->
  false.

-spec is_macro(type()) -> boolean().

is_macro(#?TYPE{name = ?M, info = #{meta := Meta}}) when is_map(Meta) ->
  maps:get(macro, Meta, false);
is_macro(#?TYPE{name = ?M}) ->
  false.

-spec module(type()) -> atom().
module(#?TYPE{name = ?M, data = {Ns, _}}) ->
  binary_to_atom(Ns, utf8).

-spec function(type()) -> atom().
function(#?TYPE{name = ?M, data = {_, Name}}) ->
  binary_to_atom(Name, utf8).

-spec val_function(type()) -> atom().
val_function(#?TYPE{name = ?M, data = {_, Name}}) ->
  binary_to_atom(<<Name/binary, "__val">>, utf8).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Named.name'(#?TYPE{name = ?M, data = {_, Name}}) ->
  Name.

'clojerl.Named.namespace'(#?TYPE{name = ?M, data = {Namespace, _}}) ->
  Namespace.

'clojerl.Stringable.str'(#?TYPE{data = {Ns, Name}}) ->
  <<"#'", Ns/binary, "/", Name/binary>>.

'clojerl.IDeref.deref'(#?TYPE{data = {Ns, Name}} = Var) ->
  Module = module(Var),
  FunctionVal = val_function(Var),

  case erlang:function_exported(Module, FunctionVal, 0) of
    true -> Module:FunctionVal();
    false ->
      throw(<<"Could not derefence ",
              Ns/binary, "/", Name/binary, ". "
              "There is no Erlang function "
              "to back it up.">>)
  end.

'clojerl.IEquiv.equiv'( #?TYPE{name = ?M, data = X}
                      , #?TYPE{name = ?M, data = X}
                      ) ->
  true;
'clojerl.IEquiv.equiv'(_, _) ->
  false.

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'( #?TYPE{name = ?M, info = Info} = Keyword
                         , Metadata
                         ) ->
  Keyword#?TYPE{info = Info#{meta => Metadata}}.

'clojerl.IFn.invoke'(#?TYPE{name =?M} = Var, Args) ->
  Module = module(Var),
  Function = function(Var),

  Args1 = case clj_core:seq(Args) of
            undefined -> [];
            Seq       -> Seq
          end,

  Args2 = process_args(Var, Args1, fun clj_core:seq/1),

  erlang:apply(Module, Function, Args2).

-spec process_args(type(), [any()], function()) -> [any()].
process_args(#?TYPE{name = ?M} = Var, Args, RestFun) ->
  Meta = case 'clojerl.IMeta.meta'(Var) of
           undefined -> #{};
           M -> M
         end,

  IsVariadic    = maps:get('variadic?', Meta, false),
  MaxFixedArity = maps:get(max_fixed_arity, Meta, undefined),
  VariadicArity = maps:get(variadic_arity, Meta, undefined),

  ArgCount = length(Args),
  case IsVariadic of
    true when ArgCount =< MaxFixedArity, MaxFixedArity =/= undefined ->
      Args;
    true when ArgCount >= VariadicArity; MaxFixedArity == undefined ->
      {Args1, Rest} = case VariadicArity < ArgCount of
                        true -> lists:split(VariadicArity, Args);
                        false -> {Args, []}
                      end,
      Args1 ++ [RestFun(Rest)];
    _ -> Args
  end.
