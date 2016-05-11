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
        , is_public/1
        , has_root/1
        , get/1
        ]).

-export([ function/1
        , module/1
        , val_function/1
        , process_args/3
        ]).

-export([ push_bindings/1
        , pop_bindings/0
        , get_bindings/0
        , get_bindings_map/0
        , reset_bindings/1
        , dynamic_binding/1
        , dynamic_binding/2
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

-type type() :: #?TYPE{data :: {binary(), binary()}}.

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

-spec is_public(type()) -> boolean().
is_public(#?TYPE{name = ?M, info = #{meta := Meta}}) when is_map(Meta) ->
  not maps:get(private, Meta, false);
is_public(#?TYPE{name = ?M}) ->
  true.

-spec has_root(type()) -> boolean().
has_root(#?TYPE{name = ?M, info = #{meta := Meta}}) when is_map(Meta) ->
  maps:get(has_root, Meta, false);
has_root(#?TYPE{name = ?M}) ->
  false.

-spec get(type()) -> boolean().
get(Var) -> 'clojerl.IDeref.deref'(Var).

-spec module(type()) -> atom().
module(#?TYPE{name = ?M, data = {Ns, _}}) ->
  binary_to_atom(Ns, utf8).

-spec function(type()) -> atom().
function(#?TYPE{name = ?M, data = {_, Name}}) ->
  binary_to_atom(Name, utf8).

-spec val_function(type()) -> atom().
val_function(#?TYPE{name = ?M, data = {_, Name}}) ->
  binary_to_atom(<<Name/binary, "__val">>, utf8).

-spec push_bindings(map()) -> ok.
push_bindings(BindingsMap) ->
  Bindings      = erlang:get(dynamic_bindings),
  NewBindings   = clj_scope:new(Bindings),
  AddBindingFun = fun(K, Acc) ->
                      clj_scope:put( Acc
                                   , clj_core:str(K)
                                   , clj_core:get(BindingsMap, K)
                                   )
                  end,
  NewBindings1  = lists:foldl( AddBindingFun
                             , NewBindings
                             , clj_core:keys(BindingsMap)
                             ),
  erlang:put(dynamic_bindings, NewBindings1),
  ok.

-spec pop_bindings() -> ok.
pop_bindings() ->
  Bindings = erlang:get(dynamic_bindings),
  Parent   = clj_scope:parent(Bindings),
  erlang:put(dynamic_bindings, Parent),
  ok.

-spec get_bindings() -> clj_scope:scope().
get_bindings() ->
  erlang:get(dynamic_bindings).

-spec get_bindings_map() -> map().
get_bindings_map() ->
  case erlang:get(dynamic_bindings) of
    undefined -> #{};
    Bindings  -> clj_scope:to_map(Bindings)
  end.

-spec reset_bindings(clj_scope:scope()) -> ok.
reset_bindings(Bindings) ->
  erlang:put(dynamic_bindings, Bindings).

-spec dynamic_binding('clojerl.Var':type()) -> any().
dynamic_binding(Var) ->
  case erlang:get(dynamic_bindings) of
    undefined -> undefined;
    Bindings  ->
      Key = clj_core:str(Var),
      clj_scope:get(Bindings, Key)
  end.

-spec dynamic_binding('clojerl.Var':type(), any()) -> any().
dynamic_binding(Var, Value) ->
  case erlang:get(dynamic_bindings) of
    undefined ->
      push_bindings(#{}),
      dynamic_binding(Var, Value);
    Bindings  ->
      Key = clj_core:str(Var),
      NewBindings = clj_scope:put(Bindings, Key, Value),
      erlang:put(dynamic_bindings, NewBindings),
      Value
  end.

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
    true ->
      case dynamic_binding(Var) of
        undefined -> Module:FunctionVal();
        Value     -> Value
      end;
    false ->
      throw(<<"Could not dereference ",
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
process_args(#?TYPE{name = ?M} = Var, Args, RestFun) when is_list(Args) ->
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
                        true  -> lists:split(VariadicArity, Args);
                        false -> {Args, []}
                      end,
      Args1 ++ [RestFun(Rest)];
    _ -> Args
  end;
process_args(Var, Args, RestFun) ->
  process_args(Var, clj_core:seq_to_list(Args), RestFun).
