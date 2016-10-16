-module('clojerl.Var').

-include("clojerl.hrl").

-behavior('clojerl.IDeref').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.Named').
-behavior('clojerl.Stringable').

-export([ ?CONSTRUCTOR/2
        , is_dynamic/1
        , is_macro/1
        , is_public/1
        , is_bound/1
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

-export([deref/1]).
-export([equiv/2]).
-export([invoke/2]).
-export([hash/1]).
-export([ meta/1
        , with_meta/2
        ]).
-export([ name/1
        , namespace/1
        ]).
-export([str/1]).

-type type() :: #?TYPE{data :: {binary(), binary()}}.

-spec ?CONSTRUCTOR(binary(), binary()) -> type().
?CONSTRUCTOR(Ns, Name) ->
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

-spec is_bound(type()) -> boolean().
is_bound(#?TYPE{name = ?M} = Var) ->
  case deref(Var) of
    ?UNBOUND -> false;
    _ -> true
  end.

-spec has_root(type()) -> boolean().
has_root(#?TYPE{name = ?M, info = #{meta := Meta}}) when is_map(Meta) ->
  maps:get(has_root, Meta, false);
has_root(#?TYPE{name = ?M}) ->
  false.

-spec get(type()) -> boolean().
get(Var) -> deref(Var).

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
                                   , {ok, clj_core:get(BindingsMap, K)}
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
    Bindings  ->
      UnwrapFun = fun(_, {ok, X}) -> X end,
      clj_scope:to_map(Bindings, UnwrapFun)
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
      NewBindings = try
                      clj_scope:update(Bindings, Key, {ok, Value})
                    catch
                      throw:{not_found, _} ->
                        clj_scope:put(Bindings, Key, {ok, Value})
                    end,
      erlang:put(dynamic_bindings, NewBindings),
      Value
  end.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

name(#?TYPE{name = ?M, data = {_, Name}}) ->
  Name.

namespace(#?TYPE{name = ?M, data = {Namespace, _}}) ->
  Namespace.

str(#?TYPE{name = ?M, data = {Ns, Name}}) ->
  <<"#'", Ns/binary, "/", Name/binary>>.

deref(#?TYPE{name = ?M, data = {Ns, Name}} = Var) ->
  Module      = module(Var),
  FunctionVal = val_function(Var),
  %% HACK
  Fun         = clj_module:fake_fun(Module, FunctionVal, 0),

  try
    %% Make the call in case the module is not loaded and handle the case
    %% when it doesn't even exist gracefully.
    Fun()
  catch
    Type:undef ->
      case erlang:function_exported(Module, FunctionVal, 0) of
        false -> throw(<<"Could not dereference ",
                         Ns/binary, "/", Name/binary, ". "
                         "There is no Erlang function "
                         "to back it up.">>);
        true  -> erlang:raise(Type, undef, erlang:get_stacktrace())
      end
  end.

equiv( #?TYPE{name = ?M, data = X}
     , #?TYPE{name = ?M, data = X}
     ) ->
  true;
equiv(_, _) ->
  false.

hash(#?TYPE{name = ?M, data = Data}) ->
  erlang:phash2(Data).

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

with_meta( #?TYPE{name = ?M, info = Info} = Keyword
         , Metadata
         ) ->
  Keyword#?TYPE{info = Info#{meta => Metadata}}.

invoke(#?TYPE{name = ?M} = Var, Args) ->
  Module   = module(Var),
  Function = function(Var),
  Args1    = case clj_core:seq(Args) of
               undefined -> [];
               Seq       -> Seq
             end,
  Args2    = process_args(Var, Args1, fun clj_core:seq/1),
  %% HACK
  Fun      = clj_module:fake_fun(Module, Function, length(Args2)),

  erlang:apply(Fun, Args2).

-spec process_args(type(), [any()], function()) -> [any()].
process_args(#?TYPE{name = ?M} = Var, Args, RestFun) when is_list(Args) ->
  Meta = case meta(Var) of
           undefined -> #{};
           M -> M
         end,

  case maps:get('variadic?', Meta, false) of
    false -> Args;
    true ->
      MaxFixedArity = maps:get(max_fixed_arity, Meta),
      VariadicArity = maps:get(variadic_arity, Meta),
      ArgCount = length(Args),
      if
        MaxFixedArity =:= undefined;
        MaxFixedArity < ArgCount, ArgCount >= VariadicArity ->
          {Args1, Rest} = case VariadicArity < ArgCount of
                            true  -> lists:split(VariadicArity, Args);
                            false -> {Args, []}
                          end,
          Args1 ++ [RestFun(Rest)];
        true -> Args
      end
  end;
process_args(Var, Args, RestFun) ->
  process_args(Var, clj_core:to_list(Args), RestFun).
