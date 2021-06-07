-module('clojerl.Var').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behavior('clojerl.IDeref').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.INamed').
-behavior('clojerl.IStringable').

-export([ ?CONSTRUCTOR/2
        , create/0
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
        , process_args/2
        , is_valid_arity/2
        , fake_fun/2
        ]).

-export([ push_bindings/1
        , pop_bindings/0
        , get_bindings/0
        , get_bindings_map/0
        , reset_bindings/1
        , dynamic_binding/1
        , dynamic_binding/2
        , find/1
        ]).

-export([deref/1]).
-export([equiv/2]).
-export([apply/2]).
-export([hash/1]).
-export([ meta/1
        , with_meta/2
        ]).
-export([ name/1
        , namespace/1
        ]).
-export([str/1]).

-type type() :: #{ ?TYPE     => ?M
                 , ns        => binary()
                 , name      => binary()
                 , ns_atom   => atom()
                 , name_atom => atom()
                 , val_atom  => atom()
                 , meta      => ?NIL | any()
                 , fake_fun  => boolean()
                 }.

-spec ?CONSTRUCTOR(binary(), binary()) -> type().
?CONSTRUCTOR(Ns, Name) ->
  #{ ?TYPE     => ?M
   , ns        => Ns
   , name      => Name
   , ns_atom   => binary_to_atom(Ns, utf8)
   , name_atom => binary_to_atom(Name, utf8)
   , val_atom  => binary_to_atom(<<Name/binary, "__val">>, utf8)
   , meta      => ?NIL
   , fake_fun  => false
   }.

-spec create() -> type().
create() ->
  Ns   = 'erlang.util.UUID':random(),
  Name = 'erlang.util.UUID':random(),
  ?CONSTRUCTOR(clj_rt:str(Ns), clj_rt:str(Name)).

-spec is_dynamic(type()) -> boolean().
is_dynamic(#{?TYPE := ?M, meta := Meta}) when is_map(Meta) ->
  clj_rt:get(Meta, dynamic, false);
is_dynamic(#{?TYPE := ?M}) ->
  false.

-spec is_macro(type()) -> boolean().
is_macro(#{?TYPE := ?M, meta := Meta}) when is_map(Meta) ->
  clj_rt:get(Meta, macro, false);
is_macro(#{?TYPE := ?M}) ->
  false.

-spec is_public(type()) -> boolean().
is_public(#{?TYPE := ?M, meta := Meta}) when is_map(Meta) ->
  not clj_rt:get(Meta, private, false);
is_public(#{?TYPE := ?M}) ->
  true.

-spec is_bound(type()) -> boolean().
is_bound(#{?TYPE := ?M} = Var) ->
  has_root(Var)
  orelse ( is_dynamic(Var)
           andalso clj_scope:contains(str(Var), get_bindings())
         ).

-spec has_root(type()) -> boolean().
has_root(#{?TYPE := ?M} = Var) ->
  deref(Var) =/= ?UNBOUND.

-spec get(type()) -> boolean().
get(Var) -> deref(Var).

-spec module(type()) -> atom().
module(#{?TYPE := ?M, ns_atom := NsAtom}) ->
  NsAtom.

-spec function(type()) -> atom().
function(#{?TYPE := ?M, name_atom := NameAtom}) ->
  NameAtom.

-spec val_function(type()) -> atom().
val_function(#{?TYPE := ?M, val_atom := ValAtom}) ->
  ValAtom.

-spec fake_fun(type(), boolean()) -> type().
fake_fun(#{?TYPE := ?M} = Var, IsFakeFun) ->
  Var#{fake_fun => IsFakeFun}.

-spec push_bindings('clojerl.IMap':type()) -> ok.
push_bindings(BindingsMap) ->
  Bindings      = get_bindings(),
  NewBindings   = clj_scope:new(Bindings),
  AddBindingFun = fun(K, Acc) ->
                      clj_scope:put( clj_rt:str(K)
                                   , {ok, clj_rt:get(BindingsMap, K)}
                                   , Acc
                                   )
                  end,
  NewBindings1  = lists:foldl( AddBindingFun
                             , NewBindings
                             , clj_rt:to_list(clj_rt:keys(BindingsMap))
                             ),
  erlang:put(dynamic_bindings, NewBindings1),
  ok.

-spec pop_bindings() -> ok.
pop_bindings() ->
  Bindings = get_bindings(),
  Parent   = clj_scope:parent(Bindings),
  erlang:put(dynamic_bindings, Parent),
  ok.

-spec get_bindings() -> clj_scope:scope().
get_bindings() ->
  case erlang:get(dynamic_bindings) of
    undefined -> ?NIL;
    Bindings  -> Bindings
  end.

-spec get_bindings_map() -> map().
get_bindings_map() ->
  case get_bindings() of
    ?NIL      -> #{};
    Bindings  ->
      UnwrapFun = fun(_, {ok, X}) -> X end,
      clj_scope:to_map(UnwrapFun, Bindings)
  end.

-spec reset_bindings(clj_scope:scope()) -> ok.
reset_bindings(Bindings) ->
  erlang:put(dynamic_bindings, Bindings).

-spec dynamic_binding('clojerl.Var':type() | binary()) -> any().
dynamic_binding(Key) when is_binary(Key) ->
  clj_scope:get(Key, get_bindings());
dynamic_binding(Var) ->
  dynamic_binding(str(Var)).

-spec dynamic_binding('clojerl.Var':type() | binary(), any()) -> any().
dynamic_binding(Key, Value) when is_binary(Key) ->
  case get_bindings() of
    ?NIL -> push_bindings(#{});
    X -> X
  end,
  Bindings0 = get_bindings(),
  Bindings1 = case clj_scope:update(Key, {ok, Value}, Bindings0) of
                  not_found -> clj_scope:put(Key, {ok, Value}, Bindings0);
                  Bindings  -> Bindings
                end,
  erlang:put(dynamic_bindings, Bindings1),
  Value;
dynamic_binding(Var, Value) ->
  dynamic_binding(str(Var), Value).

-spec find('clojerl.Symbol':type()) -> type() | ?NIL.
find(QualifiedSymbol) ->
  NsName = clj_rt:namespace(QualifiedSymbol),
  ?ERROR_WHEN( NsName =:= ?NIL
             , <<"Symbol must be namespace-qualified">>
             ),

  Ns = 'clojerl.Namespace':find(clj_rt:symbol(NsName)),
  ?ERROR_WHEN( Ns =:= ?NIL
             , [<<"No such namespace: ">>, NsName]
             ),

  'clojerl.Namespace':find_var(Ns, QualifiedSymbol).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

name(#{?TYPE := ?M, name := Name}) ->
  Name.

namespace(#{?TYPE := ?M, ns := Ns}) ->
  Ns.

str(#{?TYPE := ?M, ns := Ns, name := Name}) ->
  <<"#'", Ns/binary, "/", Name/binary>>.

deref(#{ ?TYPE    := ?M
       , ns       := Ns
       , name     := Name
       , ns_atom  := Module
       , val_atom := FunctionVal
       , fake_fun := FakeFun
       } = Var) ->
  try
    %% Make the call in case the module is not loaded and handle the case
    %% when it doesn't even exist gracefully.
    apply_fun(FakeFun, Module, FunctionVal, 0, [])
  catch
    ?WITH_STACKTRACE(Type, undef, Stacktrace)
      case erlang:function_exported(Module, FunctionVal, 0) of
        false ->
          case deref_dynamic(Var) of
            {ok, Value} -> Value;
            ?NIL        -> ?ERROR(<<"Could not dereference ",
                                    Ns/binary, "/", Name/binary, ". "
                                    "There is no Erlang function "
                                    "to back it up.">>)
          end;
        true  ->
          erlang:raise(Type, undef, Stacktrace)
      end
  end.

-spec deref_dynamic(type()) -> ?NIL | {ok, any()}.
deref_dynamic(Var) ->
  case is_dynamic(Var) of
    true -> dynamic_binding(Var);
    false -> ?NIL
  end.

equiv( #{?TYPE := ?M, ns := Ns, name := Name}
     , #{?TYPE := ?M, ns := Ns, name := Name}
     ) ->
  true;
equiv(_, _) ->
  false.

hash(#{?TYPE := ?M, ns := Ns, name := Name}) ->
  erlang:phash2({Ns, Name}).

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = Var, Metadata) ->
  Var#{meta => Metadata}.

apply( #{ ?TYPE     := ?M
        , ns_atom   := Module
        , name_atom := Function
        , fake_fun  := FakeFun
        , meta      := Meta
        }
     , Args0
     ) ->
  {Arity, Args1} = case process_args(Meta, Args0) of
                     {Arity_, Args_, Rest_} ->
                       {Arity_, Args_ ++ [Rest_]};
                     X -> X
                   end,

  apply_fun(FakeFun, Module, Function, Arity, Args1).

-spec is_valid_arity(Meta :: map(), Arity :: arity()) -> boolean().
is_valid_arity(#{'variadic?' := true} = Meta, Arity) ->
  #{ max_fixed_arity := MaxFixedArity
   , fixed_arities   := FixedArities
   } = Meta,
  MaxFixedArity =:= ?NIL
    orelse MaxFixedArity =< Arity
    orelse lists:member(Arity, FixedArities);
is_valid_arity(#{fixed_arities := FixedArities}, Arity) ->
  lists:member(Arity, FixedArities).

-spec process_args(map(), [any()]) ->
  {arity(), [any()]} | {arity(), [any()], any()}.
process_args(#{'variadic?' := true} = Meta, Args) ->
  #{ max_fixed_arity := MaxFixedArity
   , variadic_arity  := VariadicArity
   } = Meta,
  {Length, Args1, Rest} = bounded_length(Args, VariadicArity),
  if
    MaxFixedArity =/= ?NIL
    andalso Rest =:= ?NIL
    andalso (MaxFixedArity >= Length orelse Length < VariadicArity) ->
      {Length, Args1};
    true ->
      {Length + 1, Args1, Rest}
  end;
process_args(_, Args) when is_list(Args) ->
  {length(Args), Args};
process_args(_, Args) ->
  Args1 = clj_rt:to_list(Args),
  {length(Args1), Args1}.

-spec bounded_length(any(), non_neg_integer()) ->
  {non_neg_integer(), list(), any()}.
bounded_length(Args, Max) when is_list(Args) ->
  Length = length(Args),
  case Length =< Max of
    true  -> {Length, Args, ?NIL};
    false ->
      {Args1, Rest} = lists:split(Max, Args),
      {Max, Args1, Rest}
  end;
bounded_length(?NIL, Max) ->
  bounded_length(?NIL, 0, Max, []);
bounded_length(Args, Max) ->
  TypeModule = clj_rt:type_module(Args),
  bounded_length(TypeModule:seq(Args), 0, Max, []).

-spec bounded_length(any(), non_neg_integer(), non_neg_integer(), list()) ->
  {non_neg_integer(), list(), any()}.
bounded_length(?NIL, N, _Max, Acc) ->
  {N, lists:reverse(Acc), ?NIL};
bounded_length(Rest, N, _Max = N, Acc) ->
  {N, lists:reverse(Acc), Rest};
bounded_length(Rest, N, Max, Acc) ->
  TypeModule = clj_rt:type_module(Rest),
  First = TypeModule:first(Rest),
  Rest1 = TypeModule:next(Rest),
  bounded_length(Rest1, N + 1, Max, [First | Acc]).

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec apply_fun(type(), module(), atom(), arity(), [any()]) -> any().
apply_fun(false, Module, Function, _Arity, Args) ->
  erlang:apply(Module, Function, Args);
apply_fun(_, Module, Function, Arity, Args) ->
  Fun = clj_module:fake_fun(Module, Function, Arity),
  erlang:apply(Fun, Args).
