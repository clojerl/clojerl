-module('clojerl.protocol').

-export([
         resolve/3,
         'extends?'/2
        ]).

-spec resolve(atom(), atom(), list()) -> any().
resolve(Protocol, FunctionName, Args = [Head | _]) ->
  Type = clj_core:type(Head),

  case resolve_impl_cache(Protocol, FunctionName, Type, length(Args)) of
    {Module, Function} ->
      apply(Module, Function, Args);
    undefined ->
      TypeBin = atom_to_binary(Type, utf8),
      FunctionBin = atom_to_binary(FunctionName, utf8),
      ProtocolBin = atom_to_binary(Protocol, utf8),
      Value = clj_core:str(Head),
      error(<<"Type '", TypeBin/binary, "'"
              " has no implementation for function '",
              FunctionBin/binary,
              "' in protocol '",
              ProtocolBin/binary, "' ",
              "(value = ", Value/binary, ", args = ",
              (clj_core:str(Args))/binary,
              ")">>)
  end.

-spec resolve_impl_cache(atom(), atom(), atom(), integer()) ->
  {module(), atom()} | undefined.
resolve_impl_cache(Protocol, Function, Type, Arity) ->
  Key = {resolve_impl, Protocol, Function, Type, Arity},
  case erlang:get(Key) of
    undefined ->
      Value = resolve_impl(Protocol, Function, Type, Arity),
      erlang:put(Key, Value),
      Value;
     Value -> Value
  end.

-spec resolve_impl(atom(), atom(), atom(), integer()) -> {module(), atom()} | undefined.
resolve_impl(Protocol, Function, Type, Arity) ->
  ImplFunction = impl_function(Protocol, Function),
  case erlang:function_exported(Type, ImplFunction, Arity) of
    true  -> {Type, ImplFunction};
    false ->
      ImplModule = impl_module(Protocol, Type),
      case erlang:function_exported(ImplModule, Function, Arity) of
        true  -> {ImplModule, Function};
        false -> undefined
      end
  end.

-spec 'extends?'(atom(), atom()) -> boolean().
'extends?'(Protocol, Type) ->
  Key = {extends, Protocol, Type},
  case erlang:get(Key) of
    undefined ->
      Value = (erlang:function_exported(Type, module_info, 1)
               andalso
               lists:keymember([Protocol], 2, Type:module_info(attributes))
              ) orelse code:is_loaded(impl_module(Protocol, Type)) =/= false,
      erlang:put(Key, Value),
      Value;
    Value -> Value
  end.

-spec impl_module(atom(), atom()) -> atom().
impl_module(Protocol, Type) when is_atom(Protocol),
                                 is_atom(Type) ->
  TypeBin = atom_to_binary(Type, utf8),
  ProtocolBin = atom_to_binary(Protocol, utf8),
  binary_to_atom(<<TypeBin/binary, ".", ProtocolBin/binary>>, utf8).

-spec impl_function(atom(), atom()) -> atom().
impl_function(Protocol, Function) when is_atom(Protocol),
                                       is_atom(Function) ->
  ProtocolBin = atom_to_binary(Protocol, utf8),
  FunctionBin = atom_to_binary(Function, utf8),
  binary_to_atom(<<ProtocolBin/binary, ".", FunctionBin/binary>>, utf8).
