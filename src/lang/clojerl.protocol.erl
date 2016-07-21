-module('clojerl.protocol').

-export([
         resolve/3,
         'extends?'/2
        ]).

-spec resolve(atom(), atom(), list()) -> any().
resolve(Protocol, Function, Args = [Head | _]) ->
  Type = clj_core:type(Head),

  try
    apply(Type, Function, Args)
  catch
    Error:undef ->
      maybe_protocol_undef(Protocol, Type, Function, Args, Error)
  end.

maybe_protocol_undef(Proto, Type, Function, Args = [Head | _], Error) ->
  case erlang:function_exported(Type, Function, length(Args)) of
    false ->
      TypeBin = atom_to_binary(Type, utf8),
      FunctionBin = atom_to_binary(Function, utf8),
      ProtocolBin = atom_to_binary(Proto, utf8),
      Value = maybe_str(Head),
      ArgsStr = maybe_str(lists:map(fun maybe_str/1, Args)),
      error(<<"Type '", TypeBin/binary, "'"
              " has no implementation for function '",
              FunctionBin/binary,
              "' in protocol '",
              ProtocolBin/binary, "' ",
              "(value = ", Value/binary, ", args = ",
              ArgsStr/binary,
              ")">>);
    true ->
      erlang:raise(Error, undef, erlang:get_stacktrace())
  end.

-spec maybe_str(term()) -> binary().
maybe_str(X) ->
  case 'extends?'('clojerl.Stringable', clj_core:type(X)) of
    true -> clj_core:str(X);
    false -> iolist_to_binary(io_lib:format("~p", [X]))
  end.

-spec 'extends?'(atom(), atom()) -> boolean().
'extends?'(Protocol, Type) ->
  Key = {extends, Protocol, Type},
  case clj_cache:get(Key) of
    undefined ->
      Value = (erlang:function_exported(Type, module_info, 1)
               andalso
               lists:keymember([Protocol], 2, Type:module_info(attributes))
              ) orelse code:is_loaded(impl_module(Protocol, Type)) =/= false,
      clj_cache:put(Key, Value),
      Value;
    {ok, Value} -> Value
  end.

-spec impl_module(atom(), atom()) -> atom().
impl_module(Protocol, Type) when is_atom(Protocol),
                                 is_atom(Type) ->
  TypeBin = atom_to_binary(Type, utf8),
  ProtocolBin = atom_to_binary(Protocol, utf8),
  binary_to_atom(<<TypeBin/binary, ".", ProtocolBin/binary>>, utf8).
