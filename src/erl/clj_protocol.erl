-module(clj_protocol).

-include("clojerl.hrl").

-export([ resolve/3
        , resolve/4
        , resolve/5
        , resolve/6
        , resolve/7
        , resolve/8
        , resolve/9
        , 'satisfies?'/2
        , impl_module/2
        ]).

-spec resolve(atom(), atom(), any()) -> any().
resolve(Protocol, Function, Head) ->
  Type = clj_rt:type(Head),
  F    = resolve_impl_cache(Protocol, Function, Type, 1),
  F(Head).

-spec resolve(atom(), atom(), any(), any()) -> any().
resolve(Protocol, Function, Head, Arg1) ->
  Type = clj_rt:type(Head),
  F    = resolve_impl_cache(Protocol, Function, Type, 2),
  F(Head, Arg1).

-spec resolve(atom(), atom(), any(), any(), any()) -> any().
resolve(Protocol, Function, Head, Arg1, Arg2) ->
  Type = clj_rt:type(Head),
  F    = resolve_impl_cache(Protocol, Function, Type, 3),
  F(Head, Arg1, Arg2).

-spec resolve(atom(), atom(), any(), any(), any(), any()) -> any().
resolve(Protocol, Function, Head, Arg1, Arg2, Arg3) ->
  Type = clj_rt:type(Head),
  F    = resolve_impl_cache(Protocol, Function, Type, 4),
  F(Head, Arg1, Arg2, Arg3).

-spec resolve(atom(), atom(), any(), any(), any(), any(), any()) -> any().
resolve(Protocol, Function, Head, Arg1, Arg2, Arg3, Arg4) ->
  Type = clj_rt:type(Head),
  F    = resolve_impl_cache(Protocol, Function, Type, 5),
  F(Head, Arg1, Arg2, Arg3, Arg4).

-spec resolve(atom(), atom(), any(), any(), any(), any(), any(), any()) ->
  any().
resolve(Protocol, Function, Head, Arg1, Arg2, Arg3, Arg4, Arg5) ->
  Type = clj_rt:type(Head),
  F    = resolve_impl_cache(Protocol, Function, Type, 6),
  F(Head, Arg1, Arg2, Arg3, Arg4, Arg5).

-spec resolve( atom(), atom(), any(), any(), any(), any(), any(), any(), any()
             ) -> any().
resolve(Protocol, Function, Head, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6) ->
  Type = clj_rt:type(Head),
  F    = resolve_impl_cache(Protocol, Function, Type, 7),
  F(Head, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6).

-spec resolve_impl_cache(atom(), atom(), atom(), integer()) ->
  function().
resolve_impl_cache(Protocol, Function, Type, Arity) ->
  Key = {resolve_impl, Protocol, Function, Type, Arity},
  case erlang:get(Key) of
    undefined ->
      Value = case resolve_impl(Protocol, Function, Type, Arity) of
                undefined ->
                  resolve_impl(Protocol, Function, ?DEFAULT_TYPE, Arity);
                Fun ->
                  Fun
              end,

      clj_utils:error_when( Value =:= undefined
                          , [ <<"No implementation of method: ">>
                            , Function
                            , <<" of protocol: ">>
                            , Protocol
                            , <<" found for type: ">>
                            , Type
                            ]),

      erlang:put(Key, {ok, Value}),
      Value;
    {ok, Value} -> Value
  end.

-spec resolve_impl(atom(), atom(), atom(), integer()) ->
  function() | undefined.
resolve_impl(Protocol, Function, Type, Arity) ->
  case erlang:function_exported(Type, Function, Arity) of
    true  ->
      erlang:make_fun(Type, Function, Arity);
    false ->
      ImplModule = impl_module(Protocol, Type),
      case code:ensure_loaded(ImplModule) of
        {module, ImplModule} ->
          erlang:make_fun(ImplModule, Function, Arity);
        _ ->
          undefined
      end
  end.

-spec impl_module(atom() | binary(), atom() | binary()) -> atom().
impl_module(Protocol, Type)
  when is_atom(Protocol),
       is_atom(Type) ->
  TypeBin     = atom_to_binary(Type, utf8),
  ProtocolBin = atom_to_binary(Protocol, utf8),
  impl_module(ProtocolBin, TypeBin);
impl_module(ProtocolBin, TypeBin)
  when is_binary(ProtocolBin),
       is_binary(TypeBin) ->
  binary_to_atom(<<ProtocolBin/binary, "__", TypeBin/binary>>, utf8).

-spec 'satisfies?'(atom(), atom()) -> boolean().
'satisfies?'(Protocol, Type) ->
  Key = {satisfies, Protocol, Type},
  case erlang:get(Key) of
    undefined ->
      ImplModule = impl_module(Protocol, Type),
      Value = ( erlang:function_exported(Type, module_info, 1) andalso
                lists:keymember([Protocol], 2, Type:module_info(attributes))
              ) orelse {module, ImplModule} =:= code:ensure_loaded(ImplModule),
      erlang:put(Key, {ok, Value}),
      Value;
    {ok, Value} -> Value
  end.
