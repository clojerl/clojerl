-module(clj_protocol).

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-export([ not_implemented/3
        , 'satisfies?'/2
        , impl_module/2
        ]).

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
      Value = ( {module, Type} =:= code:ensure_loaded(Type) andalso
                erlang:function_exported(Type, module_info, 1) andalso
                lists:keymember([Protocol], 2, Type:module_info(attributes))
              ) orelse {module, ImplModule} =:= code:ensure_loaded(ImplModule),
      erlang:put(Key, {ok, Value}),
      Value;
    {ok, Value} -> Value
  end.

-spec not_implemented(module(), atom(), module()) -> ok.
not_implemented(Protocol, Function, Type) ->
  ?ERROR( [ <<"No implementation of method: '">>
          , atom_to_binary(Function, utf8)
          , <<"' of protocol: ">>
          , atom_to_binary(Protocol, utf8)
          , <<" found for type: ">>
          , atom_to_binary(Type, utf8)
          ]).
