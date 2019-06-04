-module(clj_protocol).

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-export([ not_implemented/3
        , impl_module/2
        ]).

-spec impl_module(atom() | binary(), atom() | binary()) -> atom().
impl_module(ProtocolBin, TypeBin)
  when is_binary(ProtocolBin),
       is_binary(TypeBin) ->
  binary_to_atom(<<ProtocolBin/binary, "__", TypeBin/binary>>, utf8).

-spec not_implemented(module(), atom(), any()) -> no_return().
not_implemented(Protocol, Function, Value) ->
  Type = clj_rt:type_module(Value),
  ?ERROR( [ <<"No implementation of method: '">>
          , atom_to_binary(Function, utf8)
          , <<"' of protocol: ">>
          , atom_to_binary(Protocol, utf8)
          , <<" found for type: ">>
          , atom_to_binary(Type, utf8)
          ]).
