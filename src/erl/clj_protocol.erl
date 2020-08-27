%% @doc Clojerl protocol helper functions.
-module(clj_protocol).

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-export([ not_implemented/3
        , impl_module/2
        ]).

%% @doc Returns the name of the module that implements `Protocol' for
%% `Type'.
-spec impl_module(atom() | binary(), atom() | binary()) -> atom().
impl_module(Protocol, Type)
  when is_binary(Protocol),
       is_binary(Type) ->
  binary_to_atom(<<Protocol/binary, "__", Type/binary>>, utf8).

%% @doc Generates an error with information regarding an unimplemented
%% protocol function.
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
