-module('clojerl.protocol').

-export([ resolve/3
        , 'extends?'/2
        ]).

-spec resolve(atom(), atom(), list()) -> any().
resolve(_Protocol, Function, Args = [Head | _]) ->
  Type = clj_core:type(Head),

  apply(Type, Function, Args).

-spec 'extends?'(atom(), atom()) -> boolean().
'extends?'(Protocol, Type) ->
  Key = {extends, Protocol, Type},
  case clj_cache:get(Key) of
    undefined ->
      Value = ( erlang:function_exported(Type, module_info, 1) andalso
                lists:keymember([Protocol], 2, Type:module_info(attributes))
              ),
      clj_cache:put(Key, Value),
      Value;
    {ok, Value} -> Value
  end.
