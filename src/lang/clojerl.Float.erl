-module('clojerl.Float').

-behaviour('clojerl.Stringable').

-export(['clojerl.Stringable.str'/1]).

'clojerl.Stringable.str'(Float) when is_float(Float) ->
  list_to_binary(io_lib:format("~w", [Float])).
