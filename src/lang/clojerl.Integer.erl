-module('clojerl.Integer').

-behaviour('clojerl.Stringable').

-export(['clojerl.Stringable.str'/1]).

'clojerl.Stringable.str'(Int) when is_integer(Int) ->
  integer_to_binary(Int).
