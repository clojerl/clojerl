-module('clojerl.Integer.clojerl.Stringable').

-behaviour('clojerl.Stringable').

-export([str/1]).

str(Int) when is_integer(Int) ->
  integer_to_binary(Int).
