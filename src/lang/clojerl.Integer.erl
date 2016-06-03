-module('clojerl.Integer').

-behavior('clojerl.IHash').
-behavior('clojerl.Stringable').

-export(['clojerl.IHash.hash'/1]).
-export(['clojerl.Stringable.str'/1]).

%% clojerl.Stringable

'clojerl.Stringable.str'(Int) when is_integer(Int) ->
  integer_to_binary(Int).

%% clojerl.IHash

'clojerl.IHash.hash'(Int) when is_integer(Int) ->
  erlang:phash2(Int).
