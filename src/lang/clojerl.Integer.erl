-module('clojerl.Integer').

-behavior('clojerl.IHash').
-behavior('clojerl.Stringable').

-export([hash/1]).
-export([str/1]).

%% clojerl.Stringable

str(Int) when is_integer(Int) ->
  integer_to_binary(Int).

%% clojerl.IHash

hash(Int) when is_integer(Int) ->
  erlang:phash2(Int).
