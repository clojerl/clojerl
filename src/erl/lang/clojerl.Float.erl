-module('clojerl.Float').

-behavior('clojerl.IHash').
-behavior('clojerl.Stringable').

-export([hash/1]).
-export([str/1]).

%% clojerl.Stringable

str(Float) when is_float(Float) ->
  list_to_binary(io_lib:format("~w", [Float])).

%% clojerl.IHash

hash(Float) when is_float(Float) ->
  erlang:phash2(Float).
