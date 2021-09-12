-module('clojerl.Float').

-behavior('clojerl.IHash').
-behavior('clojerl.IStringable').

-export([hash/1]).
-export([str/1]).

-export_type([type/0]).
-type type() :: float().

%% clojerl.IStringable

str(Float) when is_float(Float) ->
  list_to_binary(io_lib:format("~w", [Float])).

%% clojerl.IHash

hash(Float) when is_float(Float) ->
  erlang:phash2(Float).
