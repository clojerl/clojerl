-module('clojerl.Float').

-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IStringable').

-export([equiv/2]).
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

%% clojerl.IEquiv

equiv(X, Y) when is_float(X) andalso is_float(Y) ->
  X == Y;
equiv(X, Y) ->
  X =:= Y.
