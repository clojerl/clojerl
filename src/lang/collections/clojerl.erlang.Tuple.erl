-module('clojerl.erlang.Tuple').

-behavior('clojerl.Counted').
-behavior('clojerl.IHash').
-behavior('clojerl.ISequential').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([count/1]).
-export([hash/1]).
-export([noop/1]).
-export([seq/1]).
-export([str/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(Tuple) -> tuple_size(Tuple).

hash(Tuple) -> clj_murmur3:ordered(Tuple).

noop(_) -> ok.

seq({}) -> undefined;
seq(Tuple) -> tuple_to_list(Tuple).

str(Tuple) when is_tuple(Tuple) ->
  Items = tuple_to_list(Tuple),
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs = clj_utils:binary_join(ItemsStrs, <<", ">>),
  <<"#[", Strs/binary, "]">>.
