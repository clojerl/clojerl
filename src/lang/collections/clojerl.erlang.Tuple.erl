-module('clojerl.erlang.Tuple').

-behavior('clojerl.Counted').
-behavior('clojerl.IHash').
-behavior('clojerl.ISequential').
-behavior('clojerl.Indexed').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([count/1]).
-export([hash/1]).
-export(['_'/1]).
-export([ nth/2
        , nth/3
        ]).
-export([seq/1]).
-export([str/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(Tuple) -> tuple_size(Tuple).

hash(Tuple) -> clj_murmur3:ordered(Tuple).

'_'(_) -> undefined.

nth(Tuple, N) ->
  erlang:element(N + 1, Tuple).

nth(Tuple, N, NotFound) ->
  case N >= erlang:size(Tuple) of
    true  -> NotFound;
    false -> erlang:element(N + 1, Tuple)
  end.

seq({}) -> undefined;
seq(Tuple) -> tuple_to_list(Tuple).

str(Tuple) when is_tuple(Tuple) ->
  Items     = tuple_to_list(Tuple),
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs      = 'clojerl.String':join(ItemsStrs, <<", ">>),
  <<"#[", Strs/binary, "]">>.
