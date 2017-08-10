-module('clojerl.TupleChunk').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IChunk').
-behavior('clojerl.IHash').
-behavior('clojerl.Indexed').
-behavior('clojerl.IReduce').

-export([?CONSTRUCTOR/1, ?CONSTRUCTOR/2, ?CONSTRUCTOR/3]).

-export([count/1]).
-export([drop_first/1]).
-export([hash/1]).
-export([nth/2, nth/3]).
-export([reduce/2, reduce/3]).

-type type() :: #?TYPE{data :: {tuple(), non_neg_integer(), non_neg_integer()}}.

-spec ?CONSTRUCTOR(tuple()) -> type().
?CONSTRUCTOR(Tuple) when is_tuple(Tuple) ->
  ?CONSTRUCTOR(Tuple, 0).

-spec ?CONSTRUCTOR(tuple(), non_neg_integer()) -> type().
?CONSTRUCTOR(Tuple, Offset) when is_tuple(Tuple), is_integer(Offset) ->
  ?CONSTRUCTOR(Tuple, Offset, erlang:tuple_size(Tuple)).

-spec ?CONSTRUCTOR(tuple(), non_neg_integer(), non_neg_integer()) -> type().
?CONSTRUCTOR(Tuple, Pos, End) when is_tuple(Tuple),
                                   is_integer(Pos),
                                   is_integer(End) ->
  #?TYPE{data = {Tuple, Pos, End}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(#?TYPE{name = ?M, data = {_, Offset, Size}}) ->
  Size - Offset.

drop_first(#?TYPE{name = ?M, data = {Tuple, Offset, Size}}) ->
  #?TYPE{name = ?M, data = {Tuple, Offset + 1, Size}}.

hash(#?TYPE{name = ?M, data = {Tuple, Offset, Size}}) ->
  Indexes = lists:seq(Offset + 1, Size),
  Items   = [erlang:element(I, Tuple)|| I <- Indexes],
  clj_murmur3:ordered(Items).

reduce(#?TYPE{name = ?M, data = {Tuple, Offset, Size}}, Fun) ->
  Size  = erlang:tuple_size(Tuple),
  Init  = erlang:element(Offset + 1, Tuple),
  Items = case Offset + 1 < Size of
            true ->
              Indexes = lists:seq(Offset + 2, Size),
              [erlang:element(Index, Tuple)|| Index <- Indexes];
            false ->
              []
          end,
  Apply = fun(Item, Acc) -> clj_rt:apply(Fun, [Acc, Item]) end,
  lists:foldl(Apply, Init, Items).

reduce(#?TYPE{name = ?M, data = {Tuple, Offset, Size}}, Fun, Init) ->
  Size     = erlang:tuple_size(Tuple),
  Items    = [ erlang:element(Index, Tuple)
               || Index <- lists:seq(Offset + 1, Size)
             ],
  ApplyFun = fun(Item, Acc) -> clj_rt:apply(Fun, [Acc, Item]) end,
  lists:foldl(ApplyFun, Init, Items).

nth(#?TYPE{name = ?M} = TupleChunk, N) ->
  nth(TupleChunk, N, ?NIL).

nth(#?TYPE{name = ?M, data = {Tuple, Offset, Size}}, N, _Default)
  when N >= 0 andalso N < Size - Offset ->
  erlang:element(Offset + N + 1, Tuple);
nth(#?TYPE{name = ?M}, _N, Default) ->
  Default.
