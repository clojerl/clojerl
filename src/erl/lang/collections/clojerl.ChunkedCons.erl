-module('clojerl.ChunkedCons').

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IChunkedSeq').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.IReduce').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

-export([?CONSTRUCTOR/2]).

-export([count/1]).
-export([ cons/2
        , empty/1
        ]).
-export([ chunked_first/1
        , chunked_more/1
        , chunked_next/1
        ]).
-export([equiv/2]).
-export([hash/1]).
-export([ meta/1
        , with_meta/2
        ]).
-export([ reduce/2
        , reduce/3
        ]).
-export([ first/1
        , next/1
        , more/1
        ]).
-export(['_'/1]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

-type type() :: #?TYPE{}.

-spec ?CONSTRUCTOR(any(), any()) -> type().
?CONSTRUCTOR(Chunk, More) ->
  #?TYPE{data = {Chunk, More}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(#?TYPE{name = ?M, data = {Chunk, More}}) ->
  clj_rt:count(Chunk) + clj_rt:count(More).

cons(#?TYPE{name = ?M} = ChunkedCons, X) ->
  clj_rt:cons(X, ChunkedCons).

empty(_) -> [].

chunked_first(#?TYPE{name = ?M, data = {Chunk, _}}) ->
  Chunk.

chunked_next(#?TYPE{name = ?M} = ChunkedCons) ->
  clj_rt:seq(chunked_more(ChunkedCons)).

chunked_more(#?TYPE{name = ?M, data = {_, ?NIL}}) -> [];
chunked_more(#?TYPE{name = ?M, data = {_, More}}) -> More.

equiv( #?TYPE{name = ?M, data = {ChunkX, MoreX}}
     , #?TYPE{name = ?M, data = {ChunkY, MoreY}}
     ) ->
  clj_rt:equiv(ChunkX, ChunkY) andalso clj_rt:equiv(MoreX, MoreY);
equiv(#?TYPE{name = ?M} = ChunkedCons, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> clj_rt:equiv(to_list(ChunkedCons), clj_rt:seq(Y));
    false -> false
  end.

hash(#?TYPE{name = ?M, data = Cons}) ->
  clj_murmur3:ordered(Cons).

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, ?NIL).

with_meta(#?TYPE{name = ?M, info = Info} = List, Metadata) ->
  List#?TYPE{info = Info#{meta => Metadata}}.

reduce(#?TYPE{name = ?M} = ChunkedCons, F) ->
  case to_list(ChunkedCons) of
    [] -> clj_rt:apply(F, []);
    [X | Rest] -> do_reduce(F, X, Rest)
  end.

reduce(#?TYPE{name = ?M} = ChunkedCons, F, Init) ->
  do_reduce(F, Init, to_list(ChunkedCons)).

do_reduce(F, Acc, [Chunk | Items]) ->
  Val = clj_rt:apply(F, [Acc, Chunk]),
  case 'clojerl.Reduced':is_reduced(Val) of
    true  -> Val;
    false -> do_reduce(F, Val, Items)
  end;
do_reduce(_F, Acc, []) ->
  Acc.

first(#?TYPE{name = ?M, data = {Chunk, _}}) ->
  clj_rt:nth(Chunk, 0).

next(#?TYPE{name = ?M, data = {Chunk0, More}} = ChunkedCons) ->
  case clj_rt:count(Chunk0) > 1 of
    true ->
      Chunk1 = 'clojerl.IChunk':drop_first(Chunk0),
      'clojerl.ChunkedCons':?CONSTRUCTOR(Chunk1, More);
    false ->
      chunked_next(ChunkedCons)
  end.

more(#?TYPE{name = ?M, data = {Chunk0, More}} = ChunkedCons) ->
  case clj_rt:count(Chunk0) > 1 of
    true ->
      Chunk1 = 'clojerl.IChunk':drop_first(Chunk0),
      'clojerl.ChunkedCons':?CONSTRUCTOR(Chunk1, More);
    false  ->
      chunked_more(ChunkedCons)
  end.

'_'(_) -> ?NIL.

seq(#?TYPE{name = ?M} = Cons) -> Cons.

to_list(#?TYPE{name = ?M, data = {Chunk, More}}) ->
  Tail    = clj_rt:to_list(More),
  Count   = 'clojerl.TupleChunk':count(Chunk),
  Indexes = lists:seq(Count - 1, 0, -1),
  Cons    = fun(I, Acc) -> ['clojerl.TupleChunk':nth(Chunk, I) | Acc] end,
  lists:foldl(Cons, Tail, Indexes).

str(#?TYPE{name = ?M} = ChunkedCons) ->
  List = clj_rt:list(to_list(ChunkedCons)),
  clj_rt:print(List).
