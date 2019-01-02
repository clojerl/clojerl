-module('clojerl.ChunkedCons').

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IChunkedSeq').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IErl').
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
-export(['->erl'/2]).
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

-type type() :: #{ ?TYPE => ?M
                 , chunk => 'clojerl.TupleChunk':type()
                 , more  => any()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(any(), any()) -> type().
?CONSTRUCTOR(Chunk, More) ->
  #{ ?TYPE => ?M
   , chunk => Chunk
   , more  => More
   , meta  => ?NIL
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(#{?TYPE := ?M, chunk := Chunk, more := More}) ->
  'clojerl.TupleChunk':count(Chunk) + clj_rt:count(More).

%% clojerl.IColl

cons(#{?TYPE := ?M} = ChunkedCons, X) ->
  clj_rt:cons(X, ChunkedCons).

empty(_) -> [].

%% clojerl.IChunkedSeq

chunked_first(#{?TYPE := ?M, chunk := Chunk}) ->
  Chunk.

chunked_next(#{?TYPE := ?M} = ChunkedCons) ->
  clj_rt:seq(chunked_more(ChunkedCons)).

chunked_more(#{?TYPE := ?M, more := ?NIL}) -> [];
chunked_more(#{?TYPE := ?M, more := More}) -> More.

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, chunk := ChunkX, more := MoreX}
     , #{?TYPE := ?M, chunk := ChunkY, more := MoreY}
     ) ->
  'clojerl.TupleChunk':equiv(ChunkX, ChunkY) andalso clj_rt:equiv(MoreX, MoreY);
equiv(#{?TYPE := ?M} = ChunkedCons, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> 'erlang.List':equiv(to_list(ChunkedCons), clj_rt:seq(Y));
    false -> false
  end.

%% clojerl.IErl

'->erl'(#{?TYPE := ?M} = X, Recursive) ->
  List = to_list(X),
  case Recursive of
    true  -> [clj_rt:'->erl'(Item, true) || Item <- List];
    false -> List
  end.

%% clojerl.IHash

hash(#{?TYPE := ?M, chunk := Chunk, more := More}) ->
  clj_murmur3:ordered({Chunk, More}).

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = List, Metadata) ->
  List#{meta => Metadata}.

%% clojerl.IReduce

reduce(#{?TYPE := ?M} = ChunkedCons, F) ->
  case to_list(ChunkedCons) of
    [] -> clj_rt:apply(F, []);
    [X | Rest] -> do_reduce(F, X, Rest)
  end.

reduce(#{?TYPE := ?M} = ChunkedCons, F, Init) ->
  do_reduce(F, Init, to_list(ChunkedCons)).

do_reduce(F, Acc, [Chunk | Items]) ->
  Val = clj_rt:apply(F, [Acc, Chunk]),
  case 'clojerl.Reduced':is_reduced(Val) of
    true  -> 'clojerl.Reduced':deref(Val);
    false -> do_reduce(F, Val, Items)
  end;
do_reduce(_F, Acc, []) ->
  Acc.

%% clojerl.ISeq

first(#{?TYPE := ?M, chunk := Chunk}) ->
  'clojerl.TupleChunk':nth(Chunk, 0).

next(#{?TYPE := ?M, chunk := Chunk0, more := More} = ChunkedCons) ->
  case 'clojerl.TupleChunk':count(Chunk0) > 1 of
    true ->
      Chunk1 = 'clojerl.TupleChunk':drop_first(Chunk0),
      'clojerl.ChunkedCons':?CONSTRUCTOR(Chunk1, More);
    false ->
      chunked_next(ChunkedCons)
  end.

more(#{?TYPE := ?M, chunk := Chunk0, more := More} = ChunkedCons) ->
  case 'clojerl.TupleChunk':count(Chunk0) > 1 of
    true ->
      Chunk1 = 'clojerl.TupleChunk':drop_first(Chunk0),
      'clojerl.ChunkedCons':?CONSTRUCTOR(Chunk1, More);
    false  ->
      chunked_more(ChunkedCons)
  end.

%% clojerl.ISequential

'_'(_) -> ?NIL.

%% clojerl.ISeqable

seq(#{?TYPE := ?M} = Cons) -> Cons.

to_list(#{?TYPE := ?M, chunk := Chunk, more := More}) ->
  Tail    = clj_rt:to_list(More),
  Count   = 'clojerl.TupleChunk':count(Chunk),
  Indexes = lists:seq(Count - 1, 0, -1),
  Cons    = fun(I, Acc) -> ['clojerl.TupleChunk':nth(Chunk, I) | Acc] end,
  lists:foldl(Cons, Tail, Indexes).

%% clojerl.IStringable

str(#{?TYPE := ?M} = ChunkedCons) ->
  List = clj_rt:list(to_list(ChunkedCons)),
  clj_rt:print_str(List).
