-module('clojerl.Vector.ChunkedSeq').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IChunkedSeq').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([?CONSTRUCTOR/2]).

-export([count/1]).
-export([ cons/2
        , empty/1
        ]).
-export([ chunked_first/1
        , chunked_next/1
        , chunked_more/1
        ]).
-export([equiv/2]).
-export([hash/1]).
-export([ meta/1
        , with_meta/2
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

-spec ?CONSTRUCTOR(array:array(), non_neg_integer()) -> type().
?CONSTRUCTOR(Array, Index) ->
  #?TYPE{data = {Array, Index}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(#?TYPE{name = ?M, data = {Array, Index}}) ->
  array:size(Array) - Index.

cons(#?TYPE{name = ?M} = ChunkedSeq, X) ->
  clj_rt:cons(X, ChunkedSeq).

empty(_) -> clj_rt:list([]).

chunked_first(#?TYPE{name = ?M, data = {Array, Index}}) ->
  End  = lists:min([array:size(Array), Index + ?CHUNK_SIZE]),
  List = [array:get(I, Array) || I <- lists:seq(Index, End - 1)],
  'clojerl.TupleChunk':?CONSTRUCTOR(list_to_tuple(List)).

chunked_next(#?TYPE{name = ?M, data = {Array, Index}}) ->
  case Index < array:size(Array) of
    true  -> ?CONSTRUCTOR(Array, Index + ?CHUNK_SIZE);
    false -> ?NIL
  end.

chunked_more(#?TYPE{name = ?M} = ChunkedSeq) ->
  case chunked_next(ChunkedSeq) of
    ?NIL -> clj_rt:list([]);
    Seq  -> Seq
  end.

equiv( #?TYPE{name = ?M, data = {X, _}}
     , #?TYPE{name = ?M, data = {Y, _}}
     ) ->
  case array:size(X) == array:size(Y) of
    true ->
      X1 = array:to_list(X),
      Y1 = array:to_list(Y),
      clj_rt:equiv(X1, Y1);
    false -> false
  end;
equiv(#?TYPE{name = ?M, data = {X, _}}, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> clj_rt:equiv(array:to_list(X), Y);
    false -> false
  end.

hash(#?TYPE{name = ?M, data = {Array, _}}) ->
  clj_murmur3:ordered(array:to_list(Array)).

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, ?NIL).

with_meta(#?TYPE{name = ?M, info = Info} = ChunkedSeq, Metadata) ->
  ChunkedSeq#?TYPE{info = Info#{meta => Metadata}}.

first(#?TYPE{name = ?M, data = {Array, Index}}) ->
  array:get(Index, Array).

next(#?TYPE{name = ?M, data = {Array, Index}}) ->
  case Index + 1 < array:size(Array) of
    true  -> ?CONSTRUCTOR(Array, Index + 1);
    false -> ?NIL
  end.

more(#?TYPE{name = ?M} = ChunkedSeq) ->
  case next(ChunkedSeq) of
    ?NIL -> clj_rt:list([]);
    Seq  -> Seq
  end.

'_'(_) -> ?NIL.

seq(#?TYPE{name = ?M, data = {Array, Index}}  = ChunkedSeq) ->
  case Index < array:size(Array) of
    true  -> ChunkedSeq;
    false -> ?NIL
  end.

to_list(#?TYPE{name = ?M, data = {Array, Index}}) ->
  [array:get(I, Array) || I <- lists:seq(Index, array:size(Array) - 1)].

str(#?TYPE{name = ?M} = ChunkedSeq) ->
  clj_rt:print(to_list(ChunkedSeq)).
