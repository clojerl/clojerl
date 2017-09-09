-module('clojerl.Vector.ChunkedSeq').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IChunkedSeq').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
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

-type type() :: #{ ?TYPE => ?M
                 , array => array:array()
                 , index => non_neg_integer()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(array:array(), non_neg_integer()) -> type().
?CONSTRUCTOR(Array, Index) ->
  #{ ?TYPE => ?M
   , array => Array
   , index => Index
   , meta  => ?NIL
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(#{?TYPE := ?M, array := Array, index := Index}) ->
  array:size(Array) - Index.

cons(#{?TYPE := ?M} = ChunkedSeq, X) ->
  'clojerl.Cons':?CONSTRUCTOR(X, ChunkedSeq).

empty(_) -> clj_rt:list([]).

chunked_first(#{?TYPE := ?M, array := Array, index := Index}) ->
  End  = lists:min([array:size(Array), Index + ?CHUNK_SIZE]),
  List = [array:get(I, Array) || I <- lists:seq(Index, End - 1)],
  'clojerl.TupleChunk':?CONSTRUCTOR(list_to_tuple(List)).

chunked_next(#{?TYPE := ?M, array := Array, index := Index}) ->
  End = lists:min([array:size(Array), Index + ?CHUNK_SIZE]),
  case End < array:size(Array) of
    true  -> ?CONSTRUCTOR(Array, Index + ?CHUNK_SIZE);
    false -> ?NIL
  end.

chunked_more(#{?TYPE := ?M} = ChunkedSeq) ->
  case chunked_next(ChunkedSeq) of
    ?NIL -> clj_rt:list([]);
    Seq  -> Seq
  end.

equiv( #{?TYPE := ?M, array := X}
     , #{?TYPE := ?M, array := Y}
     ) ->
  case array:size(X) =:= array:size(Y) of
    true ->
      X1 = array:to_list(X),
      Y1 = array:to_list(Y),
      'erlang.List':equiv(X1, Y1);
    false -> false
  end;
equiv(#{?TYPE := ?M, array := X}, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> 'erlang.List':equiv(array:to_list(X), Y);
    false -> false
  end.

hash(#{?TYPE := ?M, array := Array}) ->
  clj_murmur3:ordered(array:to_list(Array)).

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = ChunkedSeq, Meta) ->
  ChunkedSeq#{meta => Meta}.

first(#{?TYPE := ?M, array := Array, index := Index}) ->
  array:get(Index, Array).

next(#{?TYPE := ?M, array := Array, index := Index}) ->
  case Index + 1 < array:size(Array) of
    true  -> ?CONSTRUCTOR(Array, Index + 1);
    false -> ?NIL
  end.

more(#{?TYPE := ?M} = ChunkedSeq) ->
  case next(ChunkedSeq) of
    ?NIL -> clj_rt:list([]);
    Seq  -> Seq
  end.

'_'(_) -> ?NIL.

seq(#{?TYPE := ?M, array := Array, index := Index}  = ChunkedSeq) ->
  case Index < array:size(Array) of
    true  -> ChunkedSeq;
    false -> ?NIL
  end.

to_list(#{?TYPE := ?M, array := Array, index := Index}) ->
  [array:get(I, Array) || I <- lists:seq(Index, array:size(Array) - 1)].

str(#{?TYPE := ?M} = ChunkedSeq) ->
  List = clj_rt:list(to_list(ChunkedSeq)),
  clj_rt:print(List).
