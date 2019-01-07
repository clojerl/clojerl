-module('clojerl.Vector.ChunkedSeq').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

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
        , chunked_next/1
        , chunked_more/1
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
                 , array => array:array()
                 , size  => non_neg_integer()
                 , index => non_neg_integer()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(array:array(), non_neg_integer()) -> type().
?CONSTRUCTOR(Array, Index) ->
  #{ ?TYPE => ?M
   , array => Array
   , size  => array:size(Array)
   , index => Index
   , meta  => ?NIL
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(#{?TYPE := ?M, size := Size, index := Index}) ->
  Size - Index.

%% clojerl.IColl

cons(#{?TYPE := ?M} = ChunkedSeq, X) ->
  'clojerl.Cons':?CONSTRUCTOR(X, ChunkedSeq).

empty(_) -> clj_rt:list([]).

%% clojerl.IChunkedSeq

chunked_first(#{?TYPE := ?M, array := Array, size := Size, index := Index}) ->
  End  = erlang:min(Size, Index + ?CHUNK_SIZE),
  List = [array:get(I, Array) || I <- lists:seq(Index, End - 1)],
  'clojerl.TupleChunk':?CONSTRUCTOR(list_to_tuple(List)).

chunked_next(#{?TYPE := ?M, array := Array, size := Size, index := Index}) ->
  End = erlang:min(Size, Index + ?CHUNK_SIZE),
  case End < Size of
    true  -> ?CONSTRUCTOR(Array, Index + ?CHUNK_SIZE);
    false -> ?NIL
  end.

chunked_more(#{?TYPE := ?M} = ChunkedSeq) ->
  case chunked_next(ChunkedSeq) of
    ?NIL -> clj_rt:list([]);
    Seq  -> Seq
  end.

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, array := X, size := SizeX}
     , #{?TYPE := ?M, array := Y, size := SizeY}
     ) ->
  case SizeX =:= SizeY of
    true ->
      X1 = array:to_list(X),
      Y1 = array:to_list(Y),
      'erlang.List':equiv(X1, Y1);
    false -> false
  end;
equiv(#{?TYPE := ?M} = X, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> 'erlang.List':equiv(to_list(X), Y);
    false -> false
  end.

%% clojerl.IErl

'->erl'(#{?TYPE := ?M} = X, Recursive) ->
  List0 = to_list(X),
  List1 = case Recursive of
            true  -> [clj_rt:'->erl'(Item, true) || Item <- List0];
            false -> List0
          end,
  list_to_tuple(List1).

%% clojerl.IHash

hash(#{?TYPE := ?M, array := Array}) ->
  clj_murmur3:ordered(array:to_list(Array)).

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = ChunkedSeq, Meta) ->
  ChunkedSeq#{meta => Meta}.

%% clojerl.IReduce

reduce(#{?TYPE := ?M} = ChunkedSeq, F) ->
  case seq(ChunkedSeq) of
    ?NIL ->
      clj_rt:apply(F, []);
    #{?TYPE := ?M} ->
      First = first(ChunkedSeq),
      Next  = next(ChunkedSeq),
      reduce(Next, F, First)
  end.

reduce(#{?TYPE := ?M} = ChunkedSeq, F, Init) ->
  case seq(ChunkedSeq) of
    ?NIL           -> Init;
    #{?TYPE := ?M} -> do_reduce(ChunkedSeq, F, Init)
  end.

do_reduce(ChunkedSeq, F, Init) ->
  ChunkFirst = chunked_first(ChunkedSeq),
  Val        = 'clojerl.TupleChunk':reduce(ChunkFirst, F, Init),
  case 'clojerl.Reduced':is_reduced(Val) of
    true  -> 'clojerl.Reduced':deref(Val);
    false ->
      case chunked_next(ChunkedSeq) of
        ?NIL        -> Val;
        ChunkedNext -> reduce(ChunkedNext, F, Val)
      end
  end.

%% clojerl.ISeq

first(#{?TYPE := ?M, array := Array, index := Index}) ->
  array:get(Index, Array).

next(#{?TYPE := ?M, array := Array, size := Size, index := Index}) ->
  case Index + 1 < Size of
    true  -> ?CONSTRUCTOR(Array, Index + 1);
    false -> ?NIL
  end.

more(#{?TYPE := ?M} = ChunkedSeq) ->
  case next(ChunkedSeq) of
    ?NIL -> clj_rt:list([]);
    Seq  -> Seq
  end.

%% clojerl.ISequential

'_'(_) -> ?NIL.

%% clojerl.ISeqable

seq(#{ ?TYPE := ?M
     , size  := Size
     , index := Index
     } = ChunkedSeq) ->
  case Index < Size of
    true  -> ChunkedSeq;
    false -> ?NIL
  end.

to_list(#{?TYPE := ?M, array := Array, size := Size, index := Index}) ->
  [array:get(I, Array) || I <- lists:seq(Index, Size - 1)].

%% clojerl.IStringable

str(#{?TYPE := ?M} = ChunkedSeq) ->
  List = clj_rt:list(to_list(ChunkedSeq)),
  clj_rt:print_str(List).
