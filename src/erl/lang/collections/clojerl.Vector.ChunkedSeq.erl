%% @private
-module('clojerl.Vector.ChunkedSeq').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IChunkedSeq').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IEncodeErlang').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.IReduce').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

-export([?CONSTRUCTOR/3]).

-export([count/1]).
-export([ cons/2
        , empty/1
        ]).
-export([ chunked_first/1
        , chunked_next/1
        , chunked_more/1
        ]).
-export([equiv/2]).
-export(['clj->erl'/2]).
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
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

-export_type([type/0]).
-type type() :: #{ ?TYPE     => ?M
                 , vector    => clj_vector:vector()
                 , size      => non_neg_integer()
                 , index     => non_neg_integer()
                 , offset    => non_neg_integer()
                 , node      => tuple()
                 , node_size => non_neg_integer()
                 , meta      => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(clj_vector:vector(), non_neg_integer(), non_neg_integer()) ->
  type().
?CONSTRUCTOR(Vector, Index, Offset) ->
  Size = clj_vector:size(Vector),
  #{ ?TYPE     => ?M
   , vector    => Vector
   , size      => Size
   , index     => Index
   , offset    => Offset
   , node      => clj_vector:tuple_for(Index, Vector)
   , node_size => erlang:min(Size - Index, ?CHUNK_SIZE)
   , meta      => ?NIL
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(#{?TYPE := ?M, size := Size, index := Index, offset := Offset}) ->
  Size - (Index + Offset).

%% clojerl.IColl

cons(#{?TYPE := ?M} = ChunkedSeq, X) ->
  'clojerl.Cons':?CONSTRUCTOR(X, ChunkedSeq).

empty(_) -> clj_rt:list([]).

%% clojerl.IChunkedSeq

chunked_first( #{ ?TYPE     := ?M
                , offset    := Offset
                , node      := Node
                , node_size := NodeSize
                }
             ) ->
  'clojerl.TupleChunk':?CONSTRUCTOR( Node
                                   , Offset
                                   , NodeSize
                                   ).

chunked_next( #{ ?TYPE     := ?M
               , vector    := Vector
               , size      := Size
               , index     := Index
               , node_size := NodeSize
               } = ChunkedSeq
            ) ->
  case Index + NodeSize < Size of
    true ->
      NewIndex = Index + ?CHUNK_SIZE,
      ChunkedSeq#{ index     => NewIndex
                 , offset    => 0
                 , node      => clj_vector:tuple_for(NewIndex, Vector)
                 , node_size => erlang:min(Size - NewIndex, ?CHUNK_SIZE)
                 };
    false -> ?NIL
  end.

chunked_more(#{?TYPE := ?M} = ChunkedSeq) ->
  case chunked_next(ChunkedSeq) of
    ?NIL -> clj_rt:list([]);
    Seq  -> Seq
  end.

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, vector := X, size := SizeX}
     , #{?TYPE := ?M, vector := Y, size := SizeY}
     ) ->
  case SizeX =:= SizeY of
    true ->
      X1 = clj_vector:to_list(X),
      Y1 = clj_vector:to_list(Y),
      'erlang.List':equiv(X1, Y1);
    false -> false
  end;
equiv(#{?TYPE := ?M} = X, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> 'erlang.List':equiv(to_list(X), Y);
    false -> false
  end.

%% clojerl.IEncodeErlang

'clj->erl'(#{?TYPE := ?M} = X, Recursive) ->
  List0 = to_list(X),
  List1 = case Recursive of
            true  -> [clj_rt:'clj->erl'(Item, true) || Item <- List0];
            false -> List0
          end,
  list_to_tuple(List1).

%% clojerl.IHash

hash(#{?TYPE := ?M, vector := Vector}) ->
  clj_murmur3:ordered(clj_vector:to_list(Vector)).

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
      Next = chunked_next(ChunkedSeq),
      case Next of
        ?NIL        -> Val;
        ChunkedNext -> reduce(ChunkedNext, F, Val)
      end
  end.

%% clojerl.ISeq

first(#{?TYPE := ?M, node := Node, offset := Offset}) ->
  element(Offset + 1, Node).

next( #{ ?TYPE     := ?M
       , offset    := Offset
       , node_size := NodeSize
       } = ChunkedSeq
    ) ->
  NewOffset = Offset + 1,
  case NewOffset < NodeSize of
    true  -> ChunkedSeq#{offset => NewOffset};
    false -> chunked_next(ChunkedSeq)
  end.

more(#{?TYPE := ?M} = ChunkedSeq) ->
  case next(ChunkedSeq) of
    ?NIL -> clj_rt:list([]);
    Seq  -> Seq
  end.

%% clojerl.ISeqable

seq(#{ ?TYPE  := ?M
     , size   := Size
     , index  := Index
     , offset := Offset
     } = ChunkedSeq) ->
  case Index + Offset < Size of
    true  -> ChunkedSeq;
    false -> ?NIL
  end.

to_list(#{ ?TYPE  := ?M
         , vector := Vector
         , size   := Size
         , index  := Index
         , offset := Offset
         }
       ) ->
  [clj_vector:get(I, Vector) || I <- lists:seq(Index + Offset, Size - 1)].

%% clojerl.IStringable

str(#{?TYPE := ?M} = ChunkedSeq) ->
  List = clj_rt:list(to_list(ChunkedSeq)),
  clj_rt:print_str(List).
