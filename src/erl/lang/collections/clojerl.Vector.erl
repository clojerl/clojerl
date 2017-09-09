-module('clojerl.Vector').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behavior('clojerl.IAssociative').
-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.ILookup').
-behavior('clojerl.IMeta').
-behavior('clojerl.IReduce').
-behavior('clojerl.IIndexed').
-behavior('clojerl.ISequential').
-behavior('clojerl.IStack').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

-export([?CONSTRUCTOR/1, subvec/3]).
-export([ contains_key/2
        , entry_at/2
        , assoc/3
        ]).
-export([count/1]).
-export([ cons/2
        , empty/1
        ]).
-export([equiv/2]).
-export([apply/2]).
-export([hash/1]).
-export([ get/2
        , get/3
        ]).
-export([ meta/1
        , with_meta/2
        ]).
-export([ reduce/2
        , reduce/3
        ]).
-export(['_'/1]).
-export([ nth/2
        , nth/3
        ]).
-export([ peek/1
        , pop/1
        ]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

-type type() :: #{ ?TYPE => ?M
                 , array => array:array()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(Items) when is_list(Items) ->
  #{ ?TYPE => ?M
   , array => array:from_list(Items, ?NIL)
   , meta  => ?NIL
   }.

-spec subvec(type(), integer(), integer()) -> type().
subvec(Vector, Start, End) ->
  AddItemAtFun =
    fun(Index, Subvec) ->
        cons(Subvec, nth(Vector, Index))
    end,
  lists:foldl(AddItemAtFun, ?CONSTRUCTOR([]), lists:seq(Start, End - 1)).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IAssociative

contains_key(#{?TYPE := ?M, array := Array}, Index) ->
  Index < array:size(Array).

entry_at(#{?TYPE := ?M, array := Array}, Index) ->
  array:get(Index, Array).

assoc(#{?TYPE := ?M, array := Array} = Vector, Index, Value) ->
  case  Index =< array:size(Array) of
    true  -> Vector#{array => array:set(Index, Value, Array)};
    false -> error(<<"Index out of bounds">>)
  end.

count(#{?TYPE := ?M, array := Array}) -> array:size(Array).

cons(#{?TYPE := ?M, array := Array} = Vector, X) ->
  NewArray = array:set(array:size(Array), X, Array),
  Vector#{array => NewArray}.

empty(_) -> ?CONSTRUCTOR([]).

equiv( #{?TYPE := ?M, array := X}
     , #{?TYPE := ?M, array := Y}
     ) ->
  case array:size(X) == array:size(Y) of
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

apply(#{?TYPE := ?M, array := Array}, [Index]) ->
  array:get(Index, Array);
apply(#{?TYPE := ?M}, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for vector, got: ", CountBin/binary>>).

hash(#{?TYPE := ?M, array := Array}) ->
  clj_murmur3:ordered(array:to_list(Array)).

%% clojerl.ILookup

get(#{?TYPE := ?M} = Vector, Index) ->
  get(Vector, Index, ?NIL).

get(#{?TYPE := ?M, array := Array}, Index, NotFound) ->
  case Index < array:size(Array) of
    true  -> array:get(Index, Array);
    false -> NotFound
  end.

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = Vector, Meta) ->
  Vector#{meta => Meta}.

%% clojerl.IReduce

reduce(#{?TYPE := ?M, array := Array}, F) ->
  case array:size(Array) of
    0    -> clj_rt:apply(F, []);
    Size ->
      Init = array:get(0, Array),
      do_reduce(F, Init, 1, Size, Array)
  end.

reduce(#{?TYPE := ?M, array := Array}, F, Init) ->
  Fold = fun(_, Item, Acc) ->
             clj_rt:apply(F, [Acc, Item])
         end,
  array:foldl(Fold, Init, Array).

do_reduce(F, Acc, Index, Size, Array) when Index < Size ->
  Val = clj_rt:apply(F, [Acc, array:get(Index, Array)]),
  case 'clojerl.Reduced':is_reduced(Val) of
    true  -> Val;
    false -> do_reduce(F, Val, Index + 1, Size, Array)
  end;
do_reduce(_F, Acc, _Index, _Size, _Array) ->
  Acc.

%% clojerl.ISequential

'_'(_) -> ?NIL.

nth(#{?TYPE := ?M, array := Array}, N) ->
  case N < array:size(Array) of
    true  -> array:get(N, Array);
    false -> error(badarg)
  end.

nth(#{?TYPE := ?M, array := Array}, N, NotFound) ->
  case N < array:size(Array) of
    true  -> array:get(N, Array);
    false -> NotFound
  end.

peek(#{?TYPE := ?M, array := Array}) ->
  case array:size(Array) of
    0    -> ?NIL;
    Size -> array:get(Size - 1, Array)
  end.

pop(#{?TYPE := ?M, array := Array} = Vector) ->
  case array:size(Array) of
    0    -> error(<<"Can't pop empty vector">>);
    Size ->
      NewArray = array:resize(Size - 1, Array),
      Vector#{array => NewArray}
  end.

seq(#{?TYPE := ?M, array := Array}) ->
  case array:size(Array) of
    0 -> ?NIL;
    Size when Size =< ?CHUNK_SIZE -> array:to_list(Array);
    _ -> 'clojerl.Vector.ChunkedSeq':?CONSTRUCTOR(Array, 0)
  end.

to_list(#{?TYPE := ?M, array := Array}) ->
  array:to_list(Array).

str(#{?TYPE := ?M} = Vector) ->
  clj_rt:print(Vector).
