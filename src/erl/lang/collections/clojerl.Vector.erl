-module('clojerl.Vector').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behavior('clojerl.IAssociative').
-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IErl').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.ILookup').
-behavior('clojerl.IMeta').
-behavior('clojerl.IReduce').
-behavior('clojerl.IReversible').
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
-export(['->erl'/2]).
-export([apply/2]).
-export([hash/1]).
-export([ get/2
        , get/3
        ]).
-export([ meta/1
        , with_meta/2
        ]).
-export([rseq/1]).
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
                 , array => clj_vector:vector()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(Items) when is_list(Items) ->
  #{ ?TYPE => ?M
   , array => clj_vector:new(Items)
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
  is_valid_index(Array, Index).

entry_at(#{?TYPE := ?M, array := Array}, Index) ->
  case is_valid_index(Array, Index) of
    true  -> ?CONSTRUCTOR([Index, clj_vector:get(Index, Array)]);
    false -> ?NIL
  end.

assoc(#{?TYPE := ?M, array := Array} = Vector, Index, Value) ->
  case is_valid_index(Array, Index) orelse Index == clj_vector:size(Array) of
    true  -> Vector#{array => clj_vector:set(Index, Value, Array)};
    false -> ?ERROR(<<"Index out of bounds">>)
  end.

%% clojerl.ICounted

count(#{?TYPE := ?M, array := Array}) -> clj_vector:size(Array).

%% clojerl.IColl

cons(#{?TYPE := ?M, array := Array} = Vector, X) ->
  Vector#{array => clj_vector:cons(X, Array)}.

empty(_) -> ?CONSTRUCTOR([]).

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, array := X}
     , #{?TYPE := ?M, array := Y}
     ) ->
  case clj_vector:size(X) == clj_vector:size(Y) of
    true ->
      X1 = clj_vector:to_list(X),
      Y1 = clj_vector:to_list(Y),
      'erlang.List':equiv(X1, Y1);
    false -> false
  end;
equiv(#{?TYPE := ?M, array := X}, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> 'erlang.List':equiv(clj_vector:to_list(X), Y);
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

%% clojerl.IFn

apply(#{?TYPE := ?M, array := Array}, [Index]) when is_integer(Index) ->
  ?ERROR_WHEN(not is_valid_index(Array, Index), <<"Index out of bounds">>),
  clj_vector:get(Index, Array);
apply(#{?TYPE := ?M}, [_]) ->
  ?ERROR(<<"Key must be integer">>);
apply(#{?TYPE := ?M}, Args) ->
  CountBin = integer_to_binary(length(Args)),
  ?ERROR(<<"Wrong number of args for vector, got: ", CountBin/binary>>).

%% clojerl.IHash

hash(#{?TYPE := ?M, array := Array}) ->
  clj_murmur3:ordered(clj_vector:to_list(Array)).

%% clojerl.ILookup

get(#{?TYPE := ?M} = Vector, Index) ->
  get(Vector, Index, ?NIL).

get(#{?TYPE := ?M, array := Array}, Index, NotFound) ->
  case is_valid_index(Array, Index) of
    true  -> clj_vector:get(Index, Array);
    false -> NotFound
  end.

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = Vector, Meta) ->
  Vector#{meta => Meta}.

%% clojerl.IReduce

reduce(#{?TYPE := ?M, array := Array}, F) ->
  clj_vector:reduce(F, Array).

reduce(#{?TYPE := ?M, array := Array}, F, Init) ->
  clj_vector:reduce(F, Init, Array).

%% clojerl.IReduce

rseq(#{?TYPE := ?M, array := Array}) ->
  case clj_vector:size(Array) of
    0 -> ?NIL;
    _ -> 'clojerl.Vector.RSeq':?CONSTRUCTOR(Array, clj_vector:size(Array) - 1)
  end.

%% clojerl.ISequential

'_'(_) -> ?NIL.

%% clojerl.IIndexed

nth(#{?TYPE := ?M, array := Array}, N) ->
  case is_valid_index(Array, N) of
    true  -> clj_vector:get(N, Array);
    false -> error(badarg)
  end.

nth(#{?TYPE := ?M, array := Array}, N, NotFound) ->
  case is_valid_index(Array, N) of
    true  -> clj_vector:get(N, Array);
    false -> NotFound
  end.

%% clojerl.IStack

peek(#{?TYPE := ?M, array := Array}) ->
  case clj_vector:size(Array) of
    0    -> ?NIL;
    Size -> clj_vector:get(Size - 1, Array)
  end.

pop(#{?TYPE := ?M, array := Array} = Vector) ->
  Vector#{array => clj_vector:pop(Array)}.

%% clojerl.ISeqable

seq(#{?TYPE := ?M, array := Array}) ->
  case clj_vector:size(Array) of
    0 -> ?NIL;
    Size when Size =< ?CHUNK_SIZE -> clj_vector:to_list(Array);
    _ -> 'clojerl.Vector.ChunkedSeq':?CONSTRUCTOR(Array, 0, 0)
  end.

to_list(#{?TYPE := ?M, array := Array}) ->
  clj_vector:to_list(Array).

%% clojerl.IStringable

str(#{?TYPE := ?M} = Vector) ->
  clj_rt:print_str(Vector).

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

is_valid_index(Array, Index) ->
  is_integer(Index) andalso Index >= 0 andalso Index < clj_vector:size(Array).
