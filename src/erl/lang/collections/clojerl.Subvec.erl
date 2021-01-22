%% @doc Subvector implementation.

%% @private
-module('clojerl.Subvec').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behavior('clojerl.IAssociative').
-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IEncodeErlang').
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
-behavior('clojerl.IVector').

-export([?CONSTRUCTOR/3]).
-export([ contains_key/2
        , entry_at/2
        , assoc/3
        ]).
-export([count/1]).
-export([ cons/2
        , empty/1
        ]).
-export([equiv/2]).
-export(['clj->erl'/2]).
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

-type type() :: #{ ?TYPE  => ?M
                 , vector => 'clojerl.IVector':type()
                 , start  => integer()
                 , 'end'  => integer()
                 , meta   => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR('clojerl.IVector':type(), integer(), integer()) -> type().
?CONSTRUCTOR(Vector, Start, End) ->
  #{ ?TYPE  => ?M
   , vector => Vector
   , start  => Start
   , 'end'  => End
   , meta   => ?NIL
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IAssociative

contains_key(#{?TYPE := ?M, start := Start, 'end' := End}, Index) ->
  is_valid_index(Start, End, Index).

entry_at( #{?TYPE := ?M, vector := Vector, start := Start, 'end' := End}
        , Index
        ) ->
  case is_valid_index(Start, End, Index) of
    true  ->
      Value = 'clojerl.ILookup':get(Vector, Start + Index),
      'clojerl.Vector':?CONSTRUCTOR([Index, Value]);
    false ->
      ?NIL
  end.

assoc( #{ ?TYPE := ?M, vector := Vector
        , start := Start, 'end' := End
        } = Subvec
     , Index
     , Value
     ) ->
  ?ERROR_WHEN(Start + Index > End, <<"Index out of bounds">>),
  case Start + Index of
    End -> cons(Subvec, Value);
    _   -> Subvec#{vector => clj_rt:assoc(Vector, Start + Index, Value)}
  end.

%% clojerl.ICounted

count(#{?TYPE := ?M, start := Start, 'end' := End}) -> End - Start.

%% clojerl.IColl

cons(#{?TYPE := ?M, vector := Vector, 'end' := End} = Subvec, X) ->
  Subvec#{ vector => clj_rt:assoc(Vector, End, X)
            , 'end' => End + 1
            }.

empty(_) -> 'clojerl.Vector':?CONSTRUCTOR([]).

%% clojerl.IEquiv

equiv( #{?TYPE := ?M} = X
     , #{?TYPE := ?M} = Y
     ) ->
  case count(X) == count(Y) of
    true ->
      X1 = to_list(X),
      Y1 = to_list(Y),
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

%% clojerl.IFn

apply(#{?TYPE := ?M, vector := Vector, start := Start, 'end' := End}, [Index])
  when is_integer(Index) ->
  ?ERROR_WHEN(not is_valid_index(Start, End, Index), <<"Index out of bounds">>),
  'clojerl.ILookup':get(Vector, Start + Index);
apply(#{?TYPE := ?M}, [_]) ->
  ?ERROR(<<"Key must be integer">>);
apply(#{?TYPE := ?M}, Args) ->
  CountBin = integer_to_binary(length(Args)),
  ?ERROR(<<"Wrong number of args for vector, got: ", CountBin/binary>>).

%% clojerl.IHash

hash(#{?TYPE := ?M} = Subvec) ->
  clj_murmur3:ordered(to_list(Subvec)).

%% clojerl.ILookup

get(#{?TYPE := ?M} = Subvec, Index) ->
  get(Subvec, Index, ?NIL).

get( #{?TYPE := ?M, vector := Vector, start := Start, 'end' := End}
   , Index
   , NotFound
   ) ->
  case is_valid_index(Start, End, Index) of
    true  -> 'clojerl.ILookup':get(Vector, Start + Index);
    false -> NotFound
  end.

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = Subvec, Meta) ->
  Subvec#{meta => Meta}.

%% clojerl.IReduce

reduce(#{?TYPE := ?M, start := Start, 'end' := End}, F) when Start =:= End ->
  'clojerl.IFn':apply(F, []);
reduce(#{?TYPE := ?M, vector := Vector, start := Start, 'end' := End}, F) ->
  Init = 'clojerl.ILookup':get(Vector, Start),
  do_reduce(Vector, Start + 1, End, F, Init).

reduce( #{?TYPE := ?M, vector := Vector, start := Start, 'end' := End}
      , F
      , Init
      ) ->
  do_reduce(Vector, Start, End, F, Init).

do_reduce(_Vector, Current, End, _F, Acc) when Current =:= End ->
  Acc;
do_reduce(Vector, Current, End, F, Acc0) ->
  Value = 'clojerl.ILookup':get(Vector, Current),
  Acc1 = 'clojerl.IFn':apply(F, [Acc0, Value]),
  case 'clojerl.Reduced':is_reduced(Acc1) of
    true  -> 'clojerl.Reduced':deref(Acc1);
    false -> do_reduce(Vector, Current + 1, End, F, Acc1)
  end.

%% clojerl.IReversible

rseq(#{?TYPE := ?M} = Subvec) ->
  case count(Subvec) of
    0 -> ?NIL;
    Count -> 'clojerl.Vector.RSeq':?CONSTRUCTOR(Subvec, Count - 1)
  end.

%% clojerl.IIndexed

nth(#{?TYPE := ?M, vector := Vector, start := Start, 'end' := End}, Index) ->
  case is_valid_index(Start, End, Index) of
    true  -> 'clojerl.ILookup':get(Vector, Start + Index);
    false -> error(badarg)
  end.

nth( #{?TYPE := ?M, vector := Vector, start := Start, 'end' := End}
   , Index
   , NotFound
   ) ->
  case is_valid_index(Start, End, Index) of
    true  -> 'clojerl.ILookup':get(Vector, Start + Index);
    false -> NotFound
  end.

%% clojerl.IStack

peek(#{?TYPE := ?M, vector := Vector, start := Start, 'end' := End}) ->
  case End - Start of
    0 -> ?NIL;
    _ -> 'clojerl.ILookup':get(Vector, End - 1)
  end.

pop(#{?TYPE := ?M, start := Start, 'end' := End}) when End - 1 =:= Start ->
  'clojerl.Vector':?CONSTRUCTOR([]);
pop(#{?TYPE := ?M, 'end' := End} = Subvec) ->
  Subvec#{'end' := End - 1}.

%% clojerl.ISeqable

seq(#{?TYPE := ?M, start := Start, 'end' := End} = Subvec) ->
  case End - Start of
    0 -> ?NIL;
    _ -> 'clojerl.Vector.Seq':?CONSTRUCTOR(Subvec)
  end.

to_list(#{?TYPE := ?M} = X) ->
  clj_rt:to_list(seq(X)).

%% clojerl.IStringable

str(#{?TYPE := ?M} = Subvec) ->
  clj_rt:print_str(Subvec).

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

is_valid_index(Start, End, Index) ->
  is_integer(Index) andalso Index >= 0 andalso Index < End - Start.
