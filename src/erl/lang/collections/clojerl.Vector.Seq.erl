%% @private
-module('clojerl.Vector.Seq').

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IEncodeErlang').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.IReduce').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

-export([?CONSTRUCTOR/1]).

-export([count/1]).
-export([ cons/2
        , empty/1
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
-type type() ::
        #{ ?TYPE  => ?M
         , vector => 'clojerl.IVector':type()
         , start  => non_neg_integer()
         , meta   => ?NIL | any()
         }.

-spec ?CONSTRUCTOR('clojerl.IVector':type()) ->
  type().
?CONSTRUCTOR(Vector) ->
  #{ ?TYPE     => ?M
   , vector    => Vector
   , start     => 0
   , meta      => ?NIL
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(#{?TYPE := ?M, vector := Vector, start := Start}) ->
  'clojerl.ICounted':count(Vector) - Start.

%% clojerl.IColl

cons(#{?TYPE := ?M} = Seq, X) ->
  'clojerl.Cons':?CONSTRUCTOR(X, Seq).

empty(_) -> clj_rt:list([]).

%% clojerl.IEquiv

equiv( #{?TYPE := ?M} = X
     , #{?TYPE := ?M} = Y
     ) ->
  case count(X) =:= count(Y) of
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
  case Recursive of
    true  -> [clj_rt:'clj->erl'(Item, true) || Item <- List0];
    false -> List0
  end.

%% clojerl.IHash

hash(#{?TYPE := ?M} = Seq) ->
  clj_murmur3:ordered(to_list(Seq)).

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = Seq, Meta) ->
  Seq#{meta => Meta}.

%% clojerl.IReduce

reduce(#{?TYPE := ?M, vector := Vector}, F) ->
  'clojerl.IReduce':reduce(Vector, F).

reduce(#{?TYPE := ?M, vector := Vector}, F, Init) ->
  'clojerl.IReduce':reduce(Vector, F, Init).

%% clojerl.ISeq

first(#{?TYPE := ?M, vector := Vector, start := Start}) ->
  'clojerl.ILookup':get(Vector, Start).

next(#{?TYPE := ?M, vector := Vector, start := Start} = Seq) ->
  case Start + 1 < 'clojerl.ICounted':count(Vector) of
    true  -> Seq#{start => Start + 1};
    false -> ?NIL
  end.

more(#{?TYPE := ?M} = Seq) ->
  case next(Seq) of
    ?NIL -> clj_rt:list([]);
    Next -> Next
  end.

%% clojerl.ISeqable

seq(#{?TYPE := ?M} = Seq) ->
  case count(Seq) > 0 of
    true  -> Seq;
    false -> ?NIL
  end.

to_list(#{?TYPE  := ?M, vector := Vector, start := Start}) ->
  [ 'clojerl.ILookup':get(Vector, Index)
    || Index <- lists:seq(Start, 'clojerl.ICounted':count(Vector) - 1)
  ].

%% clojerl.IStringable

str(#{?TYPE := ?M} = Seq) ->
  List = clj_rt:list(to_list(Seq)),
  clj_rt:print_str(List).
