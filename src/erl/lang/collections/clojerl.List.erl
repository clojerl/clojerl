-module('clojerl.List').

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IErl').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.IReduce').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.IStack').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

-export([?CONSTRUCTOR/1]).

-export([count/1]).
-export([ cons/2
        , empty/1
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
-export([ peek/1
        , pop/1
        ]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

-type type() :: #{ ?TYPE => ?M
                 , items => list()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(Items) when is_list(Items) ->
  #{?TYPE => ?M, items => Items, meta => ?NIL};
?CONSTRUCTOR(?NIL) ->
  ?CONSTRUCTOR([]);
?CONSTRUCTOR(Items) ->
  ?CONSTRUCTOR(clj_rt:to_list(Items)).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(#{?TYPE := ?M, items := Items}) -> length(Items).

%% clojerl.IColl

cons(#{?TYPE := ?M, items := []} = List, X) ->
  List#{items => [X]};
cons(#{?TYPE := ?M, items := Items} = List, X) ->
  List#{items => [X | Items]}.

empty(_) -> ?CONSTRUCTOR([]).

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, items := X}
     , #{?TYPE := ?M, items := Y}
     ) ->
  clj_rt:equiv(X, Y);
equiv(#{?TYPE := ?M, items := X}, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> 'erlang.List':equiv(X, Y);
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

hash(#{?TYPE := ?M, items := X}) ->
  clj_murmur3:ordered(X).

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = List, Metadata) ->
  List#{meta => Metadata}.

%% clojerl.IReduce

reduce(#{?TYPE := ?M, items := []}, F) ->
  clj_rt:apply(F, []);
reduce(#{?TYPE := ?M, items := [First | Rest]}, F) ->
  do_reduce(F, First, Rest).

reduce(#{?TYPE := ?M, items := Items}, F, Init) ->
  do_reduce(F, Init, Items).

do_reduce(F, Acc, [First | Items]) ->
  Val = clj_rt:apply(F, [Acc, First]),
  case 'clojerl.Reduced':is_reduced(Val) of
    true  -> Val;
    false -> do_reduce(F, Val, Items)
  end;
do_reduce(_F, Acc, []) ->
  Acc.

%% clojerl.ISeq

first(#{?TYPE := ?M, items := []}) -> ?NIL;
first(#{?TYPE := ?M, items := [First | _]}) -> First.

next(#{?TYPE := ?M, items := []}) -> ?NIL;
next(#{?TYPE := ?M, items := [_ | []]}) -> ?NIL;
next(#{?TYPE := ?M, items := [_ | Rest]} = List) ->
  List#{items => Rest}.

more(#{?TYPE := ?M, items := []}) -> ?NIL;
more(#{?TYPE := ?M, items := [_ | Rest]} = List) ->
  List#{items => Rest}.

%% clojerl.ISequential

'_'(_) -> ?NIL.

%% clojerl.IStack

peek(#{?TYPE := ?M, items := Items}) ->
  'erlang.List':peek(Items).

pop(#{?TYPE := ?M, items := []} = List) ->
  List;
pop(#{?TYPE := ?M, items := [_ | Rest]} = List) ->
  List#{items => Rest}.

%% clojerl.ISeq

seq(#{?TYPE := ?M, items := []}) -> ?NIL;
seq(#{?TYPE := ?M, items := Seq}) -> Seq.

to_list(#{?TYPE := ?M, items := Items}) -> Items.

%% clojerl.IStringable

str(#{?TYPE := ?M, items := []}) ->
  <<"()">>;
str(#{?TYPE := ?M} = List) ->
  clj_rt:print(List).
