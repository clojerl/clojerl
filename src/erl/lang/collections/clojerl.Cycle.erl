-module('clojerl.Cycle').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.IReduce').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISeqable').
-behavior('clojerl.ISequential').
-behavior('clojerl.IStringable').

-export([?CONSTRUCTOR/1]).

-export([count/1]).
-export([ cons/2
        , empty/1
        ]).
-export([equiv/2]).
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
-export(['_'/1]).
-export([str/1]).

-type type() :: #{ ?TYPE   => ?M
                 , items   => any()
                 , current => any()
                 , meta    => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(list()) -> type().
?CONSTRUCTOR(?NIL) ->
  clj_rt:list([]);
?CONSTRUCTOR(Items) ->
  #{ ?TYPE   => ?M
   , items   => Items
   , prev    => ?NIL
   , current => Items
   , meta    => ?NIL
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(#{?TYPE := ?M}) ->
  ?ERROR(<<"Unsupported count operation for clojerl.Cycle">>).

%% clojerl.IColl

cons(#{?TYPE := ?M} = Cycle, X) -> 'clojerl.Cons':?CONSTRUCTOR(X, Cycle).

empty(_) -> clj_rt:list([]).

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, items := X, current := A}
     , #{?TYPE := ?M, items := Y, current := B}
     ) ->
  clj_rt:equiv(X, Y) andalso clj_rt:equiv(A, B);
equiv(#{?TYPE := ?M}, _) ->
  false.

%% clojerl.IHash

hash(#{?TYPE := ?M} = X) ->
  erlang:phash2(X).

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = List, Metadata) ->
  List#{meta => Metadata}.

%% clojerl.IReduce

reduce(#{?TYPE := ?M, current := ?NIL, items := Items}, F) ->
  Init = 'clojerl.ISeq':first(Items),
  do_reduce(F, Items, 'clojerl.ISeq':next(Items), Init);
reduce(#{?TYPE := ?M, current := Current, items := Items}, F) ->
  Init = 'clojerl.ISeq':first(Current),
  do_reduce(F, Items, 'clojerl.ISeq':next(Current), Init).

reduce(#{?TYPE := ?M, current := ?NIL, items := Items}, F, Init) ->
  do_reduce(F, Items, Items, Init);
reduce(#{?TYPE := ?M, current := Current, items := Items}, F, Init) ->
  do_reduce(F, Items, Current, Init).

do_reduce(F, Items, ?NIL, Acc) ->
  do_reduce(F, Items, Items, Acc);
do_reduce(F, Items, Current, Acc0) ->
  Value = 'clojerl.ISeq':first(Current),
  Acc1  = clj_rt:apply(F, [Acc0, Value]),
  case 'clojerl.Reduced':is_reduced(Acc1) of
    true  -> 'clojerl.Reduced':deref(Acc1);
    false -> do_reduce(F, Items, 'clojerl.ISeq':next(Current), Acc1)
  end.

%% clojerl.ISeq

first(#{?TYPE := ?M, prev := ?NIL, current := ?NIL, items := Items}) ->
  'clojerl.ISeq':first(Items);
first(#{?TYPE := ?M, prev := ?NIL, current := Current}) ->
  'clojerl.ISeq':first(Current);
first(#{?TYPE := ?M, prev := Prev, items := Items}) ->
  case 'clojerl.ISeq':next(Prev) of
    ?NIL -> 'clojerl.ISeq':first(Items);
    Next -> 'clojerl.ISeq':first(Next)
  end.

next(#{?TYPE := ?M, prev := ?NIL, items := Items} = Cycle) ->
  Cycle#{prev := Items};
next(#{?TYPE := ?M, prev := Prev, items := Items} = Cycle) ->
  case 'clojerl.ISeq':next(Prev) of
    ?NIL -> Cycle#{current := 'clojerl.ISeq':next(Items), prev := ?NIL};
    X    -> Cycle#{current := ?NIL, prev := X}
  end.

more(#{?TYPE := ?M} = Cycle) -> next(Cycle).

%% clojerl.ISeqable

seq(#{?TYPE := ?M} = Cycle) -> Cycle.

to_list(#{?TYPE := ?M}) ->
  ?ERROR(<<"Can't generate an infinite list">>).

%% clojerl.ISequential

'_'(_) -> ?NIL.

%% clojerl.IStringable

str(#{?TYPE := ?M}) ->
  <<"#<clojerl.Cycle>">>.
