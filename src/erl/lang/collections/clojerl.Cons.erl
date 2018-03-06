-module('clojerl.Cons').

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
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
                 , first => any()
                 , more  => any()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(any(), any()) -> type().
?CONSTRUCTOR(First, More) ->
  #{ ?TYPE => ?M
   , first => First
   , more  => More
   , meta  => ?NIL
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(#{?TYPE := ?M, more := More}) ->
  1 + clj_rt:count(More).

cons(#{?TYPE := ?M} = Cons, X) -> ?CONSTRUCTOR(X, Cons).

empty(_) -> [].

equiv( #{?TYPE := ?M, first := FirstX, more := MoreX}
     , #{?TYPE := ?M, first := FirstY, more := MoreY}
     ) ->
  clj_rt:equiv(FirstX, FirstY) andalso clj_rt:equiv(MoreX, MoreY);
equiv(#{?TYPE := ?M} = Cons, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> 'erlang.List':equiv(to_list(Cons), clj_rt:seq(Y));
    false -> false
  end.

'->erl'(#{?TYPE := ?M} = X, Recursive) ->
  List = to_list(X),
  case Recursive of
    true  -> [clj_rt:'->erl'(Item, true) || Item <- List];
    false -> List
  end.

hash(#{?TYPE := ?M, first := First, more := More}) ->
  clj_murmur3:ordered({First, More}).

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = List, Metadata) ->
  List#{meta => Metadata}.

reduce(#{?TYPE := ?M, first := First, more := More}, F) ->
  do_reduce(F, First, clj_rt:to_list(More)).

reduce(#{?TYPE := ?M, first := First, more := More}, F, Init) ->
  do_reduce(F, Init, 'erlang.List':cons(clj_rt:to_list(More), First)).

do_reduce(F, Acc, [First | Items]) ->
  Val = clj_rt:apply(F, [Acc, First]),
  case 'clojerl.Reduced':is_reduced(Val) of
    true  -> Val;
    false -> do_reduce(F, Val, Items)
  end;
do_reduce(_F, Acc, []) ->
  Acc.

first(#{?TYPE := ?M, first := First}) -> First.

next(#{?TYPE := ?M, more := ?NIL}) -> ?NIL;
next(#{?TYPE := ?M, more := More}) -> clj_rt:seq(More).

more(#{?TYPE := ?M, more := ?NIL}) -> [];
more(#{?TYPE := ?M, more := More}) -> More.

'_'(_) -> ?NIL.

seq(#{?TYPE := ?M} = Cons) -> Cons.

to_list(#{?TYPE := ?M, first := First, more := More}) ->
  [First | clj_rt:to_list(More)].

str(#{?TYPE := ?M} = Cons) ->
  clj_rt:print(Cons).
