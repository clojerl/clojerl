-module('erlang.List').

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IColl').
-behavior('clojerl.IHash').
-behavior('clojerl.IReduce').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.IStack').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

-export([count/1]).
-export([equiv/2]).
-export([ cons/2
        , empty/1
        ]).
-export([hash/1]).
-export([ reduce/2
        , reduce/3
        ]).
-export([ first/1
        , more/1
        , next/1
        ]).
-export(['_'/1]).
-export([ peek/1
        , pop/1
        ]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(Items) -> length(Items).

hash(List) -> clj_murmur3:ordered(List).

reduce([], F) ->
  clj_rt:apply(F, []);
reduce([First | Rest], F) ->
  do_reduce(F, First, Rest).

reduce(List, F, Init) ->
  do_reduce(F, Init, List).

do_reduce(F, Acc, [First | Items]) ->
  Val = clj_rt:apply(F, [Acc, First]),
  case 'clojerl.Reduced':is_reduced(Val) of
    true  -> Val;
    false -> do_reduce(F, Val, Items)
  end;
do_reduce(_F, Acc, []) ->
  Acc.

str([]) ->
  <<"">>;
str(Items) when is_list(Items) ->
  clj_rt:print(Items).

seq([]) -> ?NIL;
seq(List) -> List.

to_list(List) -> List.

first([]) -> ?NIL;
first([First | _]) -> First.

more([]) -> [];
more([_ | Rest]) -> Rest.

next([]) -> ?NIL;
next([_ | []]) -> ?NIL;
next([_ | Rest]) -> Rest.

'_'(_) -> ?NIL.

peek([]) -> ?NIL;
peek([X | _]) -> X.

pop([]) -> [];
pop([_ | Rest]) -> Rest.

cons([], X) ->
  [X];
cons(Items, X) ->
  [X | Items].

empty(_) -> [].

equiv(X, Y) when is_list(X), is_list(Y) ->
  case length(X) =:= length(Y) of
    true  -> do_equiv(X, Y);
    false -> false
  end;
equiv(X, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> clj_rt:equiv(Y, X);
    false -> false
  end.

do_equiv([], []) ->
  true;
do_equiv([X | TailX], [Y | TailY]) ->
  case clj_rt:equiv(X, Y) of
    true  -> do_equiv(TailX, TailY);
    false -> false
  end.
