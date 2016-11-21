-module('clojerl.erlang.List').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IColl').
-behavior('clojerl.IHash').
-behavior('clojerl.IReduce').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.IStack').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

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

reduce([], _F) -> ?NIL;
reduce([First | Rest], F) ->
  do_reduce(F, First, Rest).

reduce(List, F, Init) ->
  lists:foldl(F, Init, List),
  do_reduce(F, Init, List).

do_reduce(F, Acc, [First | Items]) ->
  do_reduce(F, clj_core:apply(F, [Acc, First]), Items);
do_reduce(_F, Acc, []) ->
  Acc.

str([]) ->
  <<"()">>;
str(Items) when is_list(Items) ->
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs = 'clojerl.String':join(ItemsStrs, <<" ">>),
  <<"(", Strs/binary, ")">>.

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
  case clj_core:'sequential?'(Y) of
    true  -> clj_core:equiv(Y, X);
    false -> false
  end.

do_equiv([], []) ->
  true;
do_equiv([X | TailX], [Y | TailY]) ->
  case clj_core:equiv(X, Y) of
    true  -> do_equiv(TailX, TailY);
    false -> false
  end.
