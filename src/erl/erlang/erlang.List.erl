-module('erlang.List').

-include("clojerl.hrl").

-behavior('clojerl.IColl').
-behavior('clojerl.ICounted').
-behavior('clojerl.IEncodeClojure').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.ILookup').
-behavior('clojerl.IReduce').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.IStack').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

-export([ cons/2
        , empty/1
        ]).
-export([count/1]).
-export(['erl->clj'/2]).
-export([equiv/2]).
-export([hash/1]).
-export([ get/2
        , get/3
        ]).
-export([ reduce/2
        , reduce/3
        ]).
-export([ first/1
        , more/1
        , next/1
        ]).
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

%% clojerl.ICounted

count(Items) -> length(Items).

%% clojerl.IHash

hash(List) -> clj_murmur3:ordered(List).

%% clojerl.IEncodeErlang

'erl->clj'(List, Recursive) ->
  L = case Recursive of
        true  -> [clj_rt:'erl->clj'(Item, true) || Item <- List];
        false -> List
      end,
  'clojerl.List':?CONSTRUCTOR(L).

%% clojerl.ILookup

get(List, Key) ->
  get(List, Key, ?NIL).

get(List, Key, NotFound) ->
  case lists:keyfind(Key, 1, List) of
    {Key, Value} -> Value;
    false -> NotFound
  end.

%% clojerl.IReduce

reduce([], F) ->
  clj_rt:apply(F, []);
reduce([First | Rest], F) ->
  do_reduce(F, First, Rest).

reduce(List, F, Init) ->
  do_reduce(F, Init, List).

do_reduce(F, Acc, [First | Items]) ->
  Val = clj_rt:apply(F, [Acc, First]),
  case 'clojerl.Reduced':is_reduced(Val) of
    true  -> 'clojerl.Reduced':deref(Val);
    false -> do_reduce(F, Val, Items)
  end;
do_reduce(_F, Acc, []) ->
  Acc.

%% clojerl.IStringable

str([]) ->
  <<"">>;
str(Items) when is_list(Items) ->
  case io_lib:printable_unicode_list(Items) of
    true  -> unicode:characters_to_binary(Items);
    false -> clj_rt:print_str(Items)
  end.

%% clojerl.ISeqable

seq([]) -> ?NIL;
seq(List) -> List.

to_list(List) -> List.

%% clojerl.ISeq

first([]) -> ?NIL;
first([First | _]) -> First.

more([]) -> [];
more([_ | Rest]) -> Rest.

next([]) -> ?NIL;
next([_ | []]) -> ?NIL;
next([_ | Rest]) -> Rest.

%% clojerl.IStack

peek([]) -> ?NIL;
peek([X | _]) -> X.

pop([]) -> [];
pop([_ | Rest]) -> Rest.

%% clojerl.IColl

cons([], X) ->
  [X];
cons(Items, X) ->
  [X | Items].

empty(_) -> [].

%% clojerl.IEquiv

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
