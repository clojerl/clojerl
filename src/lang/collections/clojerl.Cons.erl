-module('clojerl.Cons').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.IReduce').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([?CONSTRUCTOR/2]).

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
-export(['_'/1]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

-type type() :: #?TYPE{}.

-spec ?CONSTRUCTOR(any(), any()) -> type().
?CONSTRUCTOR(First, More) ->
  #?TYPE{data = {First, More}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(#?TYPE{name = ?M, data = {_, More}}) ->
  1 + clj_core:count(More).

cons(#?TYPE{name = ?M} = Cons, X) -> ?CONSTRUCTOR(X, Cons).

empty(_) -> [].

equiv( #?TYPE{name = ?M, data = {XFirst, XMore}}
                      , #?TYPE{name = ?M, data = {YFirst, YMore}}
                      ) ->
  clj_core:equiv(XFirst, YFirst) andalso clj_core:equiv(XMore, YMore);
equiv(#?TYPE{name = ?M} = Cons, Y) ->
  case clj_core:'sequential?'(Y) of
    true  -> clj_core:equiv(to_list(Cons), clj_core:seq(Y));
    false -> false
  end.

hash(#?TYPE{name = ?M, data = Cons}) ->
  clj_murmur3:ordered(Cons).

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, ?NIL).

with_meta(#?TYPE{name = ?M, info = Info} = List, Metadata) ->
  List#?TYPE{info = Info#{meta => Metadata}}.

reduce(#?TYPE{name = ?M, data = {First, Rest}}, F) ->
  do_reduce(F, First, clj_core:to_list(Rest)).

reduce(#?TYPE{name = ?M, data = {First, Rest}}, F, Init) ->
  do_reduce(F, Init, clj_core:conj(clj_core:to_list(Rest), First)).

do_reduce(F, Acc, [First | Items]) ->
  Val = clj_core:apply(F, [Acc, First]),
  case 'clojerl.Reduced':is_reduced(Val) of
    true  -> Val;
    false -> do_reduce(F, Val, Items)
  end;
do_reduce(_F, Acc, []) ->
  Acc.

first(#?TYPE{name = ?M, data = {First, _}}) -> First.

next(#?TYPE{name = ?M, data = {_, ?NIL}}) -> ?NIL;
next(#?TYPE{name = ?M, data = {_, More}}) -> clj_core:seq(More).

more(#?TYPE{name = ?M, data = {_, ?NIL}}) -> [];
more(#?TYPE{name = ?M, data = {_, More}}) -> More.

'_'(_) -> ?NIL.

seq(#?TYPE{name = ?M} = Cons) -> Cons.

to_list(#?TYPE{name = ?M, data = {First, More}}) ->
  [First | clj_core:to_list(More)].

str(#?TYPE{name = ?M} = Cons) ->
  clj_core:str(to_list(Cons)).
