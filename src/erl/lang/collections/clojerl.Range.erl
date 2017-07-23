-module('clojerl.Range').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.IReduce').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([?CONSTRUCTOR/3]).

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

-spec ?CONSTRUCTOR(integer(), integer(), integer()) -> type().
?CONSTRUCTOR(Start, End, Step) when Step >= 0, End =< Start;
                                    Step < 0, Start =< End ->
  [];
?CONSTRUCTOR(Start, End, Step) ->
  #?TYPE{data = {Start, End, Step}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(#?TYPE{name = ?M, data = {Start, End, Step}}) ->
  (End - Start + Step) div Step - 1.

cons(#?TYPE{name = ?M} = Range, X) ->
  'clojerl.Cons':?CONSTRUCTOR(X, Range).

empty(_) -> [].

equiv( #?TYPE{name = ?M, data = X}
     , #?TYPE{name = ?M, data = Y}
     ) ->
  clj_rt:equiv(X, Y);
equiv(#?TYPE{name = ?M} = X, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> clj_rt:equiv(to_list(X), Y);
    false -> false
  end.

hash(#?TYPE{name = ?M} = X) ->
  clj_murmur3:ordered(to_list(X)).

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, ?NIL).

with_meta(#?TYPE{name = ?M, info = Info} = Range, Metadata) ->
  Range#?TYPE{info = Info#{meta => Metadata}}.

reduce(#?TYPE{name = ?M, data = {Start, End, Step}}, F) ->
  do_reduce(F, Start, Start + Step, End, Step).

reduce(#?TYPE{name = ?M, data = {Start, End, Step}}, F, Init) ->
  do_reduce(F, Init, Start, End, Step).

do_reduce(_F, Acc, Start, End, Step) when
    Step >= 0, Start + Step > End;
    Step < 1, Start + Step < End ->
  Acc;
do_reduce(F, Acc, Start, End, Step) ->
  Val = clj_rt:apply(F, [Acc, Start]),
  case 'clojerl.Reduced':is_reduced(Val) of
    true  -> Val;
    false -> do_reduce(F, Val, Start + Step, End, Step)
  end.

first(#?TYPE{name = ?M, data = {Start, _, _}}) -> Start.

next(#?TYPE{name = ?M, data = {Start, End, Step}}) when
    Step >= 0, Start + Step >= End;
    Step < 1, Start + Step =< End ->
  ?NIL;
next(#?TYPE{name = ?M, data = {Start, End, Step}}) ->
  ?CONSTRUCTOR(Start + Step, End, Step).

more(#?TYPE{name = ?M, data = {Start, End, Step}}) when
    Step >= 0, Start + Step >= End;
    Step < 1, Start + Step =< End ->
  [];
more(#?TYPE{name = ?M, data = {Start, End, Step}}) ->
  ?CONSTRUCTOR(Start + Step, End, Step).

'_'(_) -> ?NIL.

seq(#?TYPE{name = ?M} = Seq) -> Seq.

to_list(#?TYPE{name = ?M, data = {Start, End, Step}}) when Step >= 0 ->
  lists:seq(Start, End - 1, Step);
to_list(#?TYPE{name = ?M, data = {Start, End, Step}}) when Step < 0 ->
  lists:seq(Start, End + 1, Step).

str(#?TYPE{name = ?M} = Range) ->
  clj_rt:print(Range).
