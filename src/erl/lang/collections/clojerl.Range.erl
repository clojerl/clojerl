-module('clojerl.Range').

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IChunkedSeq').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.IReduce').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

-export([?CONSTRUCTOR/3]).

-export([count/1]).
-export([ cons/2
        , empty/1
        ]).
-export([ chunked_first/1
        , chunked_more/1
        , chunked_next/1
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

-type type() :: #?TYPE{data :: {integer(), integer(), integer()}}.

-spec ?CONSTRUCTOR(integer(), integer(), integer()) -> type().
?CONSTRUCTOR(Start, End, Step) when Step > 0, End =< Start;
                                    Step < 0, Start =< End;
                                    Start == End ->
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

chunked_first(#?TYPE{name = ?M, data = {Start0, End0, Step0}}) ->
  End1    = Start0 + ?CHUNK_SIZE * Step0,
  End2    = case Step0 >= 0 of
              true  -> lists:min([End0, End1]);
              false -> lists:max([End0, End1])
            end,
  Numbers = to_list(Start0, End2, Step0),
  Tuple   = list_to_tuple(Numbers),
  'clojerl.TupleChunk':?CONSTRUCTOR(Tuple).

chunked_next(#?TYPE{name = ?M} = Range) ->
  clj_rt:seq(chunked_more(Range)).

chunked_more(#?TYPE{name = ?M, data = {Start0, End0, Step0}}) ->
  Start1 = Start0 + ?CHUNK_SIZE * Step0,
  ?CONSTRUCTOR(Start1, End0, Step0).

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
    Step < 0, Start + Step < End ->
  Acc;
do_reduce(F, Acc, Start, End, Step) ->
  Val = clj_rt:apply(F, [Acc, Start]),
  case 'clojerl.Reduced':is_reduced(Val) of
    true  -> Val;
    false -> do_reduce(F, Val, Start + Step, End, Step)
  end.

first(#?TYPE{name = ?M, data = {Start, _, _}}) -> Start.

next(#?TYPE{name = ?M, data = {Start, End, Step}}) when
    Step > 0, Start + Step >= End;
    Step < 0, Start + Step =< End ->
  ?NIL;
next(#?TYPE{name = ?M, data = {Start, End, Step}}) ->
  ?CONSTRUCTOR(Start + Step, End, Step).

more(#?TYPE{name = ?M, data = {Start, End, Step}}) when
    Step > 0, Start + Step >= End;
    Step < 0, Start + Step =< End ->
  [];
more(#?TYPE{name = ?M, data = {Start, End, Step}}) ->
  ?CONSTRUCTOR(Start + Step, End, Step).

'_'(_) -> ?NIL.

seq(#?TYPE{name = ?M} = Seq) -> Seq.

to_list(#?TYPE{name = ?M, data = {Start, End, Step}}) ->
  to_list(Start, End, Step).

str(#?TYPE{name = ?M} = Range) ->
  clj_rt:print(Range).

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec to_list(number(), number(), number()) -> [number()].
to_list(Start, End, Step) when is_float(Start);
                               is_float(End);
                               is_float(Step) ->
  N = clj_utils:ceil((End - Start) / Step),
  to_list_loop(N, Start, Step, []);
to_list(Start, End, Step) when Step >= 0 ->
  lists:seq(Start, End - 1, Step);
to_list(Start, End, Step) when Step < 0 ->
  lists:seq(Start, End + 1, Step).

%% We need to build the list from the Start to the End to avoid
%% floating point rounding errors.
-spec to_list_loop(integer(), number(), number(), [number()]) -> [number()].
to_list_loop(N, X, Step, L) when N >= 4 ->
  Y = X + Step,
  Z = Y + Step,
  W = Z + Step,
  to_list_loop(N - 4, W + Step, Step, [W, Z, Y, X | L]);
to_list_loop(N, X, Step, L) when N >= 2 ->
  Y = X + Step,
  to_list_loop(N - 2, Y + Step, Step, [Y, X | L]);
to_list_loop(1, X, _, L) ->
  lists:reverse([X | L]);
to_list_loop(0, _, _, L) ->
  lists:reverse(L).
