-module('clojerl.Repeat').

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

-export([?CONSTRUCTOR/1, ?CONSTRUCTOR/2]).

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

-type type() :: #{ ?TYPE => ?M
                 , items => any()
                 , count => any()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(any()) -> type().
?CONSTRUCTOR(Value) ->
  ?CONSTRUCTOR(infinity, Value).

-spec ?CONSTRUCTOR(infinity | non_neg_integer(), any()) -> type().
?CONSTRUCTOR(Count, Value) when Count > 0 ->
  #{ ?TYPE => ?M
   , value => Value
   , count => Count
   , meta  => ?NIL
   };
?CONSTRUCTOR(_, _Value)  ->
  'clojerl.List':?CONSTRUCTOR([]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(#{?TYPE := ?M, count := Count}) -> Count.

%% clojerl.IColl

cons(#{?TYPE := ?M} = Iterate, X) -> 'clojerl.Cons':?CONSTRUCTOR(X, Iterate).

empty(_) -> clj_rt:list([]).

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, value := X, count := Count}
     , #{?TYPE := ?M, items := Y, count := Count}
     ) ->
  clj_rt:equiv(X, Y);
equiv(#{?TYPE := ?M, count := infinity}, _) ->
  false;
equiv(#{?TYPE := ?M} = X, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> 'erlang.List':equiv(to_list(X), Y);
    false -> false
  end.

%% clojerl.IHash

hash(#{?TYPE := ?M} = Repeat) ->
  clj_murmur3:ordered(to_list(Repeat)).

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = Repeat, Metadata) ->
  Repeat#{meta => Metadata}.

%% clojerl.IReduce

reduce(#{?TYPE := ?M, value := Value, count := 1}, _F) ->
  Value;
reduce(#{?TYPE := ?M, value := Value, count := infinity}, F) ->
  do_reduce_infinity(F, Value, Value);
reduce(#{?TYPE := ?M, value := Value, count := Count}, F) ->
  do_reduce(F, Value, Value, Count - 1).

reduce(#{?TYPE := ?M, count := 0}, _F, Init) ->
  Init;
reduce(#{?TYPE := ?M, value := Value, count := infinity}, F, Init) ->
  do_reduce_infinity(F, Value, Init);
reduce(#{?TYPE := ?M, value := Value, count := Count}, F, Init) ->
  do_reduce(F, Value, Init, Count).

do_reduce(_F, _Value, Acc, 0) ->
  Acc;
do_reduce(F, Value, Acc0, Count) ->
  Acc1 = clj_rt:apply(F, [Acc0, Value]),
  case 'clojerl.Reduced':is_reduced(Acc1) of
    true  -> 'clojerl.Reduced':deref(Acc1);
    false -> do_reduce(F, Value, Acc1, Count - 1)
  end.

do_reduce_infinity(F, Value, Acc0) ->
  Acc1 = clj_rt:apply(F, [Acc0, Value]),
  case 'clojerl.Reduced':is_reduced(Acc1) of
    true  -> 'clojerl.Reduced':deref(Acc1);
    false -> do_reduce_infinity(F, Value, Acc1)
  end.

%% clojerl.ISeq

first(#{?TYPE := ?M, value := Val}) -> Val.

next(#{?TYPE := ?M, count := infinity} = Repeat) ->
  Repeat;
next(#{?TYPE := ?M, count := 1}) ->
  ?NIL;
next(#{?TYPE := ?M, count := Count} = Repeat) ->
  Repeat#{count := Count - 1}.

more(#{?TYPE := ?M, count := infinity} = Repeat) ->
  Repeat;
more(#{?TYPE := ?M, count := 1}) ->
  [];
more(#{?TYPE := ?M, count := Count} = Repeat) ->
  Repeat#{count := Count - 1}.

%% clojerl.ISeqable

seq(#{?TYPE := ?M} = Repeat) -> Repeat.

to_list(#{?TYPE := ?M, count := infinity}) ->
  ?ERROR(<<"Can't generate an infinite list">>);
to_list(#{?TYPE := ?M, count := Count, value := Value}) ->
  lists:duplicate(Count, Value).

%% clojerl.ISequential

'_'(_) -> ?NIL.

%% clojerl.IStringable

str(#{?TYPE := ?M, count := 0}) ->
  ?NIL;
str(#{?TYPE := ?M} = Repeat) ->
  List = to_list(Repeat),
  clj_rt:print_str(clj_rt:list(List)).
