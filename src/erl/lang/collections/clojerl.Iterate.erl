-module('clojerl.Iterate').

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
-export([ seq/1
        , to_list/1
        ]).
-export(['_'/1]).
-export([str/1]).

-type type() :: #{ ?TYPE => ?M
                 , fn    => any()
                 , value => any()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(any(), any()) -> type().
?CONSTRUCTOR(Fn, Value) ->
  #{ ?TYPE => ?M
   , fn    => Fn
   , value => Value
   , meta  => ?NIL
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(#{?TYPE := ?M}) ->
  ?ERROR(<<"Unsupported count operation for clojerl.Cycle">>).

%% clojerl.IColl

cons(#{?TYPE := ?M} = Iterate, X) -> 'clojerl.Cons':?CONSTRUCTOR(X, Iterate).

empty(_) -> clj_rt:list([]).

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, fn := FnX, value := ValueX}
     , #{?TYPE := ?M, fn := FnY, value := ValueY}
     ) ->
  clj_rt:equiv(FnX, FnY) andalso clj_rt:equiv(ValueX, ValueY);
equiv(#{?TYPE := ?M}, _) ->
  false.

%% clojerl.IHash

hash(#{?TYPE := ?M} = Iterate) ->
  %% TODO: change
  erlang:phash2(Iterate).

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = Iterate, Metadata) ->
  Iterate#{meta => Metadata}.

%% clojerl.IReduce

reduce(#{?TYPE := ?M, value := Value, fn := Iterate}, F) ->
  NextValue = 'clojerl.IFn':apply(Iterate, [Value]),
  do_reduce(F, Iterate, NextValue, Value).

reduce(#{?TYPE := ?M, value := Value, fn := Iterate}, F, Init) ->
  do_reduce(F, Iterate, Value, Init).

do_reduce(F, Iterate, Value, Acc0) ->
  Acc1  = clj_rt:apply(F, [Acc0, Value]),
  case 'clojerl.Reduced':is_reduced(Acc1) of
    true  -> 'clojerl.Reduced':deref(Acc1);
    false ->
      NextValue = 'clojerl.IFn':apply(Iterate, [Value]),
      do_reduce(F, Iterate, NextValue, Acc1)
  end.

%% clojerl.ISeq

first(#{?TYPE := ?M, value := Val}) -> Val.

next(#{?TYPE := ?M, fn := Fn, value := Value} = Iterate) ->
  Iterate#{value := 'clojerl.IFn':apply(Fn, [Value])}.

more(#{?TYPE := ?M} = Iterate) -> next(Iterate).

%% clojerl.ISeqable

seq(#{?TYPE := ?M} = Iterate) -> Iterate.

to_list(#{?TYPE := ?M}) ->
  ?ERROR(<<"Can't generate an infinite list">>).

%% clojerl.ISequential

'_'(_) -> ?NIL.

%% clojerl.IStringable

str(#{?TYPE := ?M}) ->
  ?ERROR(<<"Can't generate an infinite list">>).
