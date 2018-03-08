-module('clojerl.LazySeq').

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IErl').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.IReduce').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

-export([?CONSTRUCTOR/1]).

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
                 , fn    => function()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(function()) -> type().
?CONSTRUCTOR(Fn) when is_function(Fn) ->
  #{ ?TYPE => ?M
   , fn    => Fn
   , meta  => ?NIL
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(#{?TYPE := ?M, fn := Fn}) ->
  case Fn([]) of
    ?NIL -> 0;
    Seq       -> 'clojerl.ICounted':count(Seq)
  end.

cons(#{?TYPE := ?M} = LazySeq, X) ->
  'clojerl.Cons':?CONSTRUCTOR(X, LazySeq).

empty(_) -> [].

equiv( #{?TYPE := ?M, fn := X}
     , #{?TYPE := ?M, fn := X}
     ) ->
  true;
equiv( #{?TYPE := ?M} = X
     , #{?TYPE := ?M} = Y
     ) ->
  'erlang.List':equiv(to_list(X), to_list(Y));
equiv(#{?TYPE := ?M} = LazySeq, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> 'erlang.List':equiv(to_list(LazySeq), Y);
    false -> false
  end.

'->erl'(#{?TYPE := ?M} = X, Recursive) ->
  List = to_list(X),
  case Recursive of
    true  -> [clj_rt:'->erl'(Item, true) || Item <- List];
    false -> List
  end.

hash(#{?TYPE := ?M} = LazySeq) ->
  clj_murmur3:ordered(LazySeq).

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = List, Metadata) ->
  List#{meta => Metadata}.

reduce(#{?TYPE := ?M} = LazySeq, F) ->
  case seq(LazySeq) of
    ?NIL -> clj_rt:apply(F, []);
    Seq  ->
      Init = 'clojerl.ISeq':first(Seq),
      Next = 'clojerl.ISeq':next(Seq),
      do_reduce(F, Init, Next)
  end.

reduce(#{?TYPE := ?M} = LazySeq, F, Init) ->
  do_reduce(F, Init, LazySeq).

do_reduce(F, Acc, #{?TYPE := ?M} = LazySeq) ->
  do_reduce(F, Acc, seq(LazySeq));
do_reduce(F, Acc, Seq) when Seq =/= ?NIL ->
  Module = clj_rt:type_module(Seq),
  First  = Module:first(Seq),
  Val    = clj_rt:apply(F, [Acc, First]),
  case 'clojerl.Reduced':is_reduced(Val) of
    true  -> Val;
    false -> do_reduce(F, Val, Module:next(Seq))
  end;
do_reduce(_F, Acc, _Seq) ->
  Acc.

first(#{?TYPE := ?M, fn := Fn}) ->
  case Fn([]) of
    ?NIL -> ?NIL;
    #{?TYPE := ?M} = LazySeq -> first(LazySeq);
    Seq -> clj_rt:first(Seq)
  end.

next(#{?TYPE := ?M, fn := Fn}) ->
  case Fn([]) of
    ?NIL -> ?NIL;
    #{?TYPE := ?M} = LazySeq -> next(LazySeq);
    Seq -> clj_rt:next(Seq)
  end.

more(#{?TYPE := ?M, fn := Fn}) ->
  case Fn([]) of
    ?NIL -> [];
    #{?TYPE := ?M} = LazySeq -> more(LazySeq);
    Seq -> clj_rt:rest(Seq)
  end.

'_'(_) -> ?NIL.

seq(#{?TYPE := ?M, fn := Fn}) ->
  case Fn([]) of
    ?NIL ->
      ?NIL;
    #{?TYPE := ?M} = LazySeq ->
      seq(LazySeq);
    Seq ->
      Module = clj_rt:type_module(Seq),
      Module:seq(Seq)
  end.

to_list(#{?TYPE := ?M} = LazySeq) ->
  do_to_list(LazySeq, []).

-spec do_to_list(?NIL | any(), [any()]) -> [any()].
do_to_list(?NIL, Acc) ->
  lists:reverse(Acc);
do_to_list(#{?TYPE := ?M} = LazySeq, Acc) ->
  do_to_list(seq(LazySeq), Acc);
do_to_list(Seq0, Acc) ->
  Module0 = clj_rt:type_module(Seq0),
  Seq1    = Module0:seq(Seq0),
  First   = 'clojerl.ISeq':first(Seq1),
  Rest    = 'clojerl.ISeq':next(Seq1),
  do_to_list(Rest, [First | Acc]).

str(#{?TYPE := ?M, fn := Fn}) ->
  {uniq, Uniq} = erlang:fun_info(Fn, uniq),
  UniqBin = 'clojerl.IStringable':str(Uniq),
  <<"#<clojerl.LazySeq@", UniqBin/binary, ">">>.
