-module('clojerl.LazySeq').

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
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

-spec ?CONSTRUCTOR(function()) -> type().
?CONSTRUCTOR(Fn) when is_function(Fn) ->
  #?TYPE{data = Fn}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(#?TYPE{name = ?M, data = Fn}) ->
  case Fn([]) of
    ?NIL -> 0;
    Seq       -> 'clojerl.ICounted':count(Seq)
  end.

cons(#?TYPE{name = ?M} = LazySeq, X) ->
  'clojerl.Cons':?CONSTRUCTOR(X, LazySeq).

empty(_) -> [].

equiv( #?TYPE{name = ?M, data = X}
     , #?TYPE{name = ?M, data = Y}
     ) ->
  clj_rt:equiv(X, Y);
equiv(#?TYPE{name = ?M} = LazySeq, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> clj_rt:equiv('clojerl.ISeqable':to_list(LazySeq), Y);
    false -> false
  end.

hash(#?TYPE{name = ?M} = LazySeq) ->
  clj_murmur3:ordered(LazySeq).

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, ?NIL).

with_meta(#?TYPE{name = ?M, info = Info} = List, Metadata) ->
  List#?TYPE{info = Info#{meta => Metadata}}.

reduce(#?TYPE{name = ?M} = LazySeq, F) ->
  case seq(LazySeq) of
    ?NIL -> clj_rt:apply(F, []);
    Seq  ->
      Init = 'clojerl.ISeq':first(Seq),
      Next = 'clojerl.ISeq':next(Seq),
      do_reduce(F, Init, Next)
  end.

reduce(#?TYPE{name = ?M} = LazySeq, F, Init) ->
  do_reduce(F, Init, LazySeq).

do_reduce(F, Acc, #?TYPE{name = ?M} = LazySeq) ->
  do_reduce(F, Acc, seq(LazySeq));
do_reduce(F, Acc, Seq) when Seq =/= ?NIL ->
  First = 'clojerl.ISeq':first(Seq),
  Val   = clj_rt:apply(F, [Acc, First]),
  case 'clojerl.Reduced':is_reduced(Val) of
    true  -> Val;
    false -> do_reduce(F, Val, 'clojerl.ISeq':next(Seq))
  end;
do_reduce(_F, Acc, _Seq) ->
  Acc.

first(#?TYPE{name = ?M, data = Fn}) ->
  case Fn([]) of
    ?NIL -> ?NIL;
    #?TYPE{name = ?M} = LazySeq -> first(LazySeq);
    Seq -> clj_rt:first(Seq)
  end.

next(#?TYPE{name = ?M, data = Fn}) ->
  case Fn([]) of
    ?NIL -> ?NIL;
    #?TYPE{name = ?M} = LazySeq -> next(LazySeq);
    Seq -> clj_rt:next(Seq)
  end.

more(#?TYPE{name = ?M, data = Fn}) ->
  case Fn([]) of
    ?NIL -> [];
    #?TYPE{name = ?M} = LazySeq -> more(LazySeq);
    Seq -> clj_rt:rest(Seq)
  end.

'_'(_) -> ?NIL.

seq(#?TYPE{name = ?M, data = Fn}) ->
  case Fn([]) of
    ?NIL ->
      ?NIL;
    #?TYPE{name = ?M} = LazySeq ->
      seq(LazySeq);
    Seq ->
      'clojerl.ISeqable':seq(Seq)
  end.

to_list(#?TYPE{name = ?M} = LazySeq) ->
  do_to_list(LazySeq, []).

-spec do_to_list(?NIL | any(), [any()]) -> [any()].
do_to_list(?NIL, Acc) ->
  lists:reverse(Acc);
do_to_list(Seq0, Acc) ->
  case 'clojerl.ISeqable':seq(Seq0) of
    ?NIL -> do_to_list(?NIL, Acc);
    Seq ->
      First = 'clojerl.ISeq':first(Seq),
      Rest  = 'clojerl.ISeq':next(Seq),
      do_to_list(Rest, [First | Acc])
  end.

str(#?TYPE{name = ?M, data = Fn}) ->
  {uniq, Uniq} = erlang:fun_info(Fn, uniq),
  UniqBin = 'clojerl.IStringable':str(Uniq),
  <<"#<clojerl.LazySeq@", UniqBin/binary, ">">>.
