-module('clojerl.Cons').

-include("clojerl.hrl").

-behavior('clojerl.IColl').
-behavior('clojerl.ICounted').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IErl').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

-export([?CONSTRUCTOR/2]).

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
                 , first => any()
                 , more  => any()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(any(), any()) -> type().
?CONSTRUCTOR(First, More) ->
  #{ ?TYPE => ?M
   , first => First
   , more  => More
   , meta  => ?NIL
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(#{?TYPE := ?M, more := More}) ->
  1 + clj_rt:count(More).

%% clojerl.IColl

cons(#{?TYPE := ?M} = Cons, X) -> ?CONSTRUCTOR(X, Cons).

empty(_) -> [].

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, first := FirstX, more := MoreX}
     , #{?TYPE := ?M, first := FirstY, more := MoreY}
     ) ->
  clj_rt:equiv(FirstX, FirstY) andalso clj_rt:equiv(MoreX, MoreY);
equiv(#{?TYPE := ?M} = Cons, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> 'erlang.List':equiv(to_list(Cons), clj_rt:seq(Y));
    false -> false
  end.

%% clojerl.IErl

'->erl'(#{?TYPE := ?M} = X, Recursive) ->
  List = to_list(X),
  case Recursive of
    true  -> [clj_rt:'->erl'(Item, true) || Item <- List];
    false -> List
  end.

%% clojerl.IHash

hash(#{?TYPE := ?M, first := First, more := More}) ->
  clj_murmur3:ordered({First, More}).

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = List, Metadata) ->
  List#{meta => Metadata}.

%% clojerl.ISeq

first(#{?TYPE := ?M, first := First}) -> First.

next(#{?TYPE := ?M, more := ?NIL}) -> ?NIL;
next(#{?TYPE := ?M, more := More}) -> clj_rt:seq(More).

more(#{?TYPE := ?M, more := ?NIL}) -> [];
more(#{?TYPE := ?M, more := More}) -> More.

%% clojerl.ISequential

'_'(_) -> ?NIL.

%% clojerl.ISeqable

seq(#{?TYPE := ?M} = Cons) -> Cons.

to_list(#{?TYPE := ?M, first := First, more := More}) ->
  [First | clj_rt:to_list(More)].

%% clojerl.IStringable

str(#{?TYPE := ?M} = Cons) ->
  clj_rt:print_str(Cons).
