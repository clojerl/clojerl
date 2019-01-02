-module('clojerl.Vector.RSeq').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behavior('clojerl.ICounted'). %
-behavior('clojerl.IColl'). %
-behavior('clojerl.IEquiv'). %
-behavior('clojerl.IErl').
-behavior('clojerl.IHash'). %
-behavior('clojerl.IMeta'). %
-behavior('clojerl.ISequential'). %
-behavior('clojerl.ISeqable'). %
-behavior('clojerl.ISeq'). %
-behavior('clojerl.IStringable'). %

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
                 , array => array:array()
                 , index => integer()
                 , meta  => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(array:array(), integer()) -> type().
?CONSTRUCTOR(Array, Index) when Index >= 0 ->
  #{ ?TYPE => ?M
   , array => Array
   , index => Index
   , meta  => ?NIL
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(#{?TYPE := ?M, index := Index}) -> Index + 1.

%% clojerl.IColl

cons(#{?TYPE := ?M} = X, Item) ->
  clj_rt:cons(Item, X).

empty(_) -> [].

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, index := IndexX} = X
     , #{?TYPE := ?M, index := IndexY} = Y
     ) ->
  case IndexX == IndexY of
    true  -> 'erlang.List':equiv(to_list(X), to_list(Y));
    false -> false
  end;
equiv(#{?TYPE := ?M} = X, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> 'erlang.List':equiv(to_list(X), Y);
    false -> false
  end.

%% clojerl.IErl

'->erl'(#{?TYPE := ?M} = X, Recursive) ->
  List0 = to_list(X),
  case Recursive of
    true  -> [clj_rt:'->erl'(Item, true) || Item <- List0];
    false -> List0
  end.

%% clojerl.IHash

hash(#{?TYPE := ?M} = X) ->
  clj_murmur3:ordered(to_list(X)).

%% clojerl.IMeta

meta(#{?TYPE := ?M, meta := Meta}) -> Meta.

with_meta(#{?TYPE := ?M} = X, Meta) ->
  X#{meta => Meta}.

%% clojerl.ISeq

first(#{?TYPE := ?M, index := Index, array := Array}) ->
  array:get(Index, Array).

next(#{?TYPE := ?M, index := Index}) when Index =< 0 -> ?NIL;
next(#{?TYPE := ?M, index := Index} = X) ->
  X#{index => Index - 1}.

more(#{?TYPE := ?M, index := Index}) when Index =< 0 ->
  [];
more(#{?TYPE := ?M, index := Index} = X) ->
  X#{index => Index - 1}.

%% clojerl.ISequential

'_'(_) -> ?NIL.

%% clojerl.ISeqable

seq(#{?TYPE := ?M} = X) -> X.

to_list(#{?TYPE := ?M, array := Array, index := Index}) ->
  do_to_list(Array, 0, Index, []).

do_to_list(_Array, Current, End, Result) when Current > End ->
  Result;
do_to_list(Array, Current, End, Result) ->
  Item = array:get(Current, Array),
  do_to_list(Array, Current + 1, End, [Item | Result]).

%% clojerl.IStringable

str(#{?TYPE := ?M} = X) ->
  clj_rt:print_str(clj_rt:list(to_list(X))).
