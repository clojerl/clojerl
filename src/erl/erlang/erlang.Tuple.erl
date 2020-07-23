-module('erlang.Tuple').

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IEncodeClojure').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.ISequential').
-behavior('clojerl.IIndexed').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

-export([count/1]).
-export(['erl->clj'/2]).
-export([equiv/2]).
-export([hash/1]).
-export([ nth/2
        , nth/3
        ]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.ICounted

count(Tuple) -> tuple_size(Tuple).

%% clojerl.IEncodeErlang

'erl->clj'(Tuple, Recursive) ->
  List0 = tuple_to_list(Tuple),
  List1 = case Recursive of
            true  -> [clj_rt:'erl->clj'(Item, true) || Item <- List0];
            false -> List0
          end,
  'clojerl.Vector':?CONSTRUCTOR(List1).

%% clojerl.IEquiv

equiv(X, Y) when is_tuple(X), is_tuple(Y) ->
  case erlang:tuple_size(X) =:= erlang:tuple_size(Y) of
    true  -> do_equiv(X, Y, erlang:tuple_size(X), 1);
    false -> false
  end;
equiv(X, Y) ->
  case clj_rt:'sequential?'(Y) of
    true  -> 'erlang.List':equiv(to_list(X), Y);
    false -> false
  end.

%% @private
do_equiv(_, _, Size, Index) when Size < Index -> true;
do_equiv(X, Y, Size, Index) ->
  case clj_rt:equiv(element(Index, X), element(Index, Y)) of
    false -> false;
    true  -> do_equiv(X, Y, Size, Index + 1)
  end.

%% clojerl.IHash

hash(Tuple) -> clj_murmur3:ordered(Tuple).

%% clojerl.IIndexed

nth(Tuple, N) ->
  erlang:element(N + 1, Tuple).

nth(Tuple, N, NotFound) ->
  case N >= erlang:size(Tuple) of
    true  -> NotFound;
    false -> erlang:element(N + 1, Tuple)
  end.

%% clojerl.ISeqable

seq({}) -> ?NIL;
seq(Tuple) -> tuple_to_list(Tuple).

to_list(Tuple) -> tuple_to_list(Tuple).

%% clojerl.IStringable

str(Tuple) when is_tuple(Tuple) ->
  clj_rt:print_str(Tuple).
