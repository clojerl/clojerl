-module('clojerl.erlang.Tuple').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.ISequential').
-behavior('clojerl.Indexed').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([count/1]).
-export([equiv/2]).
-export([hash/1]).
-export(['_'/1]).
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

count(Tuple) -> tuple_size(Tuple).

equiv(X, Y) when is_tuple(X), is_tuple(Y) ->
  case erlang:tuple_size(X) =:= erlang:tuple_size(Y) of
    true  -> do_equiv(X, Y, erlang:tuple_size(X), 1);
    false -> false
  end;
equiv(X, Y) ->
  case clj_core:'sequential?'(Y) of
    true  -> 'clojerl.erlang.List':equiv(to_list(X), Y);
    false -> false
  end.

%% @private
do_equiv(_, _, Size, Index) when Size < Index -> true;
do_equiv(X, Y, Size, Index) ->
  case clj_core:equiv(element(Index, X), element(Index, Y)) of
    false -> false;
    true  -> do_equiv(X, Y, Size, Index + 1)
  end.

hash(Tuple) -> clj_murmur3:ordered(Tuple).

'_'(_) -> ?NIL.

nth(Tuple, N) ->
  erlang:element(N + 1, Tuple).

nth(Tuple, N, NotFound) ->
  case N >= erlang:size(Tuple) of
    true  -> NotFound;
    false -> erlang:element(N + 1, Tuple)
  end.

seq({}) -> ?NIL;
seq(Tuple) -> tuple_to_list(Tuple).

to_list(Tuple) -> tuple_to_list(Tuple).

str(Tuple) when is_tuple(Tuple) ->
  Items     = tuple_to_list(Tuple),
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs      = 'clojerl.String':join(ItemsStrs, <<", ">>),
  <<"#erl[", Strs/binary, "]">>.
