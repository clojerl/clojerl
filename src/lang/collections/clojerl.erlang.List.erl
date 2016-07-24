-module('clojerl.erlang.List').

-behavior('clojerl.Counted').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IColl').
-behavior('clojerl.IHash').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.IStack').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([count/1]).
-export([equiv/2]).
-export([ cons/2
        , empty/1
        ]).
-export([hash/1]).
-export([ first/1
        , more/1
        , next/1
        ]).
-export([noop/1]).
-export([ peek/1
        , pop/1
        ]).
-export([seq/1]).
-export([str/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(Items) -> length(Items).

hash(List) -> clj_murmur3:ordered(List).

str([]) ->
  <<"()">>;
str(Items) when is_list(Items) ->
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs = clj_utils:binary_join(ItemsStrs, <<" ">>),
  <<"(", Strs/binary, ")">>.

seq([]) -> undefined;
seq(List) -> List.

first([]) -> undefined;
first([First | _]) -> First.

more([]) -> [];
more([_ | Rest]) -> Rest.

next([]) -> undefined;
next([_ | []]) -> undefined;
next([_ | Rest]) -> Rest.

noop(_) -> ok.

peek([]) -> undefined;
peek([X | _]) -> X.

pop([]) -> [];
pop([_ | Rest]) -> Rest.

cons([], X) ->
  [X];
cons(Items, X) ->
  [X | Items].

empty(_) -> [].

equiv(X, Y) when is_list(X), is_list(Y) ->
  case length(X) =:= length(Y) of
    true  -> do_equiv(X, Y);
    false -> false
  end;
equiv(X, Y) ->
  case clj_core:'sequential?'(Y) of
    true  -> clj_core:equiv(Y, X);
    false -> false
  end.

do_equiv([], []) ->
  true;
do_equiv([X | TailX], [Y | TailY]) ->
  case clj_core:equiv(X, Y) of
    true  -> do_equiv(TailX, TailY);
    false -> false
  end.
