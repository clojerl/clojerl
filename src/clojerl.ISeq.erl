-module('clojerl.ISeq').

-export([first/1, next/1]).

-type type() :: any().

-callback first(ISeq :: type()) -> any().
-callback next(ISeq :: type()) -> type().

-spec first(type()) -> any().
first(Seq) ->
  'clojerl.protocol':resolve('ISeq', first, [Seq]).

-spec next(type()) -> type().
next(Seq) ->
  'clojerl.protocol':resolve('ISeq', next, [Seq]).
