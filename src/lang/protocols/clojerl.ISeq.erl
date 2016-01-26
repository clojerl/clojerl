-module('clojerl.ISeq').

-export([first/1, next/1, more/1]).

-type type() :: any().

-callback 'clojerl.ISeq.first'(ISeq :: type()) -> any().
-callback 'clojerl.ISeq.next'(ISeq :: type()) -> type().
-callback 'clojerl.ISeq.more'(ISeq :: type()) -> type().

-spec first(type()) -> any().
first(Seq) ->
  'clojerl.protocol':resolve(?MODULE, first, [Seq]).

-spec next(type()) -> type().
next(Seq) ->
  'clojerl.protocol':resolve(?MODULE, next, [Seq]).

-spec more(type()) -> type().
more(Seq) ->
  'clojerl.protocol':resolve(?MODULE, more, [Seq]).
