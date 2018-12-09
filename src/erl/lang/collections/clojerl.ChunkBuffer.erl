-module('clojerl.ChunkBuffer').

-include("clojerl.hrl").

-export([?CONSTRUCTOR/1]).

-export([ add/2
        , chunk/1
        , count/1
        ]).

-type type() :: #{ ?TYPE  => ?M
                 , buffer => list()
                 }.

-spec ?CONSTRUCTOR(integer()) -> type().
?CONSTRUCTOR(_Capacity) ->
  #{ ?TYPE  => ?M
   , buffer => []
   }.

add(#{?TYPE := ?M, buffer := Items} = X, Item) ->
  X#{buffer := [Item | Items]}.

chunk(#{?TYPE := ?M, buffer := Items}) ->
  Tuple = list_to_tuple(lists:reverse(Items)),
  'clojerl.TupleChunk':?CONSTRUCTOR(Tuple).

count(#{?TYPE := ?M, buffer := Items}) ->
  length(Items).
