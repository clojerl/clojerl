-module('clojerl.IColl').

-export([count/1, cons/2, equiv/2]).

-type type() :: any().

-callback count(IColl :: type()) -> integer().
-callback cons(IColl :: type(), Item :: any()) -> type().
-callback equiv(IColl :: type(), any()) -> boolean().

-spec count(type()) -> integer().
count(Coll) ->
  'clojerl.protocol':resolve(?MODULE, count, [Coll]).

-spec cons(type(), any()) -> type().
cons(Coll, Item) ->
  'clojerl.protocol':resolve(?MODULE, cons, [Coll, Item]).

-spec equiv(type(), any()) -> boolean().
equiv(Coll, Item) ->
  'clojerl.protocol':resolve(?MODULE, equiv, [Coll, Item]).
