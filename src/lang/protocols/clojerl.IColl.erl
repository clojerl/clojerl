-module('clojerl.IColl').

-export([count/1, cons/2, empty/1, equiv/2]).

-type type() :: any().

-callback 'clojerl.IColl.count'(IColl :: type()) -> integer().
-callback 'clojerl.IColl.cons'(IColl :: type(), Item :: any()) -> type().
-callback 'clojerl.IColl.empty'(IColl :: type()) -> any().
-callback 'clojerl.IColl.equiv'(IColl :: type(), any()) -> boolean().

-spec count(type()) -> integer().
count(Coll) ->
  'clojerl.protocol':resolve(?MODULE, count, [Coll]).

-spec cons(type(), any()) -> type().
cons(Coll, Item) ->
  'clojerl.protocol':resolve(?MODULE, cons, [Coll, Item]).

-spec empty(type()) -> any().
empty(Coll) ->
  'clojerl.protocol':resolve(?MODULE, empty, [Coll]).

-spec equiv(type(), any()) -> boolean().
equiv(Coll, Item) ->
  'clojerl.protocol':resolve(?MODULE, equiv, [Coll, Item]).
