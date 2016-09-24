-module('clojerl.ISet').

-clojure(true).
-protocol(true).

-export([disjoin/2, contains/2, get/2]).

-type type() :: any().

-callback disjoin(type(), any()) -> type().
-callback contains(type(), any()) -> boolean().
-callback get(type(), any()) -> any().

-spec disjoin(type(), any()) -> type().
disjoin(Coll, Item) ->
  'clojerl.protocol':resolve(?MODULE, disjoin, [Coll, Item]).

-spec contains(type(), any()) -> type().
contains(Coll, Item) ->
  'clojerl.protocol':resolve(?MODULE, contains, [Coll, Item]).

-spec get(type(), any()) -> any().
get(Coll, Item) ->
  'clojerl.protocol':resolve(?MODULE, get, [Coll, Item]).
