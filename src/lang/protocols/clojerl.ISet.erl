-module('clojerl.ISet').

-export([disjoin/2, contains/2, get/2]).

-type type() :: any().

-callback 'clojerl.ISet.disjoin'(type(), any()) -> type().
-callback 'clojerl.ISet.contains'(type(), any()) -> boolean().
-callback 'clojerl.ISet.get'(type(), any()) -> any().

-spec disjoin(type(), any()) -> type().
disjoin(Coll, Item) ->
  'clojerl.protocol':resolve(?MODULE, disjoin, [Coll, Item]).

-spec contains(type(), any()) -> type().
contains(Coll, Item) ->
  'clojerl.protocol':resolve(?MODULE, contains, [Coll, Item]).

-spec get(type(), any()) -> any().
get(Coll, Item) ->
  'clojerl.protocol':resolve(?MODULE, get, [Coll, Item]).
