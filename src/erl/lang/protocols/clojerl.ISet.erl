-module('clojerl.ISet').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['disjoin'/2, 'contains'/2, 'get'/2]).
-export([?SATISFIES/1]).

-callback 'disjoin'(any(), any()) -> any().
-callback 'contains'(any(), any()) -> any().
-callback 'get'(any(), any()) -> any().

'disjoin'(Coll, Item) ->
  case clj_rt:type_module(Coll) of
    'clojerl.Set' ->
      'clojerl.Set':'disjoin'(Coll, Item);
    'clojerl.SortedSet' ->
      'clojerl.SortedSet':'disjoin'(Coll, Item);
    _ ->
      clj_protocol:resolve(?MODULE, 'disjoin', Coll, Item)
  end.

'contains'(Coll, Item) ->
  case clj_rt:type_module(Coll) of
    'clojerl.Set' ->
      'clojerl.Set':'contains'(Coll, Item);
    'clojerl.SortedSet' ->
      'clojerl.SortedSet':'contains'(Coll, Item);
    _ ->
      clj_protocol:resolve(?MODULE, 'contains', Coll, Item)
  end.

'get'(Coll, Item) ->
  case clj_rt:type_module(Coll) of
    'clojerl.Set' ->
      'clojerl.Set':'get'(Coll, Item);
    'clojerl.SortedSet' ->
      'clojerl.SortedSet':'get'(Coll, Item);
    _ ->
      clj_protocol:resolve(?MODULE, 'get', Coll, Item)
  end.

?SATISFIES('clojerl.Set') -> true;
?SATISFIES('clojerl.SortedSet') -> true;
?SATISFIES(_) -> false.
