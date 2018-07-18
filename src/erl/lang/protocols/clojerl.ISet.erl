-module('clojerl.ISet').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['disjoin'/2, 'contains'/2]).
-export([?SATISFIES/1]).

-callback 'disjoin'(any(), any()) -> any().
-callback 'contains'(any(), any()) -> any().

'disjoin'(Coll, Item) ->
  case clj_rt:type_module(Coll) of
    'clojerl.Set' ->
      'clojerl.Set':'disjoin'(Coll, Item);
    'clojerl.SortedSet' ->
      'clojerl.SortedSet':'disjoin'(Coll, Item);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'disjoin', Type)
  end.

'contains'(Coll, Item) ->
  case clj_rt:type_module(Coll) of
    'clojerl.Set' ->
      'clojerl.Set':'contains'(Coll, Item);
    'clojerl.SortedSet' ->
      'clojerl.SortedSet':'contains'(Coll, Item);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'contains', Type)
  end.

?SATISFIES('clojerl.Set') -> true;
?SATISFIES('clojerl.SortedSet') -> true;
?SATISFIES(_) -> false.
