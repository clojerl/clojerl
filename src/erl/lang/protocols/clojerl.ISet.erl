-module('clojerl.ISet').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['disjoin'/2, 'contains'/2]).
-export([?SATISFIES/1]).

-callback 'disjoin'(any(), any()) -> any().
-callback 'contains'(any(), any()) -> any().

'disjoin'(Coll, Item) ->
  case Coll of
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'disjoin'(Coll, Item);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'disjoin'(Coll, Item);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'disjoin', Coll);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'disjoin', Coll);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'disjoin', Coll)
  end.

'contains'(Coll, Item) ->
  case Coll of
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'contains'(Coll, Item);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'contains'(Coll, Item);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'contains', Coll);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'contains', Coll);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'contains', Coll)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Set'} -> true;
    #{?TYPE := 'clojerl.SortedSet'} -> true;
    #{?TYPE := _} -> false;
    ?NIL -> false;
    _ -> false
  end.
