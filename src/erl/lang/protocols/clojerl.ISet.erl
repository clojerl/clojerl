%%% Code generate by scripts/generate-protocols
-module('clojerl.ISet').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['disjoin'/2, 'contains'/2]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'disjoin'(any(), any()) -> any().
-callback 'contains'(any(), any()) -> any().
-optional_callbacks(['disjoin'/2, 'contains'/2]).

-export_type([type/0]).
-type type() :: #{_ => _}.

'disjoin'(Coll, Item) ->
  case Coll of
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'disjoin'(Coll, Item);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'disjoin'(Coll, Item);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'disjoin', Coll);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'disjoin', Coll);
    X_ when erlang:is_boolean(X_) ->
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
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'contains', Coll);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'contains', Coll);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'contains', Coll);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'contains', Coll)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Set'} ->  true;
    #{?TYPE := 'clojerl.SortedSet'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.Set' -> true;
    'clojerl.SortedSet' -> true;
    _ -> false
  end.
