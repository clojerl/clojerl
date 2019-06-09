-module('clojerl.IFn').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['apply'/2]).
-export([?SATISFIES/1]).

-callback 'apply'(any(), any()) -> any().

'apply'(Fn, Args) ->
  case Fn of
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.Symbol'} ->
      'clojerl.Symbol':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.Fn'} ->
      'clojerl.Fn':'apply'(Fn, Args);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'apply', Fn);
    ZZZ when is_map(ZZZ) ->
      'erlang.Map':'apply'(Fn, Args);
    ZZZ when is_function(ZZZ) ->
      'erlang.Fn':'apply'(Fn, Args);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'apply', Fn);
    ZZZ when is_atom(ZZZ) ->
      'clojerl.Keyword':'apply'(Fn, Args);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'apply', Fn)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Var'} -> true;
    #{?TYPE := 'clojerl.Symbol'} -> true;
    #{?TYPE := 'clojerl.SortedMap'} -> true;
    #{?TYPE := 'clojerl.TupleMap'} -> true;
    #{?TYPE := 'clojerl.Vector'} -> true;
    #{?TYPE := 'clojerl.Map'} -> true;
    #{?TYPE := 'clojerl.Set'} -> true;
    #{?TYPE := 'clojerl.SortedSet'} -> true;
    #{?TYPE := 'clojerl.Fn'} -> true;
    #{?TYPE := _} -> false;
    ZZZ when is_map(ZZZ) -> true;
    ZZZ when is_function(ZZZ) -> true;
    ?NIL -> false;
    ZZZ when is_atom(ZZZ) -> true;
    _ -> false
  end.
