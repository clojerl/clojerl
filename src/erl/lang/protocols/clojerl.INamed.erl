-module('clojerl.INamed').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['name'/1, 'namespace'/1]).
-export([?SATISFIES/1]).

-callback 'name'(any()) -> any().
-callback 'namespace'(any()) -> any().

'name'(X) ->
  case X of
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'name'(X);
    #{?TYPE := 'clojerl.Symbol'} ->
      'clojerl.Symbol':'name'(X);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'name', X);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'name', X);
    ZZZ when is_atom(ZZZ) ->
      'clojerl.Keyword':'name'(X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'name', X)
  end.

'namespace'(X) ->
  case X of
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'namespace'(X);
    #{?TYPE := 'clojerl.Symbol'} ->
      'clojerl.Symbol':'namespace'(X);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'namespace', X);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'namespace', X);
    ZZZ when is_atom(ZZZ) ->
      'clojerl.Keyword':'namespace'(X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'namespace', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Var'} -> true;
    #{?TYPE := 'clojerl.Symbol'} -> true;
    #{?TYPE := _} -> false;
    ?NIL -> false;
    ZZZ when is_atom(ZZZ) -> true;
    _ -> false
  end.
