-module('clojerl.INamed').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['name'/1, 'namespace'/1]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'name'(any()) -> any().
-callback 'namespace'(any()) -> any().
-optional_callbacks(['name'/1, 'namespace'/1]).

'name'(X) ->
  case X of
    #{?TYPE := 'clojerl.Symbol'} ->
      'clojerl.Symbol':'name'(X);
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'name'(X);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'name', X);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'name', X);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'name', X);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'name', X);
    X_ when erlang:is_atom(X_) ->
      'clojerl.Keyword':'name'(X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'name', X)
  end.

'namespace'(X) ->
  case X of
    #{?TYPE := 'clojerl.Symbol'} ->
      'clojerl.Symbol':'namespace'(X);
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'namespace'(X);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'namespace', X);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'namespace', X);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'namespace', X);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'namespace', X);
    X_ when erlang:is_atom(X_) ->
      'clojerl.Keyword':'namespace'(X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'namespace', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Symbol'} ->  true;
    #{?TYPE := 'clojerl.Var'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    X_ when erlang:is_atom(X_) ->  true;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.Symbol' -> true;
    'clojerl.Var' -> true;
    'clojerl.Keyword' -> true;
    _ -> false
  end.
