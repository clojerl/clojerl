%%% Code generate by scripts/generate-protocols
-module('clojerl.IResolver').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['current_ns'/1, 'resolve_class'/2, 'resolve_alias'/2, 'resolve_var'/2]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'current_ns'(any()) -> any().
-callback 'resolve_class'(any(), any()) -> any().
-callback 'resolve_alias'(any(), any()) -> any().
-callback 'resolve_var'(any(), any()) -> any().
-optional_callbacks(['current_ns'/1, 'resolve_class'/2, 'resolve_alias'/2, 'resolve_var'/2]).

-export_type([type/0]).
-type type() :: #{_ => _}.

'current_ns'(Resolver) ->
  case Resolver of
    #{?TYPE := 'clojerl.DummyResolver'} ->
      'clojerl.DummyResolver':'current_ns'(Resolver);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'current_ns', Resolver);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'current_ns', Resolver);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'current_ns', Resolver);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'current_ns', Resolver);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'current_ns', Resolver)
  end.

'resolve_class'(Resolver, _Symbol) ->
  case Resolver of
    #{?TYPE := 'clojerl.DummyResolver'} ->
      'clojerl.DummyResolver':'resolve_class'(Resolver, _Symbol);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'resolve_class', Resolver);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'resolve_class', Resolver);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'resolve_class', Resolver);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'resolve_class', Resolver);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'resolve_class', Resolver)
  end.

'resolve_alias'(Resolver, _Symbol) ->
  case Resolver of
    #{?TYPE := 'clojerl.DummyResolver'} ->
      'clojerl.DummyResolver':'resolve_alias'(Resolver, _Symbol);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'resolve_alias', Resolver);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'resolve_alias', Resolver);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'resolve_alias', Resolver);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'resolve_alias', Resolver);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'resolve_alias', Resolver)
  end.

'resolve_var'(Resolver, _Symbol) ->
  case Resolver of
    #{?TYPE := 'clojerl.DummyResolver'} ->
      'clojerl.DummyResolver':'resolve_var'(Resolver, _Symbol);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'resolve_var', Resolver);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'resolve_var', Resolver);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'resolve_var', Resolver);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'resolve_var', Resolver);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'resolve_var', Resolver)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.DummyResolver'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.DummyResolver' -> true;
    _ -> false
  end.
