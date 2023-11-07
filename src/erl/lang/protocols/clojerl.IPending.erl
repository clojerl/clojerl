%%% Code generate by scripts/generate-protocols
-module('clojerl.IPending').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['realized?'/1]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'realized?'(any()) -> any().
-optional_callbacks(['realized?'/1]).

-export_type([type/0]).
-type type() :: #{_ => _}.

'realized?'(X) ->
  case X of
    #{?TYPE := 'clojerl.Delay'} ->
      'clojerl.Delay':'realized?'(X);
    #{?TYPE := 'clojerl.Future'} ->
      'clojerl.Future':'realized?'(X);
    #{?TYPE := 'clojerl.Promise'} ->
      'clojerl.Promise':'realized?'(X);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'realized?', X);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'realized?', X);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'realized?', X);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'realized?', X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'realized?', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Delay'} ->  true;
    #{?TYPE := 'clojerl.Future'} ->  true;
    #{?TYPE := 'clojerl.Promise'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.Delay' -> true;
    'clojerl.Future' -> true;
    'clojerl.Promise' -> true;
    _ -> false
  end.
