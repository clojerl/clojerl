-module('clojerl.IExceptionInfo').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['data'/1, 'cause'/1]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'data'(any()) -> any().
-callback 'cause'(any()) -> any().
-optional_callbacks(['data'/1, 'cause'/1]).

'data'(ExInfo) ->
  case ExInfo of
    #{?TYPE := 'clojerl.ExceptionInfo'} ->
      'clojerl.ExceptionInfo':'data'(ExInfo);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'data', ExInfo);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'data', ExInfo);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'data', ExInfo);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'data', ExInfo);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'data', ExInfo)
  end.

'cause'(ExInfo) ->
  case ExInfo of
    #{?TYPE := 'clojerl.ExceptionInfo'} ->
      'clojerl.ExceptionInfo':'cause'(ExInfo);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'cause', ExInfo);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'cause', ExInfo);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'cause', ExInfo);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'cause', ExInfo);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'cause', ExInfo)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.ExceptionInfo'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.ExceptionInfo' -> true;
    _ -> false
  end.
