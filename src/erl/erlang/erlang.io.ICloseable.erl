-module('erlang.io.ICloseable').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['close'/1]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'close'(any()) -> any().

'close'(X) ->
  case X of
    #{?TYPE := 'erlang.io.PushbackReader'} ->
      'erlang.io.PushbackReader':'close'(X);
    #{?TYPE := 'clojerl.Delay'} ->
      'clojerl.Delay':'close'(X);
    #{?TYPE := 'erlang.io.StringWriter'} ->
      'erlang.io.StringWriter':'close'(X);
    #{?TYPE := 'erlang.io.File'} ->
      'erlang.io.File':'close'(X);
    #{?TYPE := 'erlang.io.StringReader'} ->
      'erlang.io.StringReader':'close'(X);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'close', X);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'close', X);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'close', X);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'close', X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'close', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'erlang.io.PushbackReader'} ->  true;
    #{?TYPE := 'clojerl.Delay'} ->  true;
    #{?TYPE := 'erlang.io.StringWriter'} ->  true;
    #{?TYPE := 'erlang.io.File'} ->  true;
    #{?TYPE := 'erlang.io.StringReader'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'erlang.io.PushbackReader' -> true;
    'clojerl.Delay' -> true;
    'erlang.io.StringWriter' -> true;
    'erlang.io.File' -> true;
    'erlang.io.StringReader' -> true;
    _ -> false
  end.
