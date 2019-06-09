-module('erlang.io.IPushbackReader').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['unread'/2]).
-export([?SATISFIES/1]).

-callback 'unread'(any(), any()) -> any().

'unread'(Reader, Ch) ->
  case Reader of
    #{?TYPE := 'erlang.io.PushbackReader'} ->
      'erlang.io.PushbackReader':'unread'(Reader, Ch);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'unread', Reader);
    X_ when is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'unread', Reader);
    X_ when is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'unread', Reader);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'unread', Reader);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'unread', Reader)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'erlang.io.PushbackReader'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when is_binary(X_) ->  false;
    X_ when is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.
