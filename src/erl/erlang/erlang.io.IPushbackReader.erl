%%% Code generate by scripts/generate-protocols
-module('erlang.io.IPushbackReader').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['unread'/2]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'unread'(any(), any()) -> any().
-optional_callbacks(['unread'/2]).

-export_type([type/0]).
-type type() :: #{_ => _}.

'unread'(Reader, Ch) ->
  case Reader of
    #{?TYPE := 'erlang.io.PushbackReader'} ->
      'erlang.io.PushbackReader':'unread'(Reader, Ch);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'unread', Reader);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'unread', Reader);
    X_ when erlang:is_boolean(X_) ->
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
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'erlang.io.PushbackReader' -> true;
    _ -> false
  end.
