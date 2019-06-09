-module('erlang.io.ICloseable').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['close'/1]).
-export([?SATISFIES/1]).

-callback 'close'(any()) -> any().

'close'(X) ->
  case X of
    #{?TYPE := 'erlang.io.PushbackReader'} ->
      'erlang.io.PushbackReader':'close'(X);
    #{?TYPE := 'erlang.io.StringReader'} ->
      'erlang.io.StringReader':'close'(X);
    #{?TYPE := 'erlang.io.StringWriter'} ->
      'erlang.io.StringWriter':'close'(X);
    #{?TYPE := 'erlang.io.File'} ->
      'erlang.io.File':'close'(X);
    #{?TYPE := 'clojerl.Delay'} ->
      'clojerl.Delay':'close'(X);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'close', X);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'close', X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'close', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'erlang.io.PushbackReader'} -> true;
    #{?TYPE := 'erlang.io.StringReader'} -> true;
    #{?TYPE := 'erlang.io.StringWriter'} -> true;
    #{?TYPE := 'erlang.io.File'} -> true;
    #{?TYPE := 'clojerl.Delay'} -> true;
    #{?TYPE := _} -> false;
    ?NIL -> false;
    _ -> false
  end.
