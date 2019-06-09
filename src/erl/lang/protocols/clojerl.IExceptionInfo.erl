-module('clojerl.IExceptionInfo').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['data'/1, 'cause'/1]).
-export([?SATISFIES/1]).

-callback 'data'(any()) -> any().
-callback 'cause'(any()) -> any().

'data'(ExInfo) ->
  case ExInfo of
    #{?TYPE := 'clojerl.ExceptionInfo'} ->
      'clojerl.ExceptionInfo':'data'(ExInfo);
    #{?TYPE := _} ->
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
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'cause', ExInfo);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'cause', ExInfo)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.ExceptionInfo'} -> true;
    #{?TYPE := _} -> false;
    ?NIL -> false;
    _ -> false
  end.
