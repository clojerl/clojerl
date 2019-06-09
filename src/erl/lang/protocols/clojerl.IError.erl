-module('clojerl.IError').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['message'/1]).
-export([?SATISFIES/1]).

-callback 'message'(any()) -> any().

'message'(Error) ->
  case Error of
    #{?TYPE := 'clojerl.IOError'} ->
      'clojerl.IOError':'message'(Error);
    #{?TYPE := 'clojerl.Error'} ->
      'clojerl.Error':'message'(Error);
    #{?TYPE := 'clojerl.ExceptionInfo'} ->
      'clojerl.ExceptionInfo':'message'(Error);
    #{?TYPE := 'clojerl.IllegalAccessError'} ->
      'clojerl.IllegalAccessError':'message'(Error);
    #{?TYPE := 'clojerl.BadArgumentError'} ->
      'clojerl.BadArgumentError':'message'(Error);
    #{?TYPE := 'clojerl.AssertionError'} ->
      'clojerl.AssertionError':'message'(Error);
    #{?TYPE := 'clojerl.ArityError'} ->
      'clojerl.ArityError':'message'(Error);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'message', Error);
    X_ when is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'message', Error);
    X_ when is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'message', Error);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'message', Error);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'message', Error)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.IOError'} ->  true;
    #{?TYPE := 'clojerl.Error'} ->  true;
    #{?TYPE := 'clojerl.ExceptionInfo'} ->  true;
    #{?TYPE := 'clojerl.IllegalAccessError'} ->  true;
    #{?TYPE := 'clojerl.BadArgumentError'} ->  true;
    #{?TYPE := 'clojerl.AssertionError'} ->  true;
    #{?TYPE := 'clojerl.ArityError'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when is_binary(X_) ->  false;
    X_ when is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.
