-module('clojerl.IError').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['message'/1]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'message'(any()) -> any().
-optional_callbacks(['message'/1]).

'message'(Error) ->
  case Error of
    #{?TYPE := 'clojerl.ArityError'} ->
      'clojerl.ArityError':'message'(Error);
    #{?TYPE := 'clojerl.AssertionError'} ->
      'clojerl.AssertionError':'message'(Error);
    #{?TYPE := 'clojerl.BadArgumentError'} ->
      'clojerl.BadArgumentError':'message'(Error);
    #{?TYPE := 'clojerl.Error'} ->
      'clojerl.Error':'message'(Error);
    #{?TYPE := 'clojerl.ExceptionInfo'} ->
      'clojerl.ExceptionInfo':'message'(Error);
    #{?TYPE := 'clojerl.IOError'} ->
      'clojerl.IOError':'message'(Error);
    #{?TYPE := 'clojerl.IllegalAccessError'} ->
      'clojerl.IllegalAccessError':'message'(Error);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'message', Error);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'message', Error);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'message', Error);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'message', Error);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'message', Error)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.ArityError'} ->  true;
    #{?TYPE := 'clojerl.AssertionError'} ->  true;
    #{?TYPE := 'clojerl.BadArgumentError'} ->  true;
    #{?TYPE := 'clojerl.Error'} ->  true;
    #{?TYPE := 'clojerl.ExceptionInfo'} ->  true;
    #{?TYPE := 'clojerl.IOError'} ->  true;
    #{?TYPE := 'clojerl.IllegalAccessError'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.ArityError' -> true;
    'clojerl.AssertionError' -> true;
    'clojerl.BadArgumentError' -> true;
    'clojerl.Error' -> true;
    'clojerl.ExceptionInfo' -> true;
    'clojerl.IOError' -> true;
    'clojerl.IllegalAccessError' -> true;
    _ -> false
  end.
