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
    #{?TYPE := 'clojerl.IllegalAccessError'} ->
      'clojerl.IllegalAccessError':'message'(Error);
    #{?TYPE := 'clojerl.AssertionError'} ->
      'clojerl.AssertionError':'message'(Error);
    #{?TYPE := 'clojerl.Error'} ->
      'clojerl.Error':'message'(Error);
    #{?TYPE := 'clojerl.BadArgumentError'} ->
      'clojerl.BadArgumentError':'message'(Error);
    #{?TYPE := 'clojerl.ArityError'} ->
      'clojerl.ArityError':'message'(Error);
    #{?TYPE := 'clojerl.IOError'} ->
      'clojerl.IOError':'message'(Error);
    #{?TYPE := 'clojerl.ExceptionInfo'} ->
      'clojerl.ExceptionInfo':'message'(Error);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'message', Error);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'message', Error)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.IllegalAccessError'} -> true;
    #{?TYPE := 'clojerl.AssertionError'} -> true;
    #{?TYPE := 'clojerl.Error'} -> true;
    #{?TYPE := 'clojerl.BadArgumentError'} -> true;
    #{?TYPE := 'clojerl.ArityError'} -> true;
    #{?TYPE := 'clojerl.IOError'} -> true;
    #{?TYPE := 'clojerl.ExceptionInfo'} -> true;
    #{?TYPE := _} -> false;
    _ -> false
  end.
