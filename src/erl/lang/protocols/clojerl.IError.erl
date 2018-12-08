-module('clojerl.IError').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['message'/1]).
-export([?SATISFIES/1]).

-callback 'message'(any()) -> any().

'message'(Error) ->
  case clj_rt:type_module(Error) of
    'clojerl.IllegalAccessError' ->
      'clojerl.IllegalAccessError':'message'(Error);
    'clojerl.AssertionError' ->
      'clojerl.AssertionError':'message'(Error);
    'clojerl.Error' ->
      'clojerl.Error':'message'(Error);
    'clojerl.BadArgumentError' ->
      'clojerl.BadArgumentError':'message'(Error);
    'clojerl.ArityError' ->
      'clojerl.ArityError':'message'(Error);
    'clojerl.IOError' ->
      'clojerl.IOError':'message'(Error);
    'clojerl.ExceptionInfo' ->
      'clojerl.ExceptionInfo':'message'(Error);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'message', Type)
  end.

?SATISFIES('clojerl.IllegalAccessError') -> true;
?SATISFIES('clojerl.AssertionError') -> true;
?SATISFIES('clojerl.Error') -> true;
?SATISFIES('clojerl.BadArgumentError') -> true;
?SATISFIES('clojerl.ArityError') -> true;
?SATISFIES('clojerl.IOError') -> true;
?SATISFIES('clojerl.ExceptionInfo') -> true;
?SATISFIES(_) -> false.
