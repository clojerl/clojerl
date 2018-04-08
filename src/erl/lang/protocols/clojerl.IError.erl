-module('clojerl.IError').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['message'/1, '__satisfies?__'/1]).

-callback 'message'(any()) -> any().

'message'(Error) ->
  case clj_rt:type_module(Error) of
    'clojerl.ArityError' ->
      'clojerl.ArityError':'message'(Error);
    'clojerl.AssertionError' ->
      'clojerl.AssertionError':'message'(Error);
    'clojerl.BadArgumentError' ->
      'clojerl.BadArgumentError':'message'(Error);
    'clojerl.Error' ->
      'clojerl.Error':'message'(Error);
    'clojerl.ExceptionInfo' ->
      'clojerl.ExceptionInfo':'message'(Error);
    'clojerl.IllegalAccessError' ->
      'clojerl.IllegalAccessError':'message'(Error);
    'clojerl.IOError' ->
      'clojerl.IOError':'message'(Error);
    _ ->
      clj_protocol:resolve(?MODULE, 'message', Error)
  end.

?SATISFIES('clojerl.ArityError') -> true;
?SATISFIES('clojerl.AssertionError') -> true;
?SATISFIES('clojerl.BadArgumentError') -> true;
?SATISFIES('clojerl.Error') -> true;
?SATISFIES('clojerl.ExceptionInfo') -> true;
?SATISFIES('clojerl.IllegalAccessError') -> true;
?SATISFIES('clojerl.IOError') -> true;
?SATISFIES(_) -> false.
