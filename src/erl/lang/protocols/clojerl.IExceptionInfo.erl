-module('clojerl.IExceptionInfo').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['data'/1, 'cause'/1, '__satisfies?__'/1]).

-callback 'data'(any()) -> any().
-callback 'cause'(any()) -> any().

'data'(ExInfo) ->
  case clj_rt:type_module(ExInfo) of
    'clojerl.ExceptionInfo' ->
      'clojerl.ExceptionInfo':'data'(ExInfo);
    _ ->
      clj_protocol:resolve(?MODULE, 'data', ExInfo)
  end.

'cause'(ExInfo) ->
  case clj_rt:type_module(ExInfo) of
    'clojerl.ExceptionInfo' ->
      'clojerl.ExceptionInfo':'cause'(ExInfo);
    _ ->
      clj_protocol:resolve(?MODULE, 'cause', ExInfo)
  end.

?SATISFIES('clojerl.ExceptionInfo') -> true;
?SATISFIES(_) -> false.
