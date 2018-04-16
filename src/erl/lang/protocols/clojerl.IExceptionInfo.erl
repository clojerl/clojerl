-module('clojerl.IExceptionInfo').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['data'/1, 'cause'/1]).
-export([?SATISFIES/1]).

-callback 'data'(any()) -> any().
-callback 'cause'(any()) -> any().

'data'(ExInfo) ->
  case clj_rt:type_module(ExInfo) of
    'clojerl.ExceptionInfo' ->
      'clojerl.ExceptionInfo':'data'(ExInfo);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'data', Type)
  end.

'cause'(ExInfo) ->
  case clj_rt:type_module(ExInfo) of
    'clojerl.ExceptionInfo' ->
      'clojerl.ExceptionInfo':'cause'(ExInfo);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'cause', Type)
  end.

?SATISFIES('clojerl.ExceptionInfo') -> true;
?SATISFIES(_) -> false.
