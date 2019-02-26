-module('erlang.io.ICloseable').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['close'/1]).
-export([?SATISFIES/1]).

-callback 'close'(any()) -> any().

'close'(X) ->
  case clj_rt:type_module(X) of
    'erlang.io.PushbackReader' ->
      'erlang.io.PushbackReader':'close'(X);
    'erlang.io.StringReader' ->
      'erlang.io.StringReader':'close'(X);
    'erlang.io.StringWriter' ->
      'erlang.io.StringWriter':'close'(X);
    'erlang.io.File' ->
      'erlang.io.File':'close'(X);
    'clojerl.Delay' ->
      'clojerl.Delay':'close'(X);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'close', Type)
  end.

?SATISFIES('erlang.io.PushbackReader') -> true;
?SATISFIES('erlang.io.StringReader') -> true;
?SATISFIES('erlang.io.StringWriter') -> true;
?SATISFIES('erlang.io.File') -> true;
?SATISFIES('clojerl.Delay') -> true;
?SATISFIES(_) -> false.
