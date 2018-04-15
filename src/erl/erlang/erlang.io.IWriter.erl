-module('erlang.io.IWriter').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['write'/2, 'write'/3]).
-export([?SATISFIES/1]).

-callback 'write'(any(), any()) -> any().
-callback 'write'(any(), any(), any()) -> any().

'write'(Writer, Str) ->
  case clj_rt:type_module(Writer) of
    'erlang.io.File' ->
      'erlang.io.File':'write'(Writer, Str);
    'erlang.io.StringWriter' ->
      'erlang.io.StringWriter':'write'(Writer, Str);
    _ ->
      clj_protocol:resolve(?MODULE, 'write', Writer, Str)
  end.

'write'(Writer, Format, Value) ->
  case clj_rt:type_module(Writer) of
    'erlang.io.File' ->
      'erlang.io.File':'write'(Writer, Format, Value);
    'erlang.io.StringWriter' ->
      'erlang.io.StringWriter':'write'(Writer, Format, Value);
    _ ->
      clj_protocol:resolve(?MODULE, 'write', Writer, Format, Value)
  end.

?SATISFIES('erlang.io.File') -> true;
?SATISFIES('erlang.io.StringWriter') -> true;
?SATISFIES(_) -> false.
