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
    'clojerl.Keyword' ->
      'clojerl.Keyword':'write'(Writer, Str);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'write', Type)
  end.

'write'(Writer, Format, Value) ->
  case clj_rt:type_module(Writer) of
    'erlang.io.File' ->
      'erlang.io.File':'write'(Writer, Format, Value);
    'erlang.io.StringWriter' ->
      'erlang.io.StringWriter':'write'(Writer, Format, Value);
    'clojerl.Keyword' ->
      'clojerl.Keyword':'write'(Writer, Format, Value);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'write', Type)
  end.

?SATISFIES('erlang.io.File') -> true;
?SATISFIES('erlang.io.StringWriter') -> true;
?SATISFIES('clojerl.Keyword') -> true;
?SATISFIES(_) -> false.
