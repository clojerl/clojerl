-module('erlang.io.IWriter').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['write'/2, 'write'/3]).
-export([?SATISFIES/1]).

-callback 'write'(any(), any()) -> any().
-callback 'write'(any(), any(), any()) -> any().

'write'(Writer, Str) ->
  case Writer of
    #{?TYPE := 'erlang.io.StringWriter'} ->
      'erlang.io.StringWriter':'write'(Writer, Str);
    #{?TYPE := 'erlang.io.File'} ->
      'erlang.io.File':'write'(Writer, Str);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer);
    ZZZ when is_atom(ZZZ) ->
      'clojerl.Keyword':'write'(Writer, Str);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer)
  end.

'write'(Writer, Format, Value) ->
  case Writer of
    #{?TYPE := 'erlang.io.StringWriter'} ->
      'erlang.io.StringWriter':'write'(Writer, Format, Value);
    #{?TYPE := 'erlang.io.File'} ->
      'erlang.io.File':'write'(Writer, Format, Value);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer);
    ZZZ when is_atom(ZZZ) ->
      'clojerl.Keyword':'write'(Writer, Format, Value);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'erlang.io.StringWriter'} -> true;
    #{?TYPE := 'erlang.io.File'} -> true;
    #{?TYPE := _} -> false;
    ?NIL -> false;
    ZZZ when is_atom(ZZZ) -> true;
    _ -> false
  end.
