%%% Code generate by scripts/generate-protocols
-module('erlang.io.IWriter').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['write'/2, 'write'/3]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'write'(any(), any()) -> any().
-callback 'write'(any(), any(), any()) -> any().
-optional_callbacks(['write'/2, 'write'/3]).

'write'(Writer, Str) ->
  case Writer of
    #{?TYPE := 'erlang.io.File'} ->
      'erlang.io.File':'write'(Writer, Str);
    #{?TYPE := 'erlang.io.StringWriter'} ->
      'erlang.io.StringWriter':'write'(Writer, Str);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer);
    X_ when erlang:is_atom(X_) ->
      'clojerl.Keyword':'write'(Writer, Str);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer)
  end.

'write'(Writer, Format, Value) ->
  case Writer of
    #{?TYPE := 'erlang.io.File'} ->
      'erlang.io.File':'write'(Writer, Format, Value);
    #{?TYPE := 'erlang.io.StringWriter'} ->
      'erlang.io.StringWriter':'write'(Writer, Format, Value);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer);
    X_ when erlang:is_atom(X_) ->
      'clojerl.Keyword':'write'(Writer, Format, Value);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'write', Writer)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'erlang.io.File'} ->  true;
    #{?TYPE := 'erlang.io.StringWriter'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    X_ when erlang:is_atom(X_) ->  true;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'erlang.io.File' -> true;
    'erlang.io.StringWriter' -> true;
    'clojerl.Keyword' -> true;
    _ -> false
  end.
