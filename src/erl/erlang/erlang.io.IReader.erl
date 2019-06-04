-module('erlang.io.IReader').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['read'/1, 'read'/2, 'read_line'/1, 'skip'/2]).
-export([?SATISFIES/1]).

-callback 'read'(any()) -> any().
-callback 'read'(any(), any()) -> any().
-callback 'read_line'(any()) -> any().
-callback 'skip'(any(), any()) -> any().

'read'(Reader) ->
  case Reader of
    #{?TYPE := 'erlang.io.PushbackReader'} ->
      'erlang.io.PushbackReader':'read'(Reader);
    #{?TYPE := 'erlang.io.StringReader'} ->
      'erlang.io.StringReader':'read'(Reader);
    #{?TYPE := 'erlang.io.File'} ->
      'erlang.io.File':'read'(Reader);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'read', Reader);
    ZZZ when is_atom(ZZZ) ->
      'clojerl.Keyword':'read'(Reader);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'read', Reader)
  end.

'read'(Reader, Length) ->
  case Reader of
    #{?TYPE := 'erlang.io.PushbackReader'} ->
      'erlang.io.PushbackReader':'read'(Reader, Length);
    #{?TYPE := 'erlang.io.StringReader'} ->
      'erlang.io.StringReader':'read'(Reader, Length);
    #{?TYPE := 'erlang.io.File'} ->
      'erlang.io.File':'read'(Reader, Length);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'read', Reader);
    ZZZ when is_atom(ZZZ) ->
      'clojerl.Keyword':'read'(Reader, Length);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'read', Reader)
  end.

'read_line'(Reader) ->
  case Reader of
    #{?TYPE := 'erlang.io.PushbackReader'} ->
      'erlang.io.PushbackReader':'read_line'(Reader);
    #{?TYPE := 'erlang.io.StringReader'} ->
      'erlang.io.StringReader':'read_line'(Reader);
    #{?TYPE := 'erlang.io.File'} ->
      'erlang.io.File':'read_line'(Reader);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'read_line', Reader);
    ZZZ when is_atom(ZZZ) ->
      'clojerl.Keyword':'read_line'(Reader);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'read_line', Reader)
  end.

'skip'(Reader, N) ->
  case Reader of
    #{?TYPE := 'erlang.io.PushbackReader'} ->
      'erlang.io.PushbackReader':'skip'(Reader, N);
    #{?TYPE := 'erlang.io.StringReader'} ->
      'erlang.io.StringReader':'skip'(Reader, N);
    #{?TYPE := 'erlang.io.File'} ->
      'erlang.io.File':'skip'(Reader, N);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'skip', Reader);
    ZZZ when is_atom(ZZZ) ->
      'clojerl.Keyword':'skip'(Reader, N);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'skip', Reader)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'erlang.io.PushbackReader'} -> true;
    #{?TYPE := 'erlang.io.StringReader'} -> true;
    #{?TYPE := 'erlang.io.File'} -> true;
    #{?TYPE := _} -> false;
    ZZZ when is_atom(ZZZ) -> true;
    _ -> false
  end.
