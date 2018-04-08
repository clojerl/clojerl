-module('erlang.io.IReader').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['read'/1, 'read'/2, 'read_line'/1, 'skip'/2, '__satisfies?__'/1]).

-callback 'read'(any()) -> any().
-callback 'read'(any(), any()) -> any().
-callback 'read_line'(any()) -> any().
-callback 'skip'(any(), any()) -> any().

'read'(Reader) ->
  case clj_rt:type_module(Reader) of
    'erlang.io.File' ->
      'erlang.io.File':'read'(Reader);
    'erlang.io.PushbackReader' ->
      'erlang.io.PushbackReader':'read'(Reader);
    'erlang.io.StringReader' ->
      'erlang.io.StringReader':'read'(Reader);
    _ ->
      clj_protocol:resolve(?MODULE, 'read', Reader)
  end.

'read'(Reader, Length) ->
  case clj_rt:type_module(Reader) of
    'erlang.io.File' ->
      'erlang.io.File':'read'(Reader, Length);
    'erlang.io.PushbackReader' ->
      'erlang.io.PushbackReader':'read'(Reader, Length);
    'erlang.io.StringReader' ->
      'erlang.io.StringReader':'read'(Reader, Length);
    _ ->
      clj_protocol:resolve(?MODULE, 'read', Reader, Length)
  end.

'read_line'(Reader) ->
  case clj_rt:type_module(Reader) of
    'erlang.io.File' ->
      'erlang.io.File':'read_line'(Reader);
    'erlang.io.PushbackReader' ->
      'erlang.io.PushbackReader':'read_line'(Reader);
    'erlang.io.StringReader' ->
      'erlang.io.StringReader':'read_line'(Reader);
    _ ->
      clj_protocol:resolve(?MODULE, 'read_line', Reader)
  end.

'skip'(Reader, N) ->
  case clj_rt:type_module(Reader) of
    'erlang.io.File' ->
      'erlang.io.File':'skip'(Reader, N);
    'erlang.io.PushbackReader' ->
      'erlang.io.PushbackReader':'skip'(Reader, N);
    'erlang.io.StringReader' ->
      'erlang.io.StringReader':'skip'(Reader, N);
    _ ->
      clj_protocol:resolve(?MODULE, 'skip', Reader, N)
  end.

?SATISFIES('erlang.io.File') -> true;
?SATISFIES('erlang.io.PushbackReader') -> true;
?SATISFIES('erlang.io.StringReader') -> true;
?SATISFIES(_) -> false.
