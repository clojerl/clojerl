-module('erlang.io.IReader').

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
  case clj_rt:type_module(Reader) of
    'erlang.io.PushbackReader' ->
      'erlang.io.PushbackReader':'read'(Reader);
    'erlang.io.StringReader' ->
      'erlang.io.StringReader':'read'(Reader);
    'erlang.io.File' ->
      'erlang.io.File':'read'(Reader);
    'clojerl.Keyword' ->
      'clojerl.Keyword':'read'(Reader);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'read', Type)
  end.

'read'(Reader, Length) ->
  case clj_rt:type_module(Reader) of
    'erlang.io.PushbackReader' ->
      'erlang.io.PushbackReader':'read'(Reader, Length);
    'erlang.io.StringReader' ->
      'erlang.io.StringReader':'read'(Reader, Length);
    'erlang.io.File' ->
      'erlang.io.File':'read'(Reader, Length);
    'clojerl.Keyword' ->
      'clojerl.Keyword':'read'(Reader, Length);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'read', Type)
  end.

'read_line'(Reader) ->
  case clj_rt:type_module(Reader) of
    'erlang.io.PushbackReader' ->
      'erlang.io.PushbackReader':'read_line'(Reader);
    'erlang.io.StringReader' ->
      'erlang.io.StringReader':'read_line'(Reader);
    'erlang.io.File' ->
      'erlang.io.File':'read_line'(Reader);
    'clojerl.Keyword' ->
      'clojerl.Keyword':'read_line'(Reader);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'read_line', Type)
  end.

'skip'(Reader, N) ->
  case clj_rt:type_module(Reader) of
    'erlang.io.PushbackReader' ->
      'erlang.io.PushbackReader':'skip'(Reader, N);
    'erlang.io.StringReader' ->
      'erlang.io.StringReader':'skip'(Reader, N);
    'erlang.io.File' ->
      'erlang.io.File':'skip'(Reader, N);
    'clojerl.Keyword' ->
      'clojerl.Keyword':'skip'(Reader, N);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'skip', Type)
  end.

?SATISFIES('erlang.io.PushbackReader') -> true;
?SATISFIES('erlang.io.StringReader') -> true;
?SATISFIES('erlang.io.File') -> true;
?SATISFIES('clojerl.Keyword') -> true;
?SATISFIES(_) -> false.
