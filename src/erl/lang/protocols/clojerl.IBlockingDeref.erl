%%% Code generate by scripts/generate-protocols
-module('clojerl.IBlockingDeref').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['deref'/3]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'deref'(any(), any(), any()) -> any().
-optional_callbacks(['deref'/3]).

'deref'(Ref, TimeoutMs, TimeoutValue) ->
  case Ref of
    #{?TYPE := 'clojerl.Future'} ->
      'clojerl.Future':'deref'(Ref, TimeoutMs, TimeoutValue);
    #{?TYPE := 'clojerl.Promise'} ->
      'clojerl.Promise':'deref'(Ref, TimeoutMs, TimeoutValue);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'deref', Ref);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'deref', Ref);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'deref', Ref);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'deref', Ref);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'deref', Ref)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Future'} ->  true;
    #{?TYPE := 'clojerl.Promise'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.Future' -> true;
    'clojerl.Promise' -> true;
    _ -> false
  end.
