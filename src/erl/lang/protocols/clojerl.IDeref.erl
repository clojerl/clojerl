-module('clojerl.IDeref').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['deref'/1]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'deref'(any()) -> any().
-optional_callbacks(['deref'/1]).

'deref'(Ref) ->
  case Ref of
    #{?TYPE := 'clojerl.Atom'} ->
      'clojerl.Atom':'deref'(Ref);
    #{?TYPE := 'clojerl.Delay'} ->
      'clojerl.Delay':'deref'(Ref);
    #{?TYPE := 'clojerl.ProcessVal'} ->
      'clojerl.ProcessVal':'deref'(Ref);
    #{?TYPE := 'clojerl.Reduced'} ->
      'clojerl.Reduced':'deref'(Ref);
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'deref'(Ref);
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
    #{?TYPE := 'clojerl.Atom'} ->  true;
    #{?TYPE := 'clojerl.Delay'} ->  true;
    #{?TYPE := 'clojerl.ProcessVal'} ->  true;
    #{?TYPE := 'clojerl.Reduced'} ->  true;
    #{?TYPE := 'clojerl.Var'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.Atom' -> true;
    'clojerl.Delay' -> true;
    'clojerl.ProcessVal' -> true;
    'clojerl.Reduced' -> true;
    'clojerl.Var' -> true;
    _ -> false
  end.
