-module('clojerl.IStack').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['peek'/1, 'pop'/1]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'peek'(any()) -> any().
-callback 'pop'(any()) -> any().
-optional_callbacks(['peek'/1, 'pop'/1]).

'peek'(Stack) ->
  case Stack of
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'peek'(Stack);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'peek'(Stack);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'peek', Stack);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'peek', Stack);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'peek', Stack);
    X_ when erlang:is_list(X_) ->
      'erlang.List':'peek'(Stack);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'peek', Stack);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'peek', Stack)
  end.

'pop'(Stack) ->
  case Stack of
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'pop'(Stack);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'pop'(Stack);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'pop', Stack);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'pop', Stack);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'pop', Stack);
    X_ when erlang:is_list(X_) ->
      'erlang.List':'pop'(Stack);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'pop', Stack);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'pop', Stack)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.List'} ->  true;
    #{?TYPE := 'clojerl.Vector'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    X_ when erlang:is_list(X_) ->  true;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.List' -> true;
    'clojerl.Vector' -> true;
    'erlang.List' -> true;
    _ -> false
  end.
