-module('clojerl.IEncodeClojure').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['erl->clj'/2]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'erl->clj'(any(), any()) -> any().
-optional_callbacks(['erl->clj'/2]).

'erl->clj'(X, Recursive) ->
  case X of
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'erl->clj', X);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'erl->clj', X);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'erl->clj', X);
    X_ when erlang:is_list(X_) ->
      'erlang.List':'erl->clj'(X, Recursive);
    X_ when erlang:is_map(X_) ->
      'erlang.Map':'erl->clj'(X, Recursive);
    X_ when erlang:is_tuple(X_) ->
      'erlang.Tuple':'erl->clj'(X, Recursive);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'erl->clj', X);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'erl->clj', X)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    X_ when erlang:is_list(X_) ->  true;
    X_ when erlang:is_map(X_) ->  true;
    X_ when erlang:is_tuple(X_) ->  true;
    ?NIL ->  false;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'erlang.List' -> true;
    'erlang.Map' -> true;
    'erlang.Tuple' -> true;
    _ -> false
  end.
