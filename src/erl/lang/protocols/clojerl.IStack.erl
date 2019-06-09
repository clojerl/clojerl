-module('clojerl.IStack').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['peek'/1, 'pop'/1]).
-export([?SATISFIES/1]).

-callback 'peek'(any()) -> any().
-callback 'pop'(any()) -> any().

'peek'(Stack) ->
  case Stack of
    #{?TYPE := 'clojerl.List'} ->
      'clojerl.List':'peek'(Stack);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'peek'(Stack);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'peek', Stack);
    ZZZ when is_list(ZZZ) ->
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
    ZZZ when is_list(ZZZ) ->
      'erlang.List':'pop'(Stack);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'pop', Stack);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'pop', Stack)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.List'} -> true;
    #{?TYPE := 'clojerl.Vector'} -> true;
    #{?TYPE := _} -> false;
    ZZZ when is_list(ZZZ) -> true;
    ?NIL -> false;
    _ -> false
  end.
