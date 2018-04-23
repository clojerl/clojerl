-module('clojerl.IStack').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['peek'/1, 'pop'/1]).
-export([?SATISFIES/1]).

-callback 'peek'(any()) -> any().
-callback 'pop'(any()) -> any().

'peek'(Stack) ->
  case clj_rt:type_module(Stack) of
    'erlang.List' ->
      'erlang.List':'peek'(Stack);
    'clojerl.List' ->
      'clojerl.List':'peek'(Stack);
    'clojerl.Vector' ->
      'clojerl.Vector':'peek'(Stack);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'peek', Type)
  end.

'pop'(Stack) ->
  case clj_rt:type_module(Stack) of
    'erlang.List' ->
      'erlang.List':'pop'(Stack);
    'clojerl.List' ->
      'clojerl.List':'pop'(Stack);
    'clojerl.Vector' ->
      'clojerl.Vector':'pop'(Stack);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'pop', Type)
  end.

?SATISFIES('erlang.List') -> true;
?SATISFIES('clojerl.List') -> true;
?SATISFIES('clojerl.Vector') -> true;
?SATISFIES(_) -> false.
