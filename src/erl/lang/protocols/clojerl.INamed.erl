-module('clojerl.INamed').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['name'/1, 'namespace'/1]).
-export([?SATISFIES/1]).

-callback 'name'(any()) -> any().
-callback 'namespace'(any()) -> any().

'name'(X) ->
  case clj_rt:type_module(X) of
    'clojerl.Var' ->
      'clojerl.Var':'name'(X);
    'clojerl.Keyword' ->
      'clojerl.Keyword':'name'(X);
    'clojerl.Symbol' ->
      'clojerl.Symbol':'name'(X);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'name', Type)
  end.

'namespace'(X) ->
  case clj_rt:type_module(X) of
    'clojerl.Var' ->
      'clojerl.Var':'namespace'(X);
    'clojerl.Keyword' ->
      'clojerl.Keyword':'namespace'(X);
    'clojerl.Symbol' ->
      'clojerl.Symbol':'namespace'(X);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'namespace', Type)
  end.

?SATISFIES('clojerl.Var') -> true;
?SATISFIES('clojerl.Keyword') -> true;
?SATISFIES('clojerl.Symbol') -> true;
?SATISFIES(_) -> false.
