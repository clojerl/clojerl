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
    'clojerl.Keyword' ->
      'clojerl.Keyword':'name'(X);
    'clojerl.Symbol' ->
      'clojerl.Symbol':'name'(X);
    'clojerl.Var' ->
      'clojerl.Var':'name'(X);
    _ ->
      clj_protocol:resolve(?MODULE, 'name', X)
  end.

'namespace'(X) ->
  case clj_rt:type_module(X) of
    'clojerl.Keyword' ->
      'clojerl.Keyword':'namespace'(X);
    'clojerl.Symbol' ->
      'clojerl.Symbol':'namespace'(X);
    'clojerl.Var' ->
      'clojerl.Var':'namespace'(X);
    _ ->
      clj_protocol:resolve(?MODULE, 'namespace', X)
  end.

?SATISFIES('clojerl.Keyword') -> true;
?SATISFIES('clojerl.Symbol') -> true;
?SATISFIES('clojerl.Var') -> true;
?SATISFIES(_) -> false.
