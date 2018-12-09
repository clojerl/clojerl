-module('clojerl.IDeref').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['deref'/1]).
-export([?SATISFIES/1]).

-callback 'deref'(any()) -> any().

'deref'(Ref) ->
  case clj_rt:type_module(Ref) of
    'clojerl.Reduced' ->
      'clojerl.Reduced':'deref'(Ref);
    'clojerl.Var' ->
      'clojerl.Var':'deref'(Ref);
    'clojerl.Atom' ->
      'clojerl.Atom':'deref'(Ref);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'deref', Type)
  end.

?SATISFIES('clojerl.Reduced') -> true;
?SATISFIES('clojerl.Var') -> true;
?SATISFIES('clojerl.Atom') -> true;
?SATISFIES(_) -> false.
