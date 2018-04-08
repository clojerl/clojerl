-module('clojerl.IDeref').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['deref'/1, '__satisfies?__'/1]).

-callback 'deref'(any()) -> any().

'deref'(Ref) ->
  case clj_rt:type_module(Ref) of
    'clojerl.Atom' ->
      'clojerl.Atom':'deref'(Ref);
    'clojerl.Reduced' ->
      'clojerl.Reduced':'deref'(Ref);
    'clojerl.Var' ->
      'clojerl.Var':'deref'(Ref);
    _ ->
      clj_protocol:resolve(?MODULE, 'deref', Ref)
  end.

?SATISFIES('clojerl.Atom') -> true;
?SATISFIES('clojerl.Reduced') -> true;
?SATISFIES('clojerl.Var') -> true;
?SATISFIES(_) -> false.
