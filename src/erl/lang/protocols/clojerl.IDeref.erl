-module('clojerl.IDeref').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['deref'/1]).
-export([?SATISFIES/1]).

-callback 'deref'(any()) -> any().

'deref'(Ref) ->
  case Ref of
    #{?TYPE := 'clojerl.Reduced'} ->
      'clojerl.Reduced':'deref'(Ref);
    #{?TYPE := 'clojerl.ProcessVal'} ->
      'clojerl.ProcessVal':'deref'(Ref);
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'deref'(Ref);
    #{?TYPE := 'clojerl.Delay'} ->
      'clojerl.Delay':'deref'(Ref);
    #{?TYPE := 'clojerl.Atom'} ->
      'clojerl.Atom':'deref'(Ref);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'deref', Ref);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'deref', Ref)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Reduced'} -> true;
    #{?TYPE := 'clojerl.ProcessVal'} -> true;
    #{?TYPE := 'clojerl.Var'} -> true;
    #{?TYPE := 'clojerl.Delay'} -> true;
    #{?TYPE := 'clojerl.Atom'} -> true;
    #{?TYPE := _} -> false;
    _ -> false
  end.
