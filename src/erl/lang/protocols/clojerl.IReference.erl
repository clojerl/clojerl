-module('clojerl.IReference').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['alter_meta'/3, 'reset_meta'/2]).
-export([?SATISFIES/1]).

-callback 'alter_meta'(any(), any(), any()) -> any().
-callback 'reset_meta'(any(), any()) -> any().

'alter_meta'(Ref, Fun, Args) ->
  case Ref of
    #{?TYPE := 'clojerl.Namespace'} ->
      'clojerl.Namespace':'alter_meta'(Ref, Fun, Args);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'alter_meta', Ref);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'alter_meta', Ref)
  end.

'reset_meta'(Ref, Meta) ->
  case Ref of
    #{?TYPE := 'clojerl.Namespace'} ->
      'clojerl.Namespace':'reset_meta'(Ref, Meta);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'reset_meta', Ref);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'reset_meta', Ref)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Namespace'} -> true;
    #{?TYPE := _} -> false;
    _ -> false
  end.
