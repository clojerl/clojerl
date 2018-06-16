-module('clojerl.IReference').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['alter_meta'/3, 'reset_meta'/2]).
-export([?SATISFIES/1]).

-callback 'alter_meta'(any(), any(), any()) -> any().
-callback 'reset_meta'(any(), any()) -> any().

'alter_meta'(Ref, Fun, Args) ->
  case clj_rt:type_module(Ref) of
    'clojerl.Namespace' ->
      'clojerl.Namespace':'alter_meta'(Ref, Fun, Args);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'alter_meta', Type)
  end.

'reset_meta'(Ref, Meta) ->
  case clj_rt:type_module(Ref) of
    'clojerl.Namespace' ->
      'clojerl.Namespace':'reset_meta'(Ref, Meta);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'reset_meta', Type)
  end.

?SATISFIES('clojerl.Namespace') -> true;
?SATISFIES(_) -> false.
