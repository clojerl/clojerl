-module(clj_behaviour).

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-export([check/1]).

-spec check(cerl:c_module()) -> ok.
check(Module) ->
  ExportsAsts = cerl:module_exports(Module),
  AttrsAsts = cerl:module_attrs(Module),

  Exports = [extract_fa(E) || E <- ExportsAsts],
  Behaviours = [extract_behaviour(A) || A <- AttrsAsts, is_behaviour(A)],

  [do_check(B, Exports) || B <- Behaviours],

  ok.

-spec extract_fa(cerl:cerl()) -> {atom(), arity()}.
extract_fa(FunArityAst) ->
  {cerl:fname_id(FunArityAst), cerl:fname_arity(FunArityAst)}.

-spec extract_behaviour({cerl:cerl(), cerl:cerl()}) -> module().
extract_behaviour({_, ValueAst}) ->
  [Module] = cerl:concrete(ValueAst),
  Module.

-spec is_behaviour({cerl:cerl(), cerl:cerl()}) -> boolean().
is_behaviour({KeyAst, _}) ->
  Key = cerl:concrete(KeyAst),
  Key =:= behavior orelse Key =:= behaviour.

-spec do_check(module(), [{atom(), arity()}]) -> ok.
do_check(Behaviour, Exports) ->
  Callbacks = sets:from_list(callbacks(Behaviour, callbacks)),
  Optional  = sets:from_list(callbacks(Behaviour, optional_callbacks)),
  Required  = sets:subtract(Callbacks, Optional),
  Missing   = sets:subtract(Required, sets:from_list(Exports)),

  [ ?WARN([ <<"undefined callback function ">>
          , F, <<"/">>, A
          , <<" (behaviour ">>, Behaviour, <<")">>
          ])
    || {F, A} <- sets:to_list(Missing)
  ],

  ok.

-spec callbacks(module(), callbacks | optional_callbacks) ->
  [{atom(), arity()}].
callbacks(Behavior, Type) ->
  try
    case Behavior:behaviour_info(Type) of
      undefined ->
        ?WARN_WHEN( Type =:= callbacks
                  , [<<"Undefined behaviour callbacks">>, Behavior]
                  ),
        [];
      Functions ->
        case is_fa_list(Functions) of
          true -> Functions;
          false ->
            ?WARN([<<"Ill defined ">>, Type, " in ", Behavior]),
            []
        end
    end
  catch
    _:_ ->
      ?WARN_WHEN( Type =:= callbacks
                , [<<"Undefined behaviour ">>, Behavior]
                ),
      []
  end.

is_fa_list([{F, A} | L]) when is_atom(F), is_integer(A), A >= 0 ->
  is_fa_list(L);
is_fa_list([]) ->
  true;
is_fa_list(_) ->
  false.
