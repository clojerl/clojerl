%% @doc Clojerl behaviour checks.
%%
%% Implements the same checks done to Erlang modules when they specify
%% they implement a behaviour.
-module(clj_behaviour).

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-export([check/1]).

%% @doc Checks that the Core Erlang module implements the behaviours
%% it specifies.
%%
%% Shows warnings by printing to `clojure.core/*err*' if any issue is
%% found.
-spec check(cerl:c_module()) -> ok.
check(Module) ->
  ExportsAsts = cerl:module_exports(Module),
  AttrsAsts   = cerl:module_attrs(Module),
  File        = find_file(AttrsAsts),

  Exports     = [extract_fa(E) || E <- ExportsAsts],
  Behaviours  = [extract_behaviour(A) || A <- AttrsAsts, is_behaviour(A)],

  [do_check(File, B, Exports) || B <- Behaviours],

  ok.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

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

-spec find_file([{cerl:cerl(), cerl:cerl()}]) -> binary().
find_file(AttrsAsts) ->
  case [V || {K, V} <- AttrsAsts, cerl:concrete(K) =:= file] of
    [] -> <<?NO_SOURCE>>;
    [V] -> to_binary(cerl:concrete(V))
  end.

-spec to_binary(any()) -> binary().
to_binary([{X, _}]) ->
  list_to_binary(X);
to_binary(X) when is_list(X) ->
  list_to_binary(X);
to_binary(_) ->
  <<?NO_SOURCE>>.

-spec do_check(binary(), module(), [{atom(), arity()}]) -> ok.
do_check(File, Behaviour, Exports) ->
  Callbacks = sets:from_list(callbacks(Behaviour, File, callbacks)),
  Optional  = sets:from_list(callbacks(Behaviour, File, optional_callbacks)),
  Required  = sets:subtract(Callbacks, Optional),
  Missing   = sets:subtract(Required, sets:from_list(Exports)),

  [ ?WARN( [ <<"Missing callback '">>
           , clj_rt:name(F), <<"' (arity ">>, A , <<")">>
           , <<" for behaviour '">>, clj_rt:name(Behaviour), <<"'">>
           ]
         , location(File)
         )
    || {F, A} <- sets:to_list(Missing)
  ],

  ok.

-spec callbacks(module(), binary(), callbacks | optional_callbacks) ->
  [{atom(), arity()}].
callbacks(Behavior, File, Type) ->
  try
    case Behavior:behaviour_info(Type) of
      undefined ->
        ?WARN_WHEN( Type =:= callbacks
                  , [<<"Undefined behaviour callbacks">>, Behavior]
                  , location(File)
                  ),
        [];
      Functions ->
        case is_fa_list(Functions) of
          true -> Functions;
          false ->
            ?WARN([<<"Ill defined ">>, Type, " in ", Behavior], location(File)),
            []
        end
    end
  catch
    _:_ ->
      ?WARN_WHEN( Type =:= callbacks
                , [<<"Undefined behaviour ">>, Behavior]
                , location(File)
                ),
      []
  end.

-spec location(binary()) -> clj_reader:location().
location(File) ->
  #{file => File, line => 1, column => 1}.

is_fa_list([{F, A} | L]) when is_atom(F), is_integer(A), A >= 0 ->
  is_fa_list(L);
is_fa_list([]) ->
  true;
is_fa_list(_) ->
  false.
