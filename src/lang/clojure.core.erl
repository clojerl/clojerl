-module('clojure.core').

-vars(#{ <<"ns">>    => { '7ype', 'clojerl.Var', {<<"clojure.core">>, <<"ns">>}
                        , #{meta => #{macro => true}}
                        }
       , <<"*ns*">>  => { '7ype', 'clojerl.Var', {<<"clojure.core">>, <<"*ns*">>} , #{}}
       , <<"*env*">> => { '7ype', 'clojerl.Var', {<<"clojure.core">>, <<"*env*">>} , #{}}
       }).

-clojure(true).

-export([ ns/3
        , ns__val/0
        , '*ns*__val'/0
        , '*env*__val'/0
        ]
       ).

ns(_Env, Form, Symbol) ->
  EnvVar = 'clojerl.Var':new(<<"clojure.core">>, <<"*env*">>),
  Env    = clj_core:deref(EnvVar),
  case clj_core:'symbol?'(Symbol) of
    true ->
      {_, NewEnv} = clj_env:find_or_create_ns(Env, Symbol),
      clj_core:'set!'(EnvVar, NewEnv),
      undefined;
    false ->
      clj_utils:throw( <<"First argument to ns must a symbol">>
                     , clj_reader:location_meta(Form)
                     )
  end.


ns__val() ->
  Var = 'clojerl.Var':new(<<"clojure.core">>, <<"ns">>),
  clj_core:with_meta(Var, #{}).

'*ns*__val'() -> throw(unbound).

'*env*__val'() -> throw(unbound).
