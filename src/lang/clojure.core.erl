-module('clojure.core').

-vars(#{ <<"ns">>    => { '7ype', 'clojerl.Var', {<<"clojure.core">>, <<"ns">>}
                        , #{meta => #{ macro           => true
                                     , 'variadic?'     => true
                                     , max_fixed_arity => undefined
                                     , variadic_arity  => 4
                                     }
                           }
                        }
       , <<"in-ns">> => { '7ype', 'clojerl.Var', {<<"clojure.core">>, <<"in-ns">>}
                        , #{meta => #{ macro           => true
                                     , 'variadic?'     => true
                                     , max_fixed_arity => undefined
                                     , variadic_arity  => 4
                                     }
                           }
                        }
       , <<"*ns*">>  => {'7ype', 'clojerl.Var', {<<"clojure.core">>, <<"*ns*">>}, #{}}
       , <<"*env*">> => {'7ype', 'clojerl.Var', {<<"clojure.core">>, <<"*env*">>}, #{}}

       , <<"*assert*">> =>
           {'7ype', 'clojerl.Var', {<<"clojure.core">>, <<"*assert*">>}, #{}}

       , <<"*out*">> => {'7ype', 'clojerl.Var', {<<"clojure.core">>, <<"*out*">>}, #{}}
       , <<"*in*">>  => {'7ype', 'clojerl.Var', {<<"clojure.core">>, <<"*in*">>}, #{}}

       , <<"*print-dup*">> =>
           {'7ype', 'clojerl.Var', {<<"clojure.core">>, <<"*print-dup*">>}, #{}}
       , <<"*flush-on-newline*">> =>
           {'7ype', 'clojerl.Var', {<<"clojure.core">>, <<"*flush-on-newline*">>}, #{}}
       , <<"*print-readably*">> =>
           {'7ype', 'clojerl.Var', {<<"clojure.core">>, <<"*print-readably*">>}, #{}}
       }).

-clojure(true).

-export([ ns/4
        , ns__val/0
        , 'in-ns'/4
        , 'in-ns__val'/0
        , '*ns*__val'/0
        , '*env*__val'/0

        , '*assert*__val'/0

        , '*out*__val'/0
        , '*in*__val'/0

        , '*print-dup*__val'/0
        , '*flush-on-newline*__val'/0
        , '*print-readably*__val'/0
        ]).

ns(Form, Env, Name, References) ->
  'in-ns'(Form, Env, Name, References).

ns__val() ->
  Var  = 'clojerl.Var':new(<<"clojure.core">>, <<"ns">>),
  Meta = #{ macro           => true
          , 'variadic?'     => true
          , max_fixed_arity => undefined
          , variadic_arity  => 4
          },
  clj_core:with_meta(Var, Meta).

'in-ns'(Form, Env, MaybeQuotedName, _) ->
  Name   = maybe_unquote_form(MaybeQuotedName),
  EnvVar = 'clojerl.Var':new(<<"clojure.core">>, <<"*env*">>),
  case clj_core:'symbol?'(Name) of
    true ->
      {_, NewEnv} = clj_env:find_or_create_ns(Env, Name),
      clj_core:'set!'(EnvVar, NewEnv),
      undefined;
    false ->
      clj_utils:throw( <<"First argument to in-ns must be a symbol">>
                     , clj_reader:location_meta(Form)
                     )
  end.

'in-ns__val'() ->
  Var = 'clojerl.Var':new(<<"clojure.core">>, <<"ns">>),
  Meta = #{ macro           => true
          , 'variadic?'     => true
          , max_fixed_arity => undefined
          , variadic_arity  => 4
          },
  clj_core:with_meta(Var, Meta).

'*ns*__val'() -> throw(unbound).

'*env*__val'() -> throw(unbound).

'*assert*__val'() -> true.

'*out*__val'() -> standard_io.

'*in*__val'() -> standard_io.

'*print-dup*__val'() -> false.

'*flush-on-newline*__val'() -> true.

'*print-readably*__val'() -> true.

%% @private
maybe_unquote_form(MaybeQuotedForm) ->
  case clj_core:'seq?'(MaybeQuotedForm) of
    false -> MaybeQuotedForm;
    true  ->
      Quote = clj_core:first(MaybeQuotedForm),
      case clj_core:equiv(Quote, clj_core:symbol(<<"quote">>)) of
        true  -> clj_core:second(MaybeQuotedForm);
        false -> MaybeQuotedForm
      end
  end.
