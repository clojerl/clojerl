-module('clojure.core').

-vars(#{ <<"ns">>    => { '7ype', 'clojerl.Var', {<<"clojure.core">>, <<"ns">>}
                        , #{meta => #{ macro           => true
                                     , 'variadic?'     => true
                                     , max_fixed_arity => undefined
                                     , variadic_arity  => 3
                                     }
                           }
                        }
       , <<"in-ns">> => { '7ype', 'clojerl.Var', {<<"clojure.core">>, <<"in-ns">>}
                        , #{meta => #{ 'variadic?'     => true
                                     , max_fixed_arity => 1
                                     , variadic_arity  => undefined
                                     }
                           }
                        }
       , <<"*ns*">>  => {'7ype', 'clojerl.Var', {<<"clojure.core">>, <<"*ns*">>}, #{}}
       , <<"*compile-files*">>  =>
           {'7ype', 'clojerl.Var', {<<"clojure.core">>, <<"*compile-files*">>}, #{}}

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
        , 'in-ns'/1
        , 'in-ns__val'/0
        , '*ns*__val'/0
        , '*compile-files*__val'/0

        , '*assert*__val'/0

        , '*out*__val'/0
        , '*in*__val'/0

        , '*print-dup*__val'/0
        , '*flush-on-newline*__val'/0
        , '*print-readably*__val'/0
        ]).

ns(_Form, _Env, Name, _References) ->
  clj_utils:throw_when( not clj_core:'symbol?'(Name)
                      , <<"First argument to ns must be a symbol">>
                      , clj_reader:location_meta(Name)
                      ),

  InNsSym = clj_core:symbol(<<"clojure.core">>, <<"in-ns">>),
  QuoteSym = clj_core:symbol(<<"quote">>),
  clj_core:list([InNsSym, clj_core:list([QuoteSym, Name])]).

ns__val() ->
  Var  = 'clojerl.Var':new(<<"clojure.core">>, <<"ns">>),
  Meta = #{ macro           => true
          , 'variadic?'     => true
          , max_fixed_arity => undefined
          , variadic_arity  => 3
          },
  clj_core:with_meta(Var, Meta).

'in-ns'(MaybeQuotedName) ->
  Name  = 'maybe-unquote'(MaybeQuotedName),

  clj_utils:throw_when( not clj_core:'symbol?'(Name)
                      , <<"First argument to ns must be a symbol">>
                      , clj_reader:location_meta(Name)
                      ),

  NsVar = 'clojerl.Var':new(<<"clojure.core">>, <<"*ns*">>),
  Ns = clj_namespace:find_or_create(Name),
  clj_core:'set!'(NsVar, Ns),
  undefined.

'in-ns__val'() ->
  Var = 'clojerl.Var':new(<<"clojure.core">>, <<"in-ns">>),
  Meta = #{ 'variadic?'     => true
          , max_fixed_arity => 1
          , variadic_arity  => undefined
          },
  clj_core:with_meta(Var, Meta).

'*ns*__val'() ->
  case 'clojerl.Var':dynamic_binding(<<"#'clojure.core/*ns*">>) of
    undefined -> 
      ClojureCoreSym = clj_core:symbol(<<"clojure.core">>),
      clj_namespace:find_or_create(ClojureCoreSym);
    X -> 
      X
  end.

'*compile-files*__val'() ->
  case 'clojerl.Var':dynamic_binding(<<"#'clojure.core/*compile-files*">>) of
    undefined -> false;
    X         -> X
  end.

'*assert*__val'() -> true.

'*out*__val'() -> standard_io.

'*in*__val'() -> standard_io.

'*print-dup*__val'() -> false.

'*flush-on-newline*__val'() -> true.

'*print-readably*__val'() -> true.

%% @private
'maybe-unquote'(MaybeQuotedForm) ->
  case clj_core:'seq?'(MaybeQuotedForm) of
    false -> MaybeQuotedForm;
    true  ->
      Quote = clj_core:first(MaybeQuotedForm),
      case clj_core:equiv(Quote, clj_core:symbol(<<"quote">>)) of
        true  -> clj_core:second(MaybeQuotedForm);
        false -> MaybeQuotedForm
      end
  end.
