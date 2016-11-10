-module('clojure.core').

-include("clojerl.hrl").

-clojure(true).

-mappings(#{ <<"ns">>    => { ?TYPE, 'clojerl.Var'
                            , {<<"clojure.core">>, <<"ns">>}
                            , #{meta => #{ macro           => true
                                         , 'variadic?'     => true
                                         , max_fixed_arity => ?NIL
                                         , variadic_arity  => 3
                                         }
                               }
                        }
           , <<"in-ns">> => { ?TYPE, 'clojerl.Var'
                            , {<<"clojure.core">>, <<"in-ns">>}
                            , #{meta => #{ 'variadic?'     => false
                                         , max_fixed_arity => 1
                                         , variadic_arity  => ?NIL
                                         }
                               }
                            }
           , <<"*ns*">>  => { ?TYPE, 'clojerl.Var'
                            , {<<"clojure.core">>, <<"*ns*">>}
                            , #{}
                            }
           , <<"*compile-files*">>  =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*compile-files*">>}
               , #{}
               }

           , <<"*assert*">> =>
               {?TYPE, 'clojerl.Var', {<<"clojure.core">>, <<"*assert*">>}, #{}}
           , <<"*read-eval*">> =>
               {?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*read-eval*">>}, #{}
               }
           , <<"*command-line-args*">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*command-line-args*">>}
               , #{}
               }

           , <<"*out*">> => { ?TYPE, 'clojerl.Var'
                            , {<<"clojure.core">>, <<"*out*">>}
                            , #{}
                            }
           , <<"*in*">>  => { ?TYPE, 'clojerl.Var'
                            , {<<"clojure.core">>, <<"*in*">>}
                            , #{}
                            }
           , <<"*err*">>  => { ?TYPE, 'clojerl.Var'
                             , {<<"clojure.core">>, <<"*err*">>}
                             , #{}
                             }

           , <<"*print-dup*">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*print-dup*">>}
               , #{}
               }
           , <<"*flush-on-newline*">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*flush-on-newline*">>}
               , #{}
               }
           , <<"*print-readably*">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*print-readably*">>}
               , #{}
               }
           , <<"*data-readers*">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*data-readers*">>}
               , #{}
               }
           , <<"default-data-readers">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"default-data-readers">>}
               , #{}
               }
           , <<"*default-data-reader-fn*">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*default-data-reader-fn*">>}
               , #{}
               }
           }).

-export([ ns/4
        , ns__val/0
        , 'in-ns'/1
        , 'in-ns__val'/0
        , '*ns*__val'/0
        , '*compile-files*__val'/0

        , '*assert*__val'/0
        , '*read-eval*__val'/0
        , '*command-line-args*__val'/0

        , '*out*__val'/0
        , '*in*__val'/0
        , '*err*__val'/0

        , '*print-dup*__val'/0
        , '*flush-on-newline*__val'/0
        , '*print-readably*__val'/0

        , '*data-readers*__val'/0
        , 'default-data-readers__val'/0
        , '*default-data-reader-fn*__val'/0
        ]).

ns(Form, _Env, Name, _References) ->
  clj_utils:error_when( not clj_core:'symbol?'(Name)
                      , <<"First argument to ns must be a symbol">>
                      , clj_reader:location_meta(Form)
                      ),

  InNsSym = clj_core:symbol(<<"clojure.core">>, <<"in-ns">>),
  QuoteSym = clj_core:symbol(<<"quote">>),
  clj_core:list([InNsSym, clj_core:list([QuoteSym, Name])]).

ns__val() ->
  Var  = 'clojerl.Var':?CONSTRUCTOR(<<"clojure.core">>, <<"ns">>),
  Meta = #{ macro           => true
          , 'variadic?'     => true
          , max_fixed_arity => ?NIL
          , variadic_arity  => 3
          },
  clj_core:with_meta(Var, Meta).

'in-ns'(MaybeQuotedName) ->
  Name  = 'maybe-unquote'(MaybeQuotedName),

  clj_utils:error_when( not clj_core:'symbol?'(Name)
                      , <<"First argument to in-ns must be a symbol">>
                      , clj_reader:location_meta(Name)
                      ),

  clj_namespace:find_or_create(Name),
  ?NIL.

'in-ns__val'() ->
  Var = 'clojerl.Var':?CONSTRUCTOR(<<"clojure.core">>, <<"in-ns">>),
  Meta = #{ 'variadic?'     => false
          , max_fixed_arity => 1
          , variadic_arity  => ?NIL
          },
  clj_core:with_meta(Var, Meta).

'*ns*__val'() ->
  case 'clojerl.Var':dynamic_binding(<<"#'clojure.core/*ns*">>) of
    ?NIL ->
      ClojureCoreSym = clj_core:symbol(<<"clojure.core">>),
      clj_namespace:find_or_create(ClojureCoreSym);
    {ok, X}   -> X
  end.

'*compile-files*__val'() ->
  var_value(<<"#'clojure.core/*compile-files*">>, false).

'*assert*__val'() ->
  var_value(<<"#'clojure.core/*assert*">>, true).

'*read-eval*__val'() ->
  var_value(<<"#'clojure.core/*read-eval*">>, true).

'*command-line-args*__val'() ->
  var_value(<<"#'clojure.core/*command-line-args*">>, true).

'*out*__val'() ->
  var_value(<<"#'clojure.core/*out*">>, standard_io).

'*in*__val'() ->
  var_value(<<"#'clojure.core/*in*">>, standard_io).

'*err*__val'() ->
  var_value(<<"#'clojure.core/*err*">>, standard_error).

'*print-dup*__val'() ->
  var_value(<<"#'clojure.core/*print-dup*">>, false).

'*flush-on-newline*__val'() ->
  var_value(<<"#'clojure.core/*flush-on-newline*">>, true).

'*print-readably*__val'() ->
  var_value(<<"#'clojure.core/*print-readably*">>, true).

'*data-readers*__val'() ->
  var_value(<<"#'clojure.core/*data-readers*">>, #{}).

'default-data-readers__val'() -> ?NIL.

'*default-data-reader-fn*__val'() ->
  var_value(<<"#'clojure.core/*default-data-reader-fn*">>, ?NIL).

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

%% @private
var_value(Name, Default) ->
  case 'clojerl.Var':dynamic_binding(Name) of
    ?NIL -> Default;
    {ok, X}   -> X
  end.
