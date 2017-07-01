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
                            , #{meta => #{dynamic => true}}
                            }

           , <<"*compile-path*">>  =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*compile-path*">>}
               , #{meta => #{dynamic => true}}
               }
           , <<"*compile-files*">>  =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*compile-files*">>}
               , #{meta => #{dynamic => true}}
               }

           , <<"*assert*">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*assert*">>}
               , #{meta => #{dynamic => true}}
               }
           , <<"*read-eval*">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*read-eval*">>}
               , #{meta => #{dynamic => true}}
               }
           , <<"*command-line-args*">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*command-line-args*">>}
               , #{meta => #{dynamic => true}}
               }

           , <<"*out*">> => { ?TYPE, 'clojerl.Var'
                            , {<<"clojure.core">>, <<"*out*">>}
                            , #{meta => #{dynamic => true}}
                            }
           , <<"*in*">>  => { ?TYPE, 'clojerl.Var'
                            , {<<"clojure.core">>, <<"*in*">>}
                            , #{meta => #{dynamic => true}}
                            }
           , <<"*err*">> => { ?TYPE, 'clojerl.Var'
                            , {<<"clojure.core">>, <<"*err*">>}
                            , #{meta => #{dynamic => true}}
                            }

           , <<"*print-dup*">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*print-dup*">>}
               , #{meta => #{dynamic => true}}
               }
           , <<"*flush-on-newline*">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*flush-on-newline*">>}
               , #{meta => #{dynamic => true}}
               }
           , <<"*print-readably*">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*print-readably*">>}
               , #{meta => #{dynamic => true}}
               }
           , <<"*data-readers*">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*data-readers*">>}
               , #{meta => #{dynamic => true}}
               }
           , <<"default-data-readers">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"default-data-readers">>}
               , #{}
               }
           , <<"*default-data-reader-fn*">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*default-data-reader-fn*">>}
               , #{meta => #{dynamic => true}}
               }
           , <<"*warn-on-infer*">> =>
               { ?TYPE, 'clojerl.Var'
               , {<<"clojure.core">>, <<"*warn-on-infer*">>}
               , #{meta => #{dynamic => true}}
               }
           }).

-export([ ns/4
        , ns__val/0
        , 'in-ns'/1
        , 'in-ns__val'/0
        , '*ns*__val'/0
        , '*compile-path*__val'/0
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

        , '*warn-on-infer*__val'/0
        ]).

ns(Form, _Env, Name, _References) ->
  clj_utils:error_when( not clj_rt:'symbol?'(Name)
                      , <<"First argument to ns must be a symbol">>
                      , clj_reader:location_meta(Form)
                      ),

  InNsSym = clj_rt:symbol(<<"clojure.core">>, <<"in-ns">>),
  QuoteSym = clj_rt:symbol(<<"quote">>),
  clj_rt:list([InNsSym, clj_rt:list([QuoteSym, Name])]).

ns__val() ->
  Var  = 'clojerl.Var':?CONSTRUCTOR(<<"clojure.core">>, <<"ns">>),
  Meta = #{ macro           => true
          , 'variadic?'     => true
          , max_fixed_arity => ?NIL
          , variadic_arity  => 3
          },
  clj_rt:with_meta(Var, Meta).

'in-ns'(MaybeQuotedName) ->
  Name  = 'maybe-unquote'(MaybeQuotedName),

  clj_utils:error_when( not clj_rt:'symbol?'(Name)
                      , <<"First argument to in-ns must be a symbol">>
                      , clj_reader:location_meta(Name)
                      ),

  'clojerl.Namespace':find_or_create(Name),
  ?NIL.

'in-ns__val'() ->
  Var = 'clojerl.Var':?CONSTRUCTOR(<<"clojure.core">>, <<"in-ns">>),
  Meta = #{ 'variadic?'     => false
          , max_fixed_arity => 1
          , variadic_arity  => ?NIL
          },
  clj_rt:with_meta(Var, Meta).

'*ns*__val'() ->
  case 'clojerl.Var':dynamic_binding(<<"#'clojure.core/*ns*">>) of
    ?NIL ->
      ClojureCoreSym = clj_rt:symbol(<<"clojure.core">>),
      'clojerl.Namespace':find_or_create(ClojureCoreSym);
    {ok, X}   -> X
  end.

'*compile-path*__val'() ->
  var_value(<<"#'clojure.core/*compile-path*">>, ?NIL).

'*compile-files*__val'() ->
  var_value(<<"#'clojure.core/*compile-files*">>, false).

'*assert*__val'() ->
  var_value(<<"#'clojure.core/*assert*">>, true).

'*read-eval*__val'() ->
  ReadEval = application:get_env(clojerl, read_eval, true),
  var_value(<<"#'clojure.core/*read-eval*">>, ReadEval).

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

'*warn-on-infer*__val'() ->
  var_value(<<"#'clojure.core/*warn-on-infer*">>, false).

%% @private
'maybe-unquote'(MaybeQuotedForm) ->
  case clj_rt:'seq?'(MaybeQuotedForm) of
    false -> MaybeQuotedForm;
    true  ->
      Quote = clj_rt:first(MaybeQuotedForm),
      case clj_rt:equiv(Quote, clj_rt:symbol(<<"quote">>)) of
        true  -> clj_rt:second(MaybeQuotedForm);
        false -> MaybeQuotedForm
      end
  end.

%% @private
var_value(Name, Default) ->
  case 'clojerl.Var':dynamic_binding(Name) of
    ?NIL -> Default;
    {ok, X}   -> X
  end.
