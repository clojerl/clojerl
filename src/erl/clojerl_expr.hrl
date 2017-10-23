%% Analyzer expressions

-type op()   :: local | binding
              | def | var | do | quote | 'if' | 'let' | 'case'
              | letfn | fn_method | fn | invoke | loop | recur
              | import | new | deftype | defprotocol | extend_type
              | throw | 'try' | 'catch'
                %% Values
              | constant | vector | map | set | with_meta
                %% Erlang
              | erl_map | erl_list | erl_alias | erl_fun | tuple
              | erl_binary | binary_segment
              | 'receive' | 'after' | on_load
                %% Type
              | resolve_type | type.

-type loop_type()      :: fn | var | loop | function.
-type loop_id()        :: 'clojerl.Symbol':type().

-type constant_expr()  :: #{ op   => constant
                           , env  => clj_env:env()
                           , form => any()
                           , tag  => expr()
                           }.

-type quote_expr()     :: #{ op   => quote
                           , env  => clj_env:env()
                           , expr => any()
                           , form => any()
                           , tag  => expr()
                           }.

-type local_expr()     :: #{ op     => local
                           , env    => clj_env:env()
                           , form   => any()
                           , tag    => expr()
                           , name   => 'clojerl.Symbol':type()
                           , shadow => any()
                           , id     => integer()
                           }.

-type binding_expr()   :: #{ op          => binding
                           , env         => clj_env:env()
                           , form        => any()
                           , tag         => expr()
                           , pattern     => expr()
                           , 'variadic?' => boolean()
                           , arg_id      => integer()
                           , local       => arg | loop | 'let'
                           }.

-type fn_expr()        :: #{ op              => fn
                           , env             => clj_env:env()
                           , form            => any()
                           , tag             => expr()
                           , 'variadic?'     => boolean()
                           , max_fixed_arity => ?NIL | integer()
                           , variadic_arity  => ?NIL | integer()
                           , methods         => [expr()]
                           , once            => boolean()
                           , local           => local_expr()
                           }.

-type fn_method_expr() :: #{ op          => fn_method
                           , env         => clj_env:env()
                           , form        => any()
                           , tag         => expr()
                           %% , name        => 'clojerl.Symbol':type()
                           , loop_id     => loop_id()
                           , loop_type   => loop_type()
                           , 'variadic?' => boolean()
                           , params      => [expr()]
                           , guard       => expr()
                           , fixed_arity => integer()
                           , body        => expr()
                           }.

-type do_expr()        :: #{ op         => do
                           , env        => clj_env:env()
                           , form       => any()
                           , tag        => expr()
                           , statements => [expr()]
                           , ret        => expr()
                           }.

-type if_expr()        :: #{ op   => 'if'
                           , env  => clj_env:env()
                           , form => any()
                           , tag  => expr()
                           , test => expr()
                           , then => expr()
                           , else => expr()
                           }.

-type let_expr()       :: #{ op       => 'let'
                           , env      => clj_env:env()
                           , form     => any()
                           , tag      => expr()
                           , body     => expr()
                           , bindings => [expr()]
                           }.

-type loop_expr()      :: #{ op       => loop
                           , env      => clj_env:env()
                           , form     => any()
                           , tag      => expr()
                           , loop_id  => loop_id()
                           , body     => expr()
                           , bindings => [expr()]
                           }.

-type recur_expr()     :: #{ op        => recur
                           , env       => clj_env:env()
                           , form      => any()
                           , tag       => expr()
                           , exprs     => [expr()]
                           , loop_id   => loop_id()
                           , loop_type => loop_type()
                           }.

-type letfn_expr()     :: #{ op        => letfn
                           , env       => clj_env:env()
                           , form      => any()
                           , tag       => expr()
                           , vars      => [local_expr()]
                           , fns       => [fn_expr()]
                           , body      => expr()
                           }.

-type case_expr()      :: #{ op      => 'case'
                           , env     => clj_env:env()
                           , form    => any()
                           , tag     => expr()
                           , test    => expr()
                           , clauses => [{expr(), expr()}]
                           , default => expr() | ?NIL
                           }.

-type erl_map_expr()   :: #{ op      => erl_map
                           , env     => clj_env:env()
                           , form    => any()
                           , tag     => expr()
                           , keys    => [expr()]
                           , vals    => [expr()]
                           , pattern => boolean()
                           }.

-type erl_list_expr()  :: #{ op      => erl_list
                           , env     => clj_env:env()
                           , form    => any()
                           , tag     => expr()
                           , items   => [expr()]
                           , tail    => expr() | ?NIL
                           }.

-type erl_binary_expr() :: #{ op       => erl_list
                            , env      => clj_env:env()
                            , form     => any()
                            , tag      => expr()
                            , segments => [binary_segment_expr()]
                            }.

-type binary_segment_expr() :: #{ op    => erl_list
                                , env   => clj_env:env()
                                , form  => any()
                                , tag   => expr()
                                , value => expr()
                                , size  => expr()
                                , unit  => expr()
                                , type  => expr()
                                , flags => expr()
                                }.

-type tuple_expr()     :: #{ op      => tuple
                           , env     => clj_env:env()
                           , form    => any()
                           , tag     => expr()
                           , items   => [expr()]
                           }.

-type def_expr()       :: #{ op      => def
                           , env     => clj_env:env()
                           , form    => any()
                           , tag     => expr()
                           , name    => 'clojerl.Symbol':type()
                           , var     => 'clojerl.Var':type()
                           , init    => expr()
                           , dynamic => boolean()
                           }.

-type import_expr()    :: #{ op       => import
                           , env      => clj_env:env()
                           , form     => any()
                           , tag      => expr()
                           , typename => binary()
                           }.

-type new_expr()       :: #{ op       => new
                           , env      => clj_env:env()
                           , form     => any()
                           , tag      => expr()
                           , type     => [expr()]
                           , args     => [expr()]
                           }.

-type deftype_expr()   :: #{ op        => deftype
                           , env       => clj_env:env()
                           , form      => any()
                           , tag       => expr()
                           , name      => 'clojerl.Symbol':type()
                           , type      => 'erlang.Type':type()
                           , fields    => [expr()]
                           , protocols => [expr()]
                           , methods   => [expr()]
                           , opts      => [any()]
                           }.

%% TODO: validate methods_sigs in analyzer
-type defprotocol_expr() :: #{ op           => defprotocol
                             , env          => clj_env:env()
                             , form         => any()
                             , tag          => expr()
                             , methods_sigs => [any()]
                             }.

-type extend_type_expr() :: #{ op        => extend_type
                             , env       => clj_env:env()
                             , form      => any()
                             , tag       => expr()
                             , type      => expr()
                             , impls     => #{expr() => [expr()]}
                             }.

-type invoke_expr()      :: #{ op   => invoke
                             , env  => clj_env:env()
                             , form => any()
                             , tag  => expr()
                             , f    => expr()
                             , args => [expr()]
                             }.

-type resolve_type_expr() :: #{ op       => resolve_type
                              , env      => clj_env:env()
                              , form     => any()
                              , tag      => expr()
                              , function => atom()
                              }.

-type throw_expr()        :: #{ op        => throw
                              , env       => clj_env:env()
                              , form      => any()
                              , tag       => expr()
                              , exception => expr()
                              }.

-type try_expr()          :: #{ op        => 'try'
                              , env       => clj_env:env()
                              , form      => any()
                              , tag       => expr()
                              , body      => expr()
                              , catches   => [catch_expr()]
                              , finally   => expr()
                              }.

-type catch_expr()        :: #{ op        => 'catch'
                              , env       => clj_env:env()
                              , form      => any()
                              , tag       => expr()
                                %% TODO: should this be a type
                              , class     => 'clojerl.Symbol':type()
                              , local     => binding_expr()
                              , guard     => expr()
                              , body      => expr()
                              }.

-type erl_fun_expr()      :: #{ op        => erl_fun
                              , env       => clj_env:env()
                              , form      => any()
                              , tag       => expr()
                              , module    => module()
                              , function  => atom()
                              , arity     => arity()
                              }.

-type var_expr()          :: #{ op         => var
                              , env        => clj_env:env()
                              , form       => any()
                              , tag        => expr()
                              , name       => 'clojerl.Symbol':type()
                              , var        => 'clojerl.Var':type()
                              , is_dynamic => boolean()
                              }.

-type type_expr()         :: #{ op         => type
                              , env        => clj_env:env()
                              , form       => any()
                              , tag        => expr()
                              , type       => 'clojerl.Symbol':type()
                              }.

-type with_meta_expr()    :: #{ op         => with_meta
                              , env        => clj_env:env()
                              , form       => any()
                              , tag        => expr()
                              , meta       => expr()
                              , expr       => expr()
                              }.

-type vector_expr()       :: #{ op         => vector
                              , env        => clj_env:env()
                              , form       => any()
                              , tag        => expr()
                              , items      => [expr()]
                              }.

-type set_expr()          :: #{ op         => set
                              , env        => clj_env:env()
                              , form       => any()
                              , tag        => expr()
                              , items      => [expr()]
                              }.

-type map_expr()          :: #{ op         => map
                              , env        => clj_env:env()
                              , form       => any()
                              , tag        => expr()
                              , keys       => [expr()]
                              , vals       => [expr()]
                              }.

-type receive_expr()      :: #{ op         => 'receive'
                              , env        => clj_env:env()
                              , form       => any()
                              , tag        => expr()
                              , clauses    => [expr()]
                              , 'after'    => after_expr()
                              }.

-type after_expr()        :: #{ op         => 'after'
                              , env        => clj_env:env()
                              , form       => any()
                              , tag        => expr()
                              , timeout    => timeout()
                              , body       => expr()
                              }.

-type erl_alias_expr()    :: #{ op       => erl_alias
                              , env      => clj_env:env()
                              , form     => any()
                              , tag      => expr()
                              , variable => expr()
                              , pattern  => expr()
                              }.

-type on_load_expr()      :: #{ op    => on_load
                              , env   => clj_env:env()
                              , form  => any()
                              , tag   => expr()
                              , body  => expr()
                              }.

-type expr() :: constant_expr()
              | quote_expr()
              | local_expr()
              | binding_expr()
              | fn_expr()
              | fn_method_expr()
              | do_expr()
              | if_expr()
              | let_expr()
              | loop_expr()
              | recur_expr()
              | letfn_expr()
              | case_expr()
              | def_expr()
              | import_expr()
              | new_expr()
              | deftype_expr()
              | defprotocol_expr()
              | extend_type_expr()
              | invoke_expr()
              | resolve_type_expr()
              | throw_expr()
              | try_expr()
              | catch_expr()
              | erl_fun_expr()
              | var_expr()
              | type_expr()
              | with_meta_expr()
              | erl_map_expr()
              | erl_list_expr()
              | erl_binary_expr()
              | binary_segment_expr()
              | tuple_expr()
              | vector_expr()
              | set_expr()
              | map_expr()
              | receive_expr()
              | after_expr().
