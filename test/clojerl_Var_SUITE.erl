-module(clojerl_Var_SUITE).

-include("clojerl.hrl").

-export([all/0, init_per_suite/1]).

-export([ 'forty-two__val'/0
        , 'forty-two'/0
        , 'forty-two'/1
        , 'forty-two'/2
        , 'forty-three'/2
        , 'forty-four__val'/0
        , 'forty-five__val'/0
        ]).

-export([ deref/1
        , equiv/1
        , apply/1
        , meta/1
        , name/1
        , str/1
        , dynamic_bindings/1
        , complete_coverage/1
        ]).

-type config() :: list().
-type result() :: {comments, string()}.

-mappings(#{<<"forty-two">> => { ?TYPE
                               , 'clojerl.Var'
                               , {<<"clojerl_Var_SUITE">>, <<"forty-two">>}
                               , #{}
                               }
           }).

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [ init_per_suite
                 , end_per_suite
                 , all
                 , module_info
                 , 'forty-two'
                 ],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(clojerl),
  Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec 'forty-two__val'() -> any().
'forty-two__val'() ->
  Name = <<"#'clojerl_Var_SUITE/forty-two">>,
  case 'clojerl.Var':dynamic_binding(Name) of
    ?NIL -> 42;
    {ok, X}   -> X
  end.

-spec 'forty-two'() -> any().
'forty-two'() -> 42.

-spec 'forty-two'(integer()) -> any().
'forty-two'(X) -> 42 + X.

-spec 'forty-two'(integer(), [integer()]) -> any().
'forty-two'(X, XS) ->
  Sum = lists:foldl(fun erlang:'+'/2, 0, XS),
  42 + X + Sum.

-spec 'forty-three'(integer(), [integer()]) -> any().
'forty-three'(X, XS) ->
  [43, X, XS].

-spec 'forty-four__val'() -> any().
'forty-four__val'() ->
  unexisting:function().

-spec 'forty-five__val'() -> any().
'forty-five__val'() -> ?UNBOUND.

-spec deref(config()) -> result().
deref(_Config) ->
  Ns   = <<"clojerl_Var_SUITE">>,
  Name = <<"forty-two">>,

  Var  = 'clojerl.Var':?CONSTRUCTOR(Ns, Name),
  42 = clj_core:deref(Var),

  ct:comment("Try to deref an unexisting var"),
  Name2 = <<"forty-three">>,
  Var1  = 'clojerl.Var':?CONSTRUCTOR(Ns, Name2),
  ok = try clj_core:deref(Var1), error
       catch _:_ -> ok
       end,

  ct:comment("Try to deref an existing var"),
  Name3 = <<"forty-four">>,
  Var2  = 'clojerl.Var':?CONSTRUCTOR(Ns, Name3),
  ok = try clj_core:deref(Var2), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  Ns   = <<"clojerl_Var_SUITE">>,
  Name = <<"forty-two">>,

  ct:comment("Check that vars with the same name are equivalent"),
  Var1 = clj_core:with_meta('clojerl.Var':?CONSTRUCTOR(Ns, Name), #{a => 1}),
  Var2 = clj_core:with_meta('clojerl.Var':?CONSTRUCTOR(Ns, Name), #{b => 2}),
  true  = clj_core:equiv(Var1, Var2),

  ct:comment("Check that vars with the same elements are not equivalent"),
  Name2 = <<"whatever">>,
  Var3 = clj_core:with_meta('clojerl.Var':?CONSTRUCTOR(Ns, Name2), #{c => 3}),
  false = clj_core:equiv(Var1, Var3),

  ct:comment("A clojerl.Var and something else"),
  false = clj_core:equiv(Var1, whatever),
  false = clj_core:equiv(Var1, #{}),
  false = clj_core:equiv(Var1, Ns),

  {comments, ""}.

-spec apply(config()) -> result().
apply(_Config) ->
  Ns     = <<"clojerl_Var_SUITE">>,
  Name42 = <<"forty-two">>,
  Name43 = <<"forty-three">>,

  Var42  = 'clojerl.Var':?CONSTRUCTOR(Ns, Name42),
  42 = clj_core:apply(Var42, []),
  45 = clj_core:apply(Var42, [3]),

  false = 'clojerl.Var':is_dynamic(Var42),
  false = 'clojerl.Var':is_macro(Var42),

  Meta42 = #{ 'variadic?'     => true
            , max_fixed_arity => 1
            , variadic_arity  => 1
            },
  Var42Variadic = clj_core:with_meta(Var42, Meta42),
  43 = clj_core:apply(Var42Variadic, [1]),
  47 = clj_core:apply(Var42Variadic, [3, 2]),
  50 = clj_core:apply(Var42Variadic, [3, 2, 1, 2]),

  false = 'clojerl.Var':is_dynamic(Var42Variadic),
  false = 'clojerl.Var':is_macro(Var42Variadic),

  'forty-two' = 'clojerl.Var':function(Var42Variadic),

  Var43  = 'clojerl.Var':?CONSTRUCTOR(Ns, Name43),
  Meta43 = #{ 'variadic?'     => true
            , max_fixed_arity => ?NIL
            , variadic_arity  => 1
            },
  Var43Variadic = clj_core:with_meta(Var43, Meta43),
  [43, 1, ?NIL]       = clj_core:apply(Var43Variadic, [1]),
  [43, 1, [2]]    = clj_core:apply(Var43Variadic, [1, 2]),
  [43, 1, [2, 3]] = clj_core:apply(Var43Variadic, [1, 2, 3]),

  {comments, ""}.

-spec meta(config()) -> result().
meta(_Config) ->
  Ns   = <<"clojerl_Var_SUITE">>,
  Name = <<"forty-two">>,

  Var = clj_core:with_meta('clojerl.Var':?CONSTRUCTOR(Ns, Name), #{a => 1}),
  #{a := 1} = clj_core:meta(Var),

  {comments, ""}.

-spec name(config()) -> result().
name(_Config) ->
  Ns   = <<"clojerl_Var_SUITE">>,
  Name = <<"forty-two">>,
  Var  = clj_core:with_meta('clojerl.Var':?CONSTRUCTOR(Ns, Name), #{a => 1}),

  <<"clojerl_Var_SUITE">> = clj_core:namespace(Var),
  <<"forty-two">> = clj_core:name(Var),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Ns   = <<"clojerl_Var_SUITE">>,
  Name = <<"forty-two">>,
  Var  = clj_core:with_meta('clojerl.Var':?CONSTRUCTOR(Ns, Name), #{a => 1}),

  <<"#'clojerl_Var_SUITE/forty-two">> = clj_core:str(Var),

  {comments, ""}.

-spec dynamic_bindings(config()) -> result().
dynamic_bindings(_Config) ->
  Ns     = <<"clojerl_Var_SUITE">>,
  Name   = <<"forty-two">>,
  Var    = clj_core:with_meta('clojerl.Var':?CONSTRUCTOR(Ns, Name), #{a => 1}),
  VarStr = clj_core:str(Var),

  #{} = 'clojerl.Var':get_bindings_map(),

  ct:comment("deref'd value should be the root binding"),
  42 = clj_core:deref(Var),

  ct:comment("Change value"),
  clj_core:'set!'(Var, 85),
  85 = clj_core:deref(Var),

  clj_core:'set!'(Var, 42),

  ct:comment("deref'd value should be the dynamic binding"),
  'clojerl.Var':push_bindings(#{Var => 84}),

  84 = clj_core:deref(Var),

  clj_core:'set!'(Var, 85),

  85 = clj_core:deref(Var),

  #{VarStr := 85} = 'clojerl.Var':get_bindings_map(),

  Bindings = 'clojerl.Var':get_bindings(),

  'clojerl.Var':pop_bindings(),

  42 = clj_core:deref(Var),

  'clojerl.Var':reset_bindings(Bindings),
  85 = clj_core:deref(Var),
  'clojerl.Var':pop_bindings(),

  42 = clj_core:deref(Var),

  ct:comment("Assign value nil to a var"),
  'clojerl.Var':push_bindings(#{Var => ?NIL}),
  ?NIL = clj_core:deref(Var),
  'clojerl.Var':pop_bindings(),

  42 = clj_core:deref(Var),

  ct:comment("Nested bindings"),
  'clojerl.Var':push_bindings(#{Var => 43}),
  43 = clj_core:deref(Var),

  'clojerl.Var':push_bindings(#{}),
  43 = clj_core:deref(Var),
  clj_core:'set!'(Var, 44),
  44 = clj_core:deref(Var),

  'clojerl.Var':pop_bindings(),
  44 = clj_core:deref(Var),

  'clojerl.Var':pop_bindings(),
  42 = clj_core:deref(Var),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  Ns     = <<"clojerl_Var_SUITE">>,
  Name   = <<"forty-two">>,
  Var    = 'clojerl.Var':?CONSTRUCTOR(Ns, Name),

  VarPrivate = clj_core:with_meta(Var, #{private => true}),
  VarRoot    = clj_core:with_meta(Var, #{has_root => true}),

  true  = 'clojerl.Var':is_public(Var),
  false = 'clojerl.Var':is_public(VarPrivate),

  true  = 'clojerl.Var':has_root(VarRoot),
  false = 'clojerl.Var':has_root(Var),

  42    = 'clojerl.Var':get(Var),

  Hash  = 'clojerl.IHash':hash(Var),
  Hash  = 'clojerl.IHash':hash(Var),
  true  = erlang:is_integer(Hash),

  VarFortyTwo  = 'clojerl.Var':?CONSTRUCTOR(Ns, Name),
  VarFortyFive = 'clojerl.Var':?CONSTRUCTOR(Ns, <<"forty-five">>),

  true  = 'clojerl.Var':is_bound(VarFortyTwo),
  false = 'clojerl.Var':is_bound(VarFortyFive),

  {comments, ""}.
