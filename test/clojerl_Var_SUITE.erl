-module(clojerl_Var_SUITE).

-export([all/0, init_per_suite/1]).

-export([ 'forty-two__val'/0
        , 'forty-two'/0
        , 'forty-two'/1
        , 'forty-two'/2
        ]).

-export([ deref/1
        , equiv/1
        , invoke/1
        , meta/1
        , name/1
        , str/1
        ]).

-type config() :: list().
-type result() :: {comments, string()}.

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
'forty-two__val'() -> 42.

-spec 'forty-two'() -> any().
'forty-two'() -> 42.

-spec 'forty-two'(integer()) -> any().
'forty-two'(X) -> 42 + X.

-spec 'forty-two'(integer(), [integer()]) -> any().
'forty-two'(X, XS) ->
  Sum = lists:foldl(fun erlang:'+'/2, 0, XS),
  42 + X + Sum.

-spec deref(config()) -> result().
deref(_Config) ->
  Ns   = clj_core:symbol(<<"clojerl_Var_SUITE">>),
  Name = clj_core:symbol(<<"forty-two">>),

  Var  = 'clojerl.Var':new(Ns, Name),
  42 = clj_core:deref(Var),  

  ct:comment("Try to deref an unexisting var"),
  Name2 = clj_core:symbol(<<"forty-three">>),
  Var1  = 'clojerl.Var':new(Ns, Name2),
  ok = try clj_core:deref(Var1), error
       catch _:_ -> ok 
       end,

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  Ns   = clj_core:symbol(<<"clojerl_Var_SUITE">>),
  Name = clj_core:symbol(<<"forty-two">>),

  ct:comment("Check that vars with the same name are equivalent"),
  Var1 = clj_core:with_meta('clojerl.Var':new(Ns, Name), #{a => 1}),
  Var2 = clj_core:with_meta('clojerl.Var':new(Ns, Name), #{b => 2}),
  true  = clj_core:equiv(Var1, Var2),

  ct:comment("Check that vars with the same elements are not equivalent"),
  Name2 = clj_core:symbol(<<"whatever">>),
  Var3 = clj_core:with_meta('clojerl.Var':new(Ns, Name2), #{c => 3}),
  false = clj_core:equiv(Var1, Var3),

  ct:comment("A clojerl.Var and something else"),
  false = clj_core:equiv(Var1, whatever),
  false = clj_core:equiv(Var1, #{}),
  false = clj_core:equiv(Var1, Ns),

  {comments, ""}.

-spec invoke(config()) -> result().
invoke(_Config) ->
  Ns   = clj_core:symbol(<<"clojerl_Var_SUITE">>),
  Name = clj_core:symbol(<<"forty-two">>),

  Var  = 'clojerl.Var':new(Ns, Name),
  42 = clj_core:invoke(Var, []),
  45 = clj_core:invoke(Var, [3]),

  false = 'clojerl.Var':is_dynamic(Var),
  false = 'clojerl.Var':is_macro(Var),

  Meta = #{ 'variadic?'     => true
          , max_fixed_arity => 1
          , variadic_arity  => 1
          },
  VarVariadic = clj_core:with_meta(Var, Meta),
  43 = clj_core:invoke(VarVariadic, [1]),
  47 = clj_core:invoke(VarVariadic, [3, 2]),
  50 = clj_core:invoke(VarVariadic, [3, 2, 1, 2]),

  false = 'clojerl.Var':is_dynamic(VarVariadic),
  false = 'clojerl.Var':is_macro(VarVariadic),

  'forty-two' = 'clojerl.Var':function(VarVariadic),

  {comments, ""}.

-spec meta(config()) -> result().
meta(_Config) ->
  Ns   = clj_core:symbol(<<"clojerl_Var_SUITE">>),
  Name = clj_core:symbol(<<"forty-two">>),

  Var = clj_core:with_meta('clojerl.Var':new(Ns, Name), #{a => 1}),
  #{a := 1} = clj_core:meta(Var),

  {comments, ""}.

-spec name(config()) -> result().
name(_Config) ->
  Ns   = clj_core:symbol(<<"clojerl_Var_SUITE">>),
  Name = clj_core:symbol(<<"forty-two">>),
  Var  = clj_core:with_meta('clojerl.Var':new(Ns, Name), #{a => 1}),

  <<"clojerl_Var_SUITE">> = clj_core:namespace(Var),
  <<"forty-two">> = clj_core:name(Var),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Ns   = clj_core:symbol(<<"clojerl_Var_SUITE">>),
  Name = clj_core:symbol(<<"forty-two">>),
  Var  = clj_core:with_meta('clojerl.Var':new(Ns, Name), #{a => 1}),

  <<"#'clojerl_Var_SUITE/forty-two">> = clj_core:str(Var),

  {comments, ""}.
