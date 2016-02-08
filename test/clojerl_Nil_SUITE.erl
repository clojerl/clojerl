-module(clojerl_Nil_SUITE).

-export([all/0, init_per_suite/1]).

-export([ cons/1
        , seq/1
        , str/1
        ]).

-type config() :: list().
-type result() :: {comments, string()}.

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(clojerl),
  Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec cons(config()) -> result().
cons(_Config) ->
  Nil = undefined,

  ct:comment("Conj an element to nil"),
  OneList = clj_core:conj(Nil, 1),

  1    = clj_core:count(OneList),
  true = clj_core:equiv(OneList, [1]),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Nil = undefined,

  undefined = clj_core:seq(Nil),

  undefined = clj_core:first(Nil),
  undefined = clj_core:next(Nil),
  undefined = clj_core:rest(Nil),

  {comments, ""}.


-spec str(config()) -> result().
str(_Config) ->
  ct:comment("Check the string representation of true and false"),
  <<"">> = clj_core:str(undefined),

  {comments, ""}.
