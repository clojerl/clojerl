-module(clojerl_Nil_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ cons/1
        , hash/1
        , seq/1
        , str/1
        , complete_coverage/1
        ]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec cons(config()) -> result().
cons(_Config) ->
  Nil = ?NIL,

  ct:comment("Conj an element to nil"),
  OneList = clj_core:conj(Nil, 1),

  1    = clj_core:count(OneList),
  true = clj_core:equiv(OneList, [1]),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  HashNil = 'clojerl.IHash':hash(?NIL),
  true = is_integer(HashNil),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Nil = ?NIL,

  ?NIL = clj_core:seq(Nil),

  ?NIL = clj_core:first(Nil),
  ?NIL = clj_core:next(Nil),
  [] = clj_core:rest(Nil),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  ct:comment("Check the string representation of true and false"),
  <<"">> = clj_core:str(?NIL),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  [] = 'clojerl.Nil':to_list(?NIL),

  {comments, ""}.
