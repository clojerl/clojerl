-module(clojerl_BitString_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ count/1
        , seq/1
        , str/1
        , hash/1
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

-spec count(config()) -> result().
count(_Config) ->
  2  = clj_rt:count(<<1:1, 1:1>>),
  11 = clj_rt:count(<<1:8, 1:3>>),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  <<"#bin[1 2 3 [1 :unit 1 :size 1]]">> = clj_rt:str(<<1, 2, 3, 1:1>>),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  BitString = <<1, 2, 3, 1:1>>,
  4 = clj_rt:count(clj_rt:seq(BitString)),
  [1, 2, 3, <<1:1>>] = clj_rt:seq(BitString),

  ?NIL = 'clojerl.BitString':seq(<<>>),

  [1, 2, 3, <<1:1>>] = clj_rt:to_list(<<1, 2, 3, 1:1>>),

  {comments, ""}.

hash(_Config) ->
  BitString = <<1, 2, 3, 1:1>>,

  Hash = 'clojerl.IHash':hash(BitString),
  Hash = 'clojerl.IHash':hash(BitString),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ?NIL = 'clojerl.BitString':'_'(<<>>),

  {comments, ""}.
