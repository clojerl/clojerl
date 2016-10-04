-module(clojerl_Reduced_SUITE).

-include("clojerl.hrl").

-export([all/0, init_per_suite/1]).

-export([ deref/1
        , hash/1
        , str/1
        , complete_coverage/1
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
  ok = clojerl:start(),
  Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec deref(config()) -> result().
deref(_Config) ->
  ct:comment("Deref reduced"),
  Value   = [some, value],
  Reduced = 'clojerl.Reduced':?CONSTRUCTOR(Value),
  Value   = clj_core:deref(Reduced),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  ct:comment("Hash for different reduced are different"),
  Value1   = [some, value],
  Value2   = [some, other, value],
  Reduced1 = 'clojerl.Reduced':?CONSTRUCTOR(Value1),
  Reduced2 = 'clojerl.Reduced':?CONSTRUCTOR(Value2),

  Hash1 = 'clojerl.IHash':hash(Reduced1),
  Hash2 = 'clojerl.IHash':hash(Reduced2),

  true = Hash1 =/= Hash2,

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  ct:comment("Check the str representation of tagged literals"),
  Value1   = [some, value],
  Reduced1 = 'clojerl.Reduced':?CONSTRUCTOR(Value1),
  Regex1 = <<"#<clojerl.Reduced \\(:some :value\\)">>,
  {match, _} = re:run(clj_core:str(Reduced1), Regex1),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  Reduced1 = 'clojerl.Reduced':?CONSTRUCTOR(value),
  true     = 'clojerl.Reduced':is_reduced(Reduced1),
  false    = 'clojerl.Reduced':is_reduced(value),

  {comments, ""}.
