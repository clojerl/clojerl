-module(clojerl_reader_ReaderConditional_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([all/0, init_per_suite/1]).

-export([ equiv/1
        , hash/1
        , get/1
        , str/1
        ]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Compare two different reader conditionals"),
  ReadCond  = 'clojerl.reader.ReaderConditional':?CONSTRUCTOR([], false),
  ReadCond1 = 'clojerl.reader.ReaderConditional':?CONSTRUCTOR([a, b], true),
  false = clj_core:equiv(ReadCond, ReadCond1),

  ct:comment("Compare the same reader conditional"),
  true = clj_core:equiv(ReadCond, ReadCond),

  ct:comment("Compare a reader conditional with something else"),
  false = clj_core:equiv(ReadCond, 42),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  ct:comment("Hash for different reader conditionals are different"),
  ReadCond  = 'clojerl.reader.ReaderConditional':?CONSTRUCTOR([], false),
  ReadCond1 = 'clojerl.reader.ReaderConditional':?CONSTRUCTOR([a, b], true),

  Hash1 = 'clojerl.IHash':hash(ReadCond),
  Hash2 = 'clojerl.IHash':hash(ReadCond1),

  true = Hash1 =/= Hash2,

  {comments, ""}.

-spec get(config()) -> result().
get(_Config) ->
  ct:comment("Only :form and :splicing? keys return something"),

  ReadCond  = 'clojerl.reader.ReaderConditional':?CONSTRUCTOR([a, b], true),
  [a, b]    = clj_core:get(ReadCond, form),
  true      = clj_core:get(ReadCond, 'splicing?'),
  ?NIL      = clj_core:get(ReadCond, bla),
  not_found = clj_core:get(ReadCond, bla, not_found),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  ct:comment("Check the str representation of reader cond with no splicing"),
  ReadCond = 'clojerl.reader.ReaderConditional':?CONSTRUCTOR([], false),
  Regex = <<"#?\\(\\)">>,
  {match, _} = re:run(clj_core:str(ReadCond), Regex),

  ct:comment("Check the str representation of reader cond with splicing"),
  ReadCond1 = 'clojerl.reader.ReaderConditional':?CONSTRUCTOR([a, b], true),
  SplicingRegex = <<"#?@\\(:a :b\\)">>,
  {match, _} = re:run(clj_core:str(ReadCond1), SplicingRegex),

  {comments, ""}.
