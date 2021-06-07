-module(clojerl_StringSeq_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ new/1
        , count/1
        , str/1
        , is_sequential/1
        , hash/1
        , seq/1
        , equiv/1
        , cons/1
        , reduce/1
        , complete_coverage/1
        ]).

-clojure(true).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec new(config()) -> result().
new(_Config) ->
  StringSeq = clj_rt:seq(<<"foo">>),
  StringSeq = clj_rt:seq(StringSeq),

  ?NIL = clj_rt:seq(<<>>),

  Cons = clj_rt:cons(<<"!">>, StringSeq),
  [<<"!">>, <<"f">>, <<"o">>, <<"o">>] = clj_rt:to_list(Cons),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  StringSeq = clj_rt:seq(<<"foo">>),
  3 = clj_rt:count(StringSeq),

  Rest = clj_rt:next(StringSeq),
  2 = clj_rt:count(Rest),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  StringSeq = clj_rt:seq(<<"foo">>),
  <<"(\"f\" \"o\" \"o\")">> = clj_rt:str(StringSeq),

  Empty = clj_rt:seq(<<"">>),
  <<"">> = clj_rt:str(Empty),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  StringSeq = clj_rt:seq(<<"foo">>),
  true = clj_rt:'sequential?'(StringSeq),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  StringSeq1 = clj_rt:seq(<<"foo">>),
  StringSeq2 = clj_rt:seq(<<"ofo">>),
  StringSeq3 = clj_rt:seq(<<"Foo">>),

  Hash1 = 'clojerl.IHash':hash(StringSeq1),
  Hash2 = 'clojerl.IHash':hash(StringSeq2),
  Hash3 = 'clojerl.IHash':hash(StringSeq3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  StringSeq = clj_rt:seq(<<"foo">>),
  true = clj_rt:'seq?'(StringSeq),
  <<"f">> = clj_rt:first(StringSeq),
  [<<"o">>, <<"o">>] = clj_rt:to_list(clj_rt:next(StringSeq)),
  [<<"o">>, <<"o">>] = clj_rt:to_list(clj_rt:rest(StringSeq)),

  StringSeq2 = clj_rt:seq(<<"f">>),
  <<"f">> = clj_rt:first(StringSeq2),
  ?NIL = clj_rt:next(StringSeq2),
  [] = clj_rt:to_list(clj_rt:rest(StringSeq2)),

  StringSeq3 = 'clojerl.StringSeq':?CONSTRUCTOR(<<>>),
  true = clj_rt:'seq?'(StringSeq3),
  ?NIL = clj_rt:first(StringSeq3),
  ?NIL = clj_rt:next(StringSeq3),
  [] = clj_rt:rest(StringSeq3),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that a list of characters is equivalent"),
  StringSeq1 = clj_rt:seq(<<"foo">>),
  List = [<<"f">>, <<"o">>, <<"o">>],
  true = clj_rt:equiv(StringSeq1, List),

  ct:comment("Check that the same seqs are equivalent"),
  StringSeq2 = clj_rt:seq(<<"foo">>),
  true = clj_rt:equiv(StringSeq1, StringSeq2),

  ct:comment("Compare to something non-sequential"),
  false = 'clojerl.StringSeq':equiv(StringSeq1, foo),
  false = 'clojerl.StringSeq':equiv(StringSeq1, 42),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  Empty = 'clojerl.StringSeq':?CONSTRUCTOR(<<>>),

  ct:comment("Conj an element to an empty list"),
  OneList = clj_rt:conj(Empty, 1),

  1    = clj_rt:count(OneList),
  true = clj_rt:equiv(OneList, [1]),

  ct:comment("Conj an element to a list with one element"),
  TwoList = clj_rt:conj(OneList, 2),

  2    = clj_rt:count(TwoList),
  true = clj_rt:equiv(TwoList, [2, 1]),

  {comments, ""}.

-spec reduce(config()) -> result().
reduce(_Config) ->
  Append    = fun
                ([]) -> <<>>;
                ([Acc, L]) -> <<L/binary, Acc/binary>>
              end,
  AppendFun = 'clojerl.Fn':?CONSTRUCTOR(Append),
  Empty     = 'clojerl.StringSeq':?CONSTRUCTOR(<<>>),

  <<>>      = 'clojerl.IReduce':reduce(Empty, AppendFun),
  <<"foo">> = 'clojerl.IReduce':reduce(Empty, AppendFun, <<"foo">>),

  Word      = 'clojerl.StringSeq':?CONSTRUCTOR(<<"word">>),
  Drow      = <<"drow">>,
  Drow      = 'clojerl.IReduce':reduce(Word, AppendFun),
  Drows     = <<"drows">>,
  Drows     = 'clojerl.IReduce':reduce(Word, AppendFun, <<"s">>),

  UntilFun  = fun
                (Acc, <<"r">>) -> 'clojerl.Reduced':?CONSTRUCTOR(Acc);
                (Acc, L) -> <<L/binary, Acc/binary>>
              end,
  Ow        = <<"ow">>,
  Ow        = 'clojerl.IReduce':reduce(Word, UntilFun),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  StringSeq = clj_rt:seq(<<"foo">>),
  []        = clj_rt:empty(StringSeq),

  {comments, ""}.
