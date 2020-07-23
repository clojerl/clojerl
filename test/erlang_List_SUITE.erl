-module(erlang_List_SUITE).

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
        , get/1
        , hash/1
        , seq/1
        , equiv/1
        , cons/1
        , stack/1
        , to_clj/1
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
  List = [1, 2, 3],
  [1, 2, 3] = clj_rt:seq(List),

  List2 = [],
  ?NIL = clj_rt:seq(List2),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  List = [1, 2, 3],
  3 = clj_rt:count(List),

  List2 = [],
  0 = clj_rt:count(List2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  <<"#erl(1 2 3)">> = clj_rt:str([1, 2, 3]),
  <<"">>            = clj_rt:str([]),
  <<"foo">>         = clj_rt:str("foo"),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  List = [1, 2, 3],
  true = clj_rt:'sequential?'(List),

  {comments, ""}.

-spec get(config()) -> result().
get(_Config) ->
  List = [{a, 1}, {b, 2}, {c, 3}],

  1    = clj_rt:get(List, a),
  2    = clj_rt:get(List, b),
  3    = clj_rt:get(List, c),

  ?NIL = clj_rt:get(List, d),
  42   = clj_rt:get(List, d, 42),

  ?NIL = clj_rt:get([], foo),
  ?NIL = clj_rt:get([a, 1, ""], foo),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  List1 = [1, 2, 3],
  List2 = [1, 3, 2],
  List3 = [1.0, 2, 3],

  Hash1 = 'clojerl.IHash':hash(List1),
  Hash2 = 'clojerl.IHash':hash(List2),
  Hash3 = 'clojerl.IHash':hash(List3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  List = [1, 2, 3],
  1 = clj_rt:first(List),
  [2, 3] = clj_rt:seq(clj_rt:next(List)),
  [2, 3] = clj_rt:seq(clj_rt:rest(List)),

  List2 = [1],
  1 = clj_rt:first(List2),
  ?NIL = clj_rt:next(List2),
  [] = clj_rt:to_list(clj_rt:rest(List2)),

  List3 = [],
  ?NIL = clj_rt:first(List3),
  ?NIL = clj_rt:next(List3),
  [] = clj_rt:rest(List3),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that lists with the same elements are equivalent"),
  List1 = [1, 2, 3],
  List2 = [1, 2, 3],

  true  = clj_rt:equiv(List1, List2),

  ct:comment("Check that lists with the same elements are not equivalent"),
  List3 = [a, b, c],
  false = clj_rt:equiv(List1, List3),

  ct:comment("A erlang.List and a clojerl.List"),
  true = clj_rt:equiv(List1, clj_rt:list([1, 2, 3])),
  false = clj_rt:equiv(List1, clj_rt:list([1, 2, 3, a])),

  ct:comment("A erlang.List and a clojerl.Vector"),
  true = clj_rt:equiv(List1, clj_rt:vector([1, 2, 3])),
  false = clj_rt:equiv(List1, clj_rt:vector([1, 2, 3, a])),

  ct:comment("A erlang.List and something else"),
  false = clj_rt:equiv(List1, whatever),
  false = clj_rt:equiv(List1, #{}),

  ct:comment("An empty list and other stuff"),
  false = clj_rt:equiv([], [1, 2]),
  false = clj_rt:equiv([1, 2], []),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  EmptyList = [],

  ct:comment("Conj an element to an empty list"),
  OneList = clj_rt:conj(EmptyList, 1),

  1    = clj_rt:count(OneList),
  true = clj_rt:equiv(OneList, [1]),

  ct:comment("Conj an element to a list with one element"),
  TwoList = clj_rt:conj(OneList, 2),

  2    = clj_rt:count(TwoList),
  true = clj_rt:equiv(TwoList, [2, 1]),

  {comments, ""}.

-spec stack(config()) -> result().
stack(_Config) ->
  EmptyList = [],
  ?NIL = clj_rt:peek(EmptyList),
  EmptyList = clj_rt:pop(EmptyList),

  OneList   = [1],
  1         = clj_rt:peek(OneList),
  EmptyList = clj_rt:pop(OneList),

  TwoList   = [2, 1],
  2         = clj_rt:peek(TwoList),
  OneList = clj_rt:pop(TwoList),

  {comments, ""}.

-spec to_clj(config()) -> result().
to_clj(_Config) ->
  List    = [1, 2, 3],
  CljList = clj_rt:'erl->clj'(List, true),
  true    = clj_rt:equiv(CljList, clj_rt:list([1, 2, 3])),

  EmptyList    = [],
  EmptyCljList = clj_rt:'erl->clj'(EmptyList, false),
  true         = clj_rt:equiv(EmptyCljList, clj_rt:list([])),

  {comments, ""}.

-spec reduce(config()) -> result().
reduce(_Config) ->
  PlusFun0  = fun
                ([]) -> 0;
                ([X, Y]) -> X + Y
              end,
  PlusFun   = 'clojerl.Fn':?CONSTRUCTOR(PlusFun0),
  EmptyList = [],

  0  = 'clojerl.IReduce':reduce(EmptyList, PlusFun),
  42 = 'clojerl.IReduce':reduce(EmptyList, PlusFun, 42),

  TenList = lists:seq(1, 10),
  55 = 'clojerl.IReduce':reduce(TenList, PlusFun),
  60 = 'clojerl.IReduce':reduce(TenList, PlusFun, 5),

  PlusMaxFun = fun
                 (X, Y) when X < 10 -> X + Y;
                 (X, _) -> 'clojerl.Reduced':?CONSTRUCTOR(X)
               end,
  10 = 'clojerl.IReduce':reduce(TenList, PlusMaxFun),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  NotEmptyList = [a, b, 2, 3],
  EmptyList    = clj_rt:empty(NotEmptyList),
  EmptyList    = [],

  ListMeta  = clj_rt:with_meta(clj_rt:list([1, 2, 3]), #{a => 1}),
  #{a := 1} = clj_rt:meta(ListMeta),

  [1, 2, 3] = 'erlang.List':to_list([1, 2, 3]),

  {comments, ""}.
