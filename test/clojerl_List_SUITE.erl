-module(clojerl_List_SUITE).

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
        , stack/1
        , reduce/1
        , to_erl/1
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
  List = clj_rt:list([1, 2, 3]),
  [1, 2, 3] = clj_rt:seq(List),

  List2 = clj_rt:list([]),
  List2 = clj_rt:list(?NIL),
  ?NIL = clj_rt:seq(List2),

  List = clj_rt:list(clj_rt:cons(1, [2, 3])),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  List = clj_rt:list([1, 2, 3]),
  3 = clj_rt:count(List),

  List2 = clj_rt:list([]),
  0 = clj_rt:count(List2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  List = clj_rt:list([1, 2, 3]),
  <<"(1 2 3)">> = clj_rt:str(List),

  List2 = clj_rt:list([]),
  <<"()">> = clj_rt:str(List2),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  List = clj_rt:list([1, 2, 3]),
  true = clj_rt:'sequential?'(List),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  List1 = clj_rt:list([1, 2, 3]),
  List2 = clj_rt:list([1, 3, 2]),
  List3 = clj_rt:list([1.0, 2, 3]),

  Hash1 = 'clojerl.IHash':hash(List1),
  Hash2 = 'clojerl.IHash':hash(List2),
  Hash3 = 'clojerl.IHash':hash(List3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  List = clj_rt:list([1, 2, 3]),
  1 = clj_rt:first(List),
  [2, 3] = clj_rt:seq(clj_rt:next(List)),
  [2, 3] = clj_rt:seq(clj_rt:rest(List)),

  List2 = clj_rt:list([1]),
  1 = clj_rt:first(List2),
  ?NIL = clj_rt:next(List2),
  [] = clj_rt:to_list(clj_rt:rest(List2)),

  List3 = clj_rt:list([]),
  ?NIL = clj_rt:first(List3),
  ?NIL = clj_rt:next(List3),
  ?NIL = clj_rt:rest(List3),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that lists with the same elements are equivalent"),
  List1 = clj_rt:with_meta(clj_rt:list([1, 2, 3]), #{a => 1}),
  List2 = clj_rt:with_meta(clj_rt:list([1, 2, 3]), #{b => 2}),

  true  = clj_rt:equiv(List1, List2),

  ct:comment("Check that lists with the same elements are not equivalent"),
  List3 = clj_rt:with_meta(clj_rt:list([1, 2, 3, 4]), #{c => 3}),
  false = clj_rt:equiv(List1, List3),

  ct:comment("A clojerl.List and an erlang.List"),
  true = clj_rt:equiv(List1, [1, 2, 3]),
  false = clj_rt:equiv(List1, [1, 2, 3, a]),

  ct:comment("A clojerl.List and something else"),
  false = clj_rt:equiv(List1, whatever),
  false = clj_rt:equiv(List1, #{}),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  EmptyList = clj_rt:list([]),

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
  EmptyList = clj_rt:list([]),
  ?NIL = clj_rt:peek(EmptyList),
  EmptyList = clj_rt:pop(EmptyList),

  OneList   = clj_rt:list([1]),
  1         = clj_rt:peek(OneList),
  EmptyList = clj_rt:pop(OneList),

  TwoList   = clj_rt:list([2, 1]),
  2         = clj_rt:peek(TwoList),
  OneList = clj_rt:pop(TwoList),

  {comments, ""}.

-spec reduce(config()) -> result().
reduce(_Config) ->
  PlusFun = fun
              ([]) -> 0;
              ([X, Y]) -> X + Y
            end,
  EmptyList = clj_rt:list([]),

  0  = 'clojerl.IReduce':reduce(EmptyList, PlusFun),
  42 = 'clojerl.IReduce':reduce(EmptyList, PlusFun, 42),

  TenList = clj_rt:list(lists:seq(1, 10)),
  55 = 'clojerl.IReduce':reduce(TenList, PlusFun),
  60 = 'clojerl.IReduce':reduce(TenList, PlusFun, 5),

  PlusMaxFun = fun
                 ([]) -> 0;
                 ([X, Y]) when X < 10 -> X + Y;
                 ([X, _]) -> 'clojerl.Reduced':?CONSTRUCTOR(X)
            end,
  Reduced = 'clojerl.IReduce':reduce(TenList, PlusMaxFun),
  10 = clj_rt:deref(Reduced),

  {comments, ""}.

-spec to_erl(config()) -> result().
to_erl(_Config) ->
  List1     = clj_rt:list([1, 2, 3]),
  [1, 2, 3] = clj_rt:'->erl'(List1, false),
  [1, 2, 3] = clj_rt:'->erl'(List1, true),

  List2          = clj_rt:list([1, List1]),
  [1, List1]     = clj_rt:'->erl'(List2, false),
  [1, [1, 2, 3]] = clj_rt:'->erl'(List2, true),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ?NIL = 'clojerl.List':'_'(?NIL),

  NotEmptyList = clj_rt:list([a, b, 2, 3]),
  EmptyList    = clj_rt:empty(NotEmptyList),
  EmptyList    = clj_rt:list([]),

  ListMeta  = clj_rt:with_meta(clj_rt:list([1, 2, 3]), #{a => 1}),
  #{a := 1} = clj_rt:meta(ListMeta),

  {comments, ""}.
