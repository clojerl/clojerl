-module(clojerl_List_SUITE).

-export([all/0, init_per_suite/1]).

-export([ new/1
        , count/1
        , str/1
        , is_sequential/1
        , hash/1
        , seq/1
        , equiv/1
        , cons/1
        , stack/1
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
  application:ensure_all_started(clojerl),
  Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec new(config()) -> result().
new(_Config) ->
  List = clj_core:list([1, 2, 3]),
  [1, 2, 3] = clj_core:seq(List),

  List2 = clj_core:list([]),
  undefined = clj_core:seq(List2),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  List = clj_core:list([1, 2, 3]),
  3 = clj_core:count(List),

  List2 = clj_core:list([]),
  0 = clj_core:count(List2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  List = clj_core:list([1, 2, 3]),
  <<"(1 2 3)">> = clj_core:str(List),

  List2 = clj_core:list([]),
  <<"()">> = clj_core:str(List2),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  List = clj_core:list([1, 2, 3]),
  true = clj_core:'sequential?'(List),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  List1 = clj_core:list([1, 2, 3]),
  List2 = clj_core:list([1, 3, 2]),
  List3 = clj_core:list([1.0, 2, 3]),

  Hash1 = 'clojerl.IHash':hash(List1),
  Hash2 = 'clojerl.IHash':hash(List2),
  Hash3 = 'clojerl.IHash':hash(List3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  List = clj_core:list([1, 2, 3]),
  1 = clj_core:first(List),
  [2, 3] = clj_core:seq(clj_core:next(List)),
  [2, 3] = clj_core:seq(clj_core:rest(List)),

  List2 = clj_core:list([1]),
  1 = clj_core:first(List2),
  undefined = clj_core:next(List2),
  [] = clj_core:seq_to_list(clj_core:rest(List2)),

  List3 = clj_core:list([]),
  undefined = clj_core:first(List3),
  undefined = clj_core:next(List3),
  undefined = clj_core:rest(List3),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that lists with the same elements are equivalent"),
  List1 = clj_core:with_meta(clj_core:list([1, 2, 3]), #{a => 1}),
  List2 = clj_core:with_meta(clj_core:list([1, 2, 3]), #{b => 2}),

  true  = clj_core:equiv(List1, List2),

  ct:comment("Check that lists with the same elements are not equivalent"),
  List3 = clj_core:with_meta(clj_core:list([1, 2, 3, 4]), #{c => 3}),
  false = clj_core:equiv(List1, List3),

  ct:comment("A clojerl.List and an clojerl.erlang.List"),
  true = clj_core:equiv(List1, [1, 2, 3]),
  false = clj_core:equiv(List1, [1, 2, 3, a]),

  ct:comment("A clojerl.List and something else"),
  false = clj_core:equiv(List1, whatever),
  false = clj_core:equiv(List1, #{}),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  EmptyList = clj_core:list([]),

  ct:comment("Conj an element to an empty list"),
  OneList = clj_core:conj(EmptyList, 1),

  1    = clj_core:count(OneList),
  true = clj_core:equiv(OneList, [1]),

  ct:comment("Conj an element to a list with one element"),
  TwoList = clj_core:conj(OneList, 2),

  2    = clj_core:count(TwoList),
  true = clj_core:equiv(TwoList, [2, 1]),

  {comments, ""}.

-spec stack(config()) -> result().
stack(_Config) ->
  EmptyList = clj_core:list([]),
  undefined = clj_core:peek(EmptyList),
  EmptyList = clj_core:pop(EmptyList),

  OneList   = clj_core:list([1]),
  1         = clj_core:peek(OneList),
  EmptyList = clj_core:pop(OneList),

  TwoList   = clj_core:list([2, 1]),
  2         = clj_core:peek(TwoList),
  OneList = clj_core:pop(TwoList),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ok = 'clojerl.List':noop(ok),

  NotEmptyList = clj_core:list([a, b, 2, 3]),
  EmptyList    = clj_core:empty(NotEmptyList),
  EmptyList    = clj_core:list([]),

  ListMeta  = clj_core:with_meta(clj_core:list([1, 2, 3]), #{a => 1}),
  #{a := 1} = clj_core:meta(ListMeta),

  {comments, ""}.
