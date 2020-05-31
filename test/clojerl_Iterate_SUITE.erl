-module(clojerl_Iterate_SUITE).

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
  Iterate = create_iterate(),
  Iterate = clj_rt:seq(Iterate),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Iterate = create_iterate(),
  ok = try clj_rt:count(Iterate), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Iterate = create_iterate(),
  <<"#<clojerl.Iterate>">> = clj_rt:str(Iterate),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  Iterate = create_iterate(),
  true  = clj_rt:'sequential?'(Iterate),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  Iterate1 = create_iterate(),
  Iterate2 = create_iterate(fun(X) -> X * 2  end, 1),
  Iterate3 = create_iterate(fun(X) -> X + 1 end, 0.5),

  Hash1 = 'clojerl.IHash':hash(Iterate1),
  Hash2 = 'clojerl.IHash':hash(Iterate2),
  Hash3 = 'clojerl.IHash':hash(Iterate3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Iterate = create_iterate(),
  Next0  = clj_rt:next(Iterate),
  1 = clj_rt:first(Iterate),
  2 = clj_rt:first(Next0),
  2 = clj_rt:first(clj_rt:rest(Iterate)),

  Next1 = clj_rt:next(Next0),
  3 = clj_rt:first(Next1),

  Next2 = clj_rt:next(Next1),
  4 = clj_rt:first(Next2),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that lists with the same elements are equivalent"),
  Iterate1 = clj_rt:with_meta(create_iterate(), #{a => 1}),
  Iterate2 = clj_rt:with_meta(create_iterate(), #{b => 2}),

  true   = clj_rt:equiv(Iterate1, Iterate2),

  ct:comment("Check that iterates with different elements are not equivalent"),
  Iterate3 = clj_rt:with_meta(create_iterate(0.5), #{c => 3}),
  false = clj_rt:equiv(Iterate1, Iterate3),

  ct:comment("A clojerl.Iterate and a list"),
  false = clj_rt:equiv(Iterate1, [1, 2, 3]),
  false = clj_rt:equiv(Iterate1, [1, 2, 3, a]),

  ct:comment("A clojerl.Iterate and something else"),
  false = clj_rt:equiv(Iterate1, whatever),
  false = clj_rt:equiv(Iterate1, #{}),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  Iterate0 = create_iterate(),

  ct:comment("Conj an element to a iterate"),
  Iterate1 = clj_rt:conj(Iterate0, 1),

  ok = try clj_rt:count(Iterate1), error
       catch _:_ -> ok
       end,

  ct:comment("Conj another element"),
  Iterate2 = clj_rt:conj(Iterate1, 2),

  ok = try clj_rt:count(Iterate2), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec reduce(config()) -> result().
reduce(_Config) ->
  TenIterate   = create_iterate(fun(X) -> X end, 1),
  PlusMaxFun = fun
                 (X, Y) when X < 10 -> X + Y;
                 (X, _) -> 'clojerl.Reduced':?CONSTRUCTOR(X)
               end,
  10   = 'clojerl.IReduce':reduce(TenIterate, PlusMaxFun),
  10.5 = 'clojerl.IReduce':reduce(TenIterate, PlusMaxFun, 0.5),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  NotEmptyIterate = create_iterate(),
  EmptyIterate    = clj_rt:empty(NotEmptyIterate),
  true          = clj_rt:equiv(EmptyIterate, []),

  IterateMeta = clj_rt:with_meta(create_iterate(), #{a => 1}),
  #{a := 1} = clj_rt:meta(IterateMeta),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec create_iterate() -> 'clojerl.Iterate':type().
create_iterate() ->
  create_iterate(fun(X) -> X + 1 end, 1).

-spec create_iterate(any()) -> 'clojerl.Iterate':type().
create_iterate(Init) ->
  create_iterate(fun(X) -> X + 1 end, Init).

-spec create_iterate(any(), any()) -> 'clojerl.Iterate':type().
create_iterate(Fn, Init) ->
  'clojerl.Iterate':?CONSTRUCTOR(Fn, Init).
