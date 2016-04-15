-module(clojerl_Vector_SUITE).

-export([all/0, init_per_suite/1]).

-export([ new/1
        , count/1
        , str/1
        , is_sequential/1
        , invoke/1
        , nth/1
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
  Vector = clj_core:vector([1, 2, 3]),
  [1, 2, 3] = clj_core:seq(Vector),

  Vector2 = clj_core:vector([]),
  undefined = clj_core:seq(Vector2),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Vector = clj_core:vector([1, 2, 3]),
  3 = clj_core:count(Vector),

  Vector2 = clj_core:vector([]),
  0 = clj_core:count(Vector2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Vector = clj_core:vector([1, 2, 3]),
  <<"[1, 2, 3]">> = clj_core:str(Vector),

  Vector2 = clj_core:vector([]),
  <<"[]">> = clj_core:str(Vector2),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  Vector = clj_core:vector([1, 2, 3]),
  true = clj_core:'sequential?'(Vector),

  {comments, ""}.

-spec invoke(config()) -> result().
invoke(_Config) ->
  ct:comment("Invoke a vector "),
  Vector = clj_core:vector([1, b, 3]),
  1 = clj_core:invoke(Vector, [0]),
  b = clj_core:invoke(Vector, [1]),
  3 = clj_core:invoke(Vector, [2]),

  ct:comment("Invoke a vector with two arguments"),
  ok = try clj_core:invoke(Vector, [1, 2]), error
       catch _:_ -> ok end,

  {comments, ""}.

-spec nth(config()) -> result().
nth(_Config) ->
  Vector = clj_core:vector([1, b, 3]),

  1 = clj_core:nth(Vector, 0),
  b = clj_core:nth(Vector, 1),
  3 = clj_core:nth(Vector, 2),

  3 = clj_core:nth(Vector, 2, not_found),
  not_found = clj_core:nth(Vector, 42, not_found),

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Vector = clj_core:vector([1, 2, 3]),
  1 = clj_core:first(Vector),
  [2, 3] = clj_core:seq(clj_core:next(Vector)),
  [2, 3] = clj_core:seq(clj_core:rest(Vector)),

  Vector2 = clj_core:vector([1]),
  1 = clj_core:first(Vector2),
  undefined = clj_core:next(Vector2),
  [] = clj_core:seq_to_list(clj_core:rest(Vector2)),

  Vector3 = clj_core:vector([]),
  undefined = clj_core:first(Vector3),
  undefined = clj_core:next(Vector3),
  [] = clj_core:rest(Vector3),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that vectors with the same elements are equivalent"),
  Vector1 = clj_core:with_meta(clj_core:vector([1, 2, 3]), #{a => 1}),
  Vector2 = clj_core:with_meta(clj_core:vector([1, 2, 3]), #{b => 2}),

  true  = clj_core:equiv(Vector1, Vector2),

  ct:comment("Check that vectors with the same elements are not equivalent"),
  Vector3 = clj_core:with_meta(clj_core:vector([1, 2, 3, 4]), #{c => 3}),
  false = clj_core:equiv(Vector1, Vector3),

  ct:comment("A clojerl.Vector and an clojerl.erlang.List"),
  true = clj_core:equiv(Vector1, [1, 2, 3]),
  false = clj_core:equiv(Vector1, [1, 2, 3, a]),

  ct:comment("A clojerl.Vector and something else"),
  false = clj_core:equiv(Vector1, whatever),
  false = clj_core:equiv(Vector1, #{}),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  EmptyVector = clj_core:vector([]),

  ct:comment("Conj an element to an empty vector"),
  OneVector = clj_core:conj(EmptyVector, 1),

  1    = clj_core:count(OneVector),
  true = clj_core:equiv(OneVector, [1]),

  ct:comment("Conj an element to a vector with one element"),
  TwoVector = clj_core:conj(OneVector, 2),

  2    = clj_core:count(TwoVector),
  true = clj_core:equiv(TwoVector, [1, 2]),

  {comments, ""}.

-spec stack(config()) -> result().
stack(_Config) ->
  EmptyVector = clj_core:vector([]),
  undefined = clj_core:peek(EmptyVector),
  ok = try EmptyVector = clj_core:pop(EmptyVector), error
       catch _ -> ok
       end,

  OneVector = clj_core:vector([1]),
  1         = clj_core:peek(OneVector),
  true      = clj_core:equiv(clj_core:pop(OneVector), EmptyVector),

  TwoVector   = clj_core:vector([1, 2]),
  2         = clj_core:peek(TwoVector),
  true      = clj_core:equiv(clj_core:pop(TwoVector), OneVector),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ok = 'clojerl.Vector':'clojerl.ISequential.noop'(ok),

  NotEmptyVector = clj_core:vector([a, b, 2, 3]),
  EmptyVector    = clj_core:empty(NotEmptyVector),
  EmptyVector    = clj_core:vector([]),

  VectorMeta  = clj_core:with_meta(clj_core:vector([1, 2, 3]), #{a => 1}),
  #{a := 1} = clj_core:meta(VectorMeta),

  {comments, ""}.
