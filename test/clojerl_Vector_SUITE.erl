-module(clojerl_Vector_SUITE).

-export([all/0, init_per_suite/1]).

-export([ new/1
        , count/1
        , str/1
        , is_sequential/1
        , hash/1
        , invoke/1
        , nth/1
        , seq/1
        , equiv/1
        , cons/1
        , subvec/1
        , stack/1
        , associative/1
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
  <<"[1 2 3]">> = clj_core:str(Vector),

  Vector2 = clj_core:vector([]),
  <<"[]">> = clj_core:str(Vector2),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  Vector = clj_core:vector([1, 2, 3]),
  true = clj_core:'sequential?'(Vector),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  Vector1 = clj_core:vector([1, 2, 3]),
  Vector2 = clj_core:vector([1, 3, 2]),
  Vector3 = clj_core:vector([1.0, 2, 3]),

  Hash1 = 'clojerl.IHash':hash(Vector1),
  Hash2 = 'clojerl.IHash':hash(Vector2),
  Hash3 = 'clojerl.IHash':hash(Vector3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

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
  [] = clj_core:to_list(clj_core:rest(Vector2)),

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

-spec subvec(config()) -> result().
subvec(_Config) ->
  OneToTen = clj_core:vector(lists:seq(1, 10)),

  ct:comment("Get subvec from 1 to 4, 0-based indexes"),
  TwoToFive = 'clojerl.Vector':subvec(OneToTen, 1, 4),
  [2, 3, 4] = clj_core:to_list(TwoToFive),

  {comments, ""}.

-spec stack(config()) -> result().
stack(_Config) ->
  EmptyVector = clj_core:vector([]),
  undefined = clj_core:peek(EmptyVector),
  ok = try EmptyVector = clj_core:pop(EmptyVector), error
       catch _:_ -> ok
       end,

  OneVector = clj_core:vector([1]),
  1         = clj_core:peek(OneVector),
  true      = clj_core:equiv(clj_core:pop(OneVector), EmptyVector),

  TwoVector   = clj_core:vector([1, 2]),
  2         = clj_core:peek(TwoVector),
  true      = clj_core:equiv(clj_core:pop(TwoVector), OneVector),

  {comments, ""}.

-spec associative(config()) -> result().
associative(_Config) ->
  EmptyVector = clj_core:vector([]),

  OneVector  = clj_core:assoc(EmptyVector, 0, a),
  TwoVector  = clj_core:assoc(OneVector, 1, b),
  OneVector2 = clj_core:assoc(TwoVector, 0, c),

  [a]    = clj_core:to_list(OneVector),
  [a, b] = clj_core:to_list(TwoVector),
  [c, b] = clj_core:to_list(OneVector2),

  ok = try clj_core:assoc(EmptyVector, 2, a), error
       catch error:_ -> ok
       end,

  ok = try clj_core:assoc(OneVector, 2, a), error
       catch error:_ -> ok
       end,

  a = clj_core:find(TwoVector, 0),
  b = clj_core:find(TwoVector, 1),

  undefined = clj_core:find(TwoVector, 3),

  true = clj_core:'contains?'(OneVector, 0),
  true = clj_core:'contains?'(TwoVector, 1),

  false = clj_core:'contains?'(EmptyVector, 1),
  false = clj_core:'contains?'(OneVector, 1),
  false = clj_core:'contains?'(TwoVector, 2),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  undefined = 'clojerl.Vector':'_'(undefined),

  NotEmptyVector = clj_core:vector([a, b, 2, 3]),
  EmptyVector    = clj_core:empty(NotEmptyVector),
  EmptyVector    = clj_core:vector([]),

  VectorMeta  = clj_core:with_meta(clj_core:vector([1, 2, 3]), #{a => 1}),
  #{a := 1} = clj_core:meta(VectorMeta),

  a = clj_core:get(NotEmptyVector, 0),
  b = clj_core:get(NotEmptyVector, 1),
  2 = clj_core:get(NotEmptyVector, 2),
  3 = clj_core:get(NotEmptyVector, 3),
  undefined = clj_core:get(NotEmptyVector, 4),
  bla = clj_core:get(NotEmptyVector, 43, bla),

  {comments, ""}.
