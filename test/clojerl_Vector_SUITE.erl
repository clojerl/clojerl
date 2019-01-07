-module(clojerl_Vector_SUITE).

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
        , apply/1
        , nth/1
        , seq/1
        , equiv/1
        , cons/1
        , subvec/1
        , stack/1
        , reduce/1
        , associative/1
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
  Vector = clj_rt:vector([1, 2, 3]),
  true   = clj_rt:equiv([1, 2, 3], clj_rt:seq(Vector)),

  Vector2 = clj_rt:vector([]),
  ?NIL = clj_rt:seq(Vector2),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Vector = clj_rt:vector([1, 2, 3]),
  3 = clj_rt:count(Vector),

  Vector2 = clj_rt:vector([]),
  0 = clj_rt:count(Vector2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Vector = clj_rt:vector([1, 2, 3]),
  <<"[1 2 3]">> = clj_rt:str(Vector),

  Vector2 = clj_rt:vector([]),
  <<"[]">> = clj_rt:str(Vector2),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  Vector = clj_rt:vector([1, 2, 3]),
  true = clj_rt:'sequential?'(Vector),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  Vector1 = clj_rt:vector([1, 2, 3]),
  Vector2 = clj_rt:vector([1, 3, 2]),
  Vector3 = clj_rt:vector([1.0, 2, 3]),

  Hash1 = 'clojerl.IHash':hash(Vector1),
  Hash2 = 'clojerl.IHash':hash(Vector2),
  Hash3 = 'clojerl.IHash':hash(Vector3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec apply(config()) -> result().
apply(_Config) ->
  ct:comment("Invoke a vector "),
  Vector = clj_rt:vector([1, b, 3]),
  1 = clj_rt:apply(Vector, [0]),
  b = clj_rt:apply(Vector, [1]),
  3 = clj_rt:apply(Vector, [2]),

  ct:comment("Invoke a vector with two arguments"),
  ok = try clj_rt:apply(Vector, [1, 2]), error
       catch _:_ -> ok end,

  {comments, ""}.

-spec nth(config()) -> result().
nth(_Config) ->
  Vector = clj_rt:vector([1, b, 3]),

  1 = clj_rt:nth(Vector, 0),
  b = clj_rt:nth(Vector, 1),
  3 = clj_rt:nth(Vector, 2),

  3 = clj_rt:nth(Vector, 2, not_found),
  not_found = clj_rt:nth(Vector, 42, not_found),

  ok = try clj_rt:nth(Vector, 3), error
       catch error:badarg -> ok end,

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Vector = clj_rt:vector([1, 2, 3]),
  1 = clj_rt:first(Vector),
  [2, 3] = clj_rt:seq(clj_rt:next(Vector)),
  [2, 3] = clj_rt:seq(clj_rt:rest(Vector)),

  [1, 2, 3] = clj_rt:to_list(Vector),

  Vector2 = clj_rt:vector([1]),
  1 = clj_rt:first(Vector2),
  ?NIL = clj_rt:next(Vector2),
  [] = clj_rt:to_list(clj_rt:rest(Vector2)),

  [1] = clj_rt:to_list(Vector2),

  Vector3 = clj_rt:vector([]),
  ?NIL = clj_rt:first(Vector3),
  ?NIL = clj_rt:next(Vector3),
  [] = clj_rt:rest(Vector3),

  [] = clj_rt:to_list(Vector3),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Check that vectors with the same elements are equivalent"),
  Vector1 = clj_rt:with_meta(clj_rt:vector([1, 2, 3]), #{a => 1}),
  Vector2 = clj_rt:with_meta(clj_rt:vector([1, 2, 3]), #{b => 2}),

  true  = clj_rt:equiv(Vector1, Vector2),

  ct:comment("Check that vectors with the same elements are not equivalent"),
  Vector3 = clj_rt:with_meta(clj_rt:vector([1, 2, 3, 4]), #{c => 3}),
  false = clj_rt:equiv(Vector1, Vector3),

  ct:comment("A clojerl.Vector and an erlang.List"),
  true = clj_rt:equiv(Vector1, [1, 2, 3]),
  false = clj_rt:equiv(Vector1, [1, 2, 3, a]),

  ct:comment("A clojerl.Vector and something else"),
  false = clj_rt:equiv(Vector1, whatever),
  false = clj_rt:equiv(Vector1, #{}),

  {comments, ""}.

-spec cons(config()) -> result().
cons(_Config) ->
  EmptyVector = clj_rt:vector([]),

  ct:comment("Conj an element to an empty vector"),
  OneVector = clj_rt:conj(EmptyVector, 1),

  1    = clj_rt:count(OneVector),
  true = clj_rt:equiv(OneVector, [1]),

  ct:comment("Conj an element to a vector with one element"),
  TwoVector = clj_rt:conj(OneVector, 2),

  2    = clj_rt:count(TwoVector),
  true = clj_rt:equiv(TwoVector, [1, 2]),

  {comments, ""}.

-spec subvec(config()) -> result().
subvec(_Config) ->
  OneToTen = clj_rt:vector(lists:seq(1, 10)),

  ct:comment("Get subvec from 1 to 4, 0-based indexes"),
  TwoToFive = 'clojerl.Vector':subvec(OneToTen, 1, 4),
  [2, 3, 4] = clj_rt:to_list(TwoToFive),

  {comments, ""}.

-spec stack(config()) -> result().
stack(_Config) ->
  EmptyVector = clj_rt:vector([]),
  ?NIL = clj_rt:peek(EmptyVector),
  ok = try EmptyVector = clj_rt:pop(EmptyVector), error
       catch _:_ -> ok
       end,

  OneVector = clj_rt:vector([1]),
  1         = clj_rt:peek(OneVector),
  true      = clj_rt:equiv(clj_rt:pop(OneVector), EmptyVector),

  TwoVector   = clj_rt:vector([1, 2]),
  2         = clj_rt:peek(TwoVector),
  true      = clj_rt:equiv(clj_rt:pop(TwoVector), OneVector),

  {comments, ""}.

-spec reduce(config()) -> result().
reduce(_Config) ->
  PlusFun = fun
              ([]) -> 0;
              ([X, Y]) -> X + Y
            end,
  EmptyVector = clj_rt:vector([]),

  0  = 'clojerl.IReduce':reduce(EmptyVector, PlusFun),
  42 = 'clojerl.IReduce':reduce(EmptyVector, PlusFun, 42),

  TenVector = clj_rt:vector(lists:seq(1, 10)),
  55 = 'clojerl.IReduce':reduce(TenVector, PlusFun),
  60 = 'clojerl.IReduce':reduce(TenVector, PlusFun, 5),

  PlusMaxFun = fun
                 ([]) -> 0;
                 ([X, Y]) when X < 10 -> X + Y;
                 ([X, _]) -> 'clojerl.Reduced':?CONSTRUCTOR(X)
            end,
  10 = 'clojerl.IReduce':reduce(TenVector, PlusMaxFun),

  {comments, ""}.

-spec associative(config()) -> result().
associative(_Config) ->
  EmptyVector = clj_rt:vector([]),

  OneVector  = clj_rt:assoc(EmptyVector, 0, a),
  TwoVector  = clj_rt:assoc(OneVector, 1, b),
  OneVector2 = clj_rt:assoc(TwoVector, 0, c),

  [a]    = clj_rt:to_list(OneVector),
  [a, b] = clj_rt:to_list(TwoVector),
  [c, b] = clj_rt:to_list(OneVector2),

  ok = try clj_rt:assoc(EmptyVector, 2, a), error
       catch error:_ -> ok
       end,

  ok = try clj_rt:assoc(OneVector, 2, a), error
       catch error:_ -> ok
       end,

  true = clj_rt:equiv([0, a], clj_rt:find(TwoVector, 0)),
  true = clj_rt:equiv([1, b], clj_rt:find(TwoVector, 1)),

  ?NIL = clj_rt:find(TwoVector, 3),

  true = clj_rt:'contains?'(OneVector, 0),
  true = clj_rt:'contains?'(TwoVector, 1),

  false = clj_rt:'contains?'(EmptyVector, 1),
  false = clj_rt:'contains?'(OneVector, 1),
  false = clj_rt:'contains?'(TwoVector, 2),

  {comments, ""}.

-spec to_erl(config()) -> result().
to_erl(_Config) ->
  Vector1   = clj_rt:vector([1, 2, 3]),
  {1, 2, 3} = clj_rt:'->erl'(Vector1, false),
  {1, 2, 3} = clj_rt:'->erl'(Vector1, true),

  Vector2        = clj_rt:vector([1, Vector1]),
  {1, Vector1}   = clj_rt:'->erl'(Vector2, false),
  {1, {1, 2, 3}} = clj_rt:'->erl'(Vector2, true),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ?NIL = 'clojerl.Vector':'_'(?NIL),

  NotEmptyVector = clj_rt:vector([a, b, 2, 3]),
  EmptyVector    = clj_rt:empty(NotEmptyVector),
  EmptyVector    = clj_rt:vector([]),

  VectorMeta  = clj_rt:with_meta(clj_rt:vector([1, 2, 3]), #{a => 1}),
  #{a := 1} = clj_rt:meta(VectorMeta),

  a = clj_rt:get(NotEmptyVector, 0),
  b = clj_rt:get(NotEmptyVector, 1),
  2 = clj_rt:get(NotEmptyVector, 2),
  3 = clj_rt:get(NotEmptyVector, 3),
  ?NIL = clj_rt:get(NotEmptyVector, 4),
  bla = clj_rt:get(NotEmptyVector, 43, bla),

  RSeq = 'clojerl.Vector':rseq(NotEmptyVector),
  3    = clj_rt:first(RSeq),
  ?NIL = 'clojerl.Vector':rseq(EmptyVector),

  {comments, ""}.
