-module(clojerl_Subvec_SUITE).

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
        , coll/1
        , lookup/1
        , meta/1
        , stack/1
        , reduce/1
        , reversible/1
        , associative/1
        , to_erl/1
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
  Subvec1 = subvec(1, 3, 0, 3),
  true = clj_rt:equiv([1, 2, 3], Subvec1),

  Subvec2 = subvec(0, 3, 0, 0),
  ?NIL = clj_rt:seq(Subvec2),

  {comments, ""}.

-spec count(config()) -> result().
count(_Config) ->
  Subvec1 = subvec(1, 3, 1, 3),
  2 = clj_rt:count(Subvec1),

  Subvec2 = subvec(1, 3, 0, 0),
  0 = clj_rt:count(Subvec2),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Subvec1 = subvec(0, 3, 1, 4),
  <<"[1 2 3]">> = clj_rt:str(Subvec1),

  Subvec2 = subvec(0, 3, 0, 0),
  <<"[]">> = clj_rt:str(Subvec2),

  {comments, ""}.

-spec is_sequential(config()) -> result().
is_sequential(_Config) ->
  Vector = clj_rt:vector([1, 2, 3]),
  true = clj_rt:'sequential?'(Vector),

  {comments, ""}.

-spec hash(config()) -> result().
hash(_Config) ->
  Subvec1 = subvec(0, 3, 1, 4),
  Subvec2 = subvec(0, 3, 0, 3),

  Subvec3 = subvec([1.0, 2, 3], 0, 3),

  Hash1 = 'clojerl.IHash':hash(Subvec1),
  Hash2 = 'clojerl.IHash':hash(Subvec2),
  Hash3 = 'clojerl.IHash':hash(Subvec3),

  true = Hash1 =/= Hash2,
  true = Hash2 =/= Hash3,

  {comments, ""}.

-spec apply(config()) -> result().
apply(_Config) ->
  ct:comment("Invoke a vector "),
  Subvec1 = subvec([1, b, 3], 1, 3),
  b  = clj_rt:apply(Subvec1, [0]),
  3  = clj_rt:apply(Subvec1, [1]),
  ok = try clj_rt:apply(Subvec1, [2]), error
       catch _:_ -> ok
       end,

  ct:comment("Invoke a vector with two arguments"),
  ok = try clj_rt:apply(Subvec1, [1, 2]), error
       catch _:_ -> ok
       end,

  ct:comment("Invoke a vector with an index that's not an integer"),
  ok = try clj_rt:apply(Subvec1, [a]), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec nth(config()) -> result().
nth(_Config) ->
  Subvec1 = subvec([1, b, 3], 0, 3),

  1 = clj_rt:nth(Subvec1, 0),
  b = clj_rt:nth(Subvec1, 1),
  3 = clj_rt:nth(Subvec1, 2),

  3 = clj_rt:nth(Subvec1, 2, not_found),
  not_found = clj_rt:nth(Subvec1, 42, not_found),

  ok = try clj_rt:nth(Subvec1, 3), error
       catch error:badarg -> ok end,

  {comments, ""}.

-spec seq(config()) -> result().
seq(_Config) ->
  Subvec1 = subvec(1, 4, 1, 4),
  2 = clj_rt:first(Subvec1),
  [3, 4] = clj_rt:to_list(clj_rt:seq(clj_rt:next(Subvec1))),
  [3, 4] = clj_rt:to_list(clj_rt:seq(clj_rt:rest(Subvec1))),

  [2, 3, 4] = clj_rt:to_list(Subvec1),

  Subvec2 = subvec(1, 2, 1, 2),
  2 = clj_rt:first(Subvec2),
  ?NIL = clj_rt:next(Subvec2),
  [] = clj_rt:to_list(clj_rt:rest(Subvec2)),

  [2] = clj_rt:to_list(Subvec2),

  Subvec3 = subvec(0, 4, 0, 0),
  ?NIL = clj_rt:first(Subvec3),
  ?NIL = clj_rt:next(Subvec3),
  [] = clj_rt:rest(Subvec3),

  [] = clj_rt:to_list(Subvec3),

  {comments, ""}.

-spec equiv(config()) -> result().
equiv(_Config) ->
  ct:comment("Subvectors with the same elements are equivalent"),
  Subvec1 = clj_rt:with_meta(subvec(1, 3, 1, 3), #{a => 1}),
  Subvec2 = clj_rt:with_meta(subvec(1, 3, 1, 3), #{b => 2}),

  true  = clj_rt:equiv(Subvec1, Subvec2),

  ct:comment("Subvectors with different elements are not equivalent"),
  Subvec3 = clj_rt:with_meta(subvec(1, 4, 1, 4), #{c => 3}),

  false = clj_rt:equiv(Subvec1, Subvec3),

  ct:comment("A clojerl.Subvec and an erlang.List"),
  true = clj_rt:equiv(Subvec1, [2, 3]),
  false = clj_rt:equiv(Subvec1, [2, 3, a]),

  ct:comment("A clojerl.Subvec and something else"),
  false = clj_rt:equiv(Subvec1, whatever),
  false = clj_rt:equiv(Subvec1, #{}),

  {comments, ""}.

-spec coll(config()) -> result().
coll(_Config) ->
  EmptySubvec = subvec(1, 3, 0, 0),

  ct:comment("Conj an element to an empty subvector"),
  OneSubvec = clj_rt:conj(EmptySubvec, 1),

  1    = clj_rt:count(OneSubvec),
  true = clj_rt:equiv(OneSubvec, [1]),

  ct:comment("Conj an element to a subvector with one element"),
  TwoSubvec = clj_rt:conj(OneSubvec, 2),

  2    = clj_rt:count(TwoSubvec),
  true = clj_rt:equiv(TwoSubvec, [1, 2]),

  ct:comment("Create an empty instance from a non-empty subvector"),
  NotEmptySubvec = subvec([a, b, 2, 3], 0, 4),
  EmptyVector = clj_rt:empty(NotEmptySubvec),
  EmptyVector = clj_rt:vector([]),

  {comments, ""}.

-spec lookup(config()) -> result().
lookup(_Config) ->
  Subvec = subvec([a, b, 2, 3], 0, 4),

  a = clj_rt:get(Subvec, 0),
  b = clj_rt:get(Subvec, 1),
  2 = clj_rt:get(Subvec, 2),
  3 = clj_rt:get(Subvec, 3),
  ?NIL = clj_rt:get(Subvec, 4),
  bla = clj_rt:get(Subvec, 43, bla),

  {comments, ""}.

-spec meta(config()) -> result().
meta(_Config) ->
  ct:comment("Set and retrieve metadata"),
  Meta = #{a => 1},
  Subvec = clj_rt:with_meta(subvec(1, 3, 1, 3), Meta),
  Meta = clj_rt:meta(Subvec),

  {comments, ""}.

-spec stack(config()) -> result().
stack(_Config) ->
  EmptySubvec = subvec(1, 3, 0, 0),
  ?NIL = clj_rt:peek(EmptySubvec),
  ok = try EmptySubvec = clj_rt:pop(EmptySubvec), error
       catch _:_ -> ok
       end,

  OneSubvec = subvec([1], 0, 1),
  1 = clj_rt:peek(OneSubvec),
  true = clj_rt:equiv(clj_rt:pop(OneSubvec), EmptySubvec),

  TwoSubvec = subvec(0, 2, 1, 3),
  2 = clj_rt:peek(TwoSubvec),
  true = clj_rt:equiv(clj_rt:pop(TwoSubvec), OneSubvec),

  {comments, ""}.

-spec reduce(config()) -> result().
reduce(_Config) ->
  PlusFun0    = fun
                  ([]) -> 0;
                  ([X, Y]) -> X + Y
                end,
  PlusFun     = 'clojerl.Fn':?CONSTRUCTOR(PlusFun0),
  EmptySubvec = subvec(1, 3, 0, 0),

  0  = 'clojerl.IReduce':reduce(EmptySubvec, PlusFun),
  42 = 'clojerl.IReduce':reduce(EmptySubvec, PlusFun, 42),

  TenSubvec = subvec(1, 10, 0, 10),
  55 = 'clojerl.IReduce':reduce(TenSubvec, PlusFun),
  60 = 'clojerl.IReduce':reduce(TenSubvec, PlusFun, 5),

  PlusMaxFun = fun
                 (X, Y) when X < 10 -> X + Y;
                 (X, _) -> 'clojerl.Reduced':?CONSTRUCTOR(X)
               end,
  10 = 'clojerl.IReduce':reduce(TenSubvec, PlusMaxFun),

  {comments, ""}.

-spec reversible(config()) -> result().
reversible(_Config) ->
  Subvec = subvec([a, b, 2, 3], 0, 4),

  RSeq = 'clojerl.Subvec':rseq(Subvec),
  3    = clj_rt:first(RSeq),
  ?NIL = 'clojerl.Subvec':rseq(subvec([1, 2, 3], 0, 0)),

  {comments, ""}.

-spec associative(config()) -> result().
associative(_Config) ->
  EmptySubvec = subvec(1, 3, 0, 0),

  OneSubvec = clj_rt:assoc(EmptySubvec, 0, a),
  TwoSubvec = clj_rt:assoc(OneSubvec, 1, b),
  OneSubvec2 = clj_rt:assoc(TwoSubvec, 0, c),

  [a]    = clj_rt:to_list(OneSubvec),
  [a, b] = clj_rt:to_list(TwoSubvec),
  [c, b] = clj_rt:to_list(OneSubvec2),

  ok = try clj_rt:assoc(EmptySubvec, 2, a), error
       catch error:_ -> ok
       end,

  ok = try clj_rt:assoc(OneSubvec, 2, a), error
       catch error:_ -> ok
       end,

  true = clj_rt:equiv([0, a], clj_rt:find(TwoSubvec, 0)),
  true = clj_rt:equiv([1, b], clj_rt:find(TwoSubvec, 1)),

  ?NIL = clj_rt:find(TwoSubvec, 3),

  true = clj_rt:'contains?'(OneSubvec, 0),
  true = clj_rt:'contains?'(TwoSubvec, 1),

  false = clj_rt:'contains?'(EmptySubvec, 1),
  false = clj_rt:'contains?'(OneSubvec, 1),
  false = clj_rt:'contains?'(TwoSubvec, 2),

  {comments, ""}.

-spec to_erl(config()) -> result().
to_erl(_Config) ->
  Subvec1 = subvec(1, 3, 0, 3),
  {1, 2, 3} = clj_rt:'->erl'(Subvec1, false),
  {1, 2, 3} = clj_rt:'->erl'(Subvec1, true),

  Subvec2 = subvec([1, Subvec1], 0, 2),
  {1, Subvec1} = clj_rt:'->erl'(Subvec2, false),
  {1, {1, 2, 3}} = clj_rt:'->erl'(Subvec2, true),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec subvec(integer(), integer(), integer(), integer()) ->
  'clojerl.Subvec':type().
subvec(From, To, Start, End) ->
  subvec(lists:seq(From, To), Start, End).

-spec subvec(list(), integer(), integer()) ->
  'clojerl.Subvec':type().
subvec(List, Start, End) ->
  Vector = clj_rt:vector(List),
  'clojerl.Subvec':?CONSTRUCTOR(Vector, Start, End).
