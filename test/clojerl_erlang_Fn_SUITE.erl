-module(clojerl_erlang_Fn_SUITE).

-include("clj_test_utils.hrl").

-export([all/0, init_per_suite/1]).

-export([ apply/1
        , hash/1
        , str/1
        ]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec hash(config()) -> result().
hash(_Config) ->
  ct:comment("Different instances of the same function are different"),
  OkFun1 = fun() -> ok end,
  OkFun2 = fun() -> ok end,

  HashOkFun1 = 'clojerl.IHash':hash(OkFun1),
  HashOkFun2 = 'clojerl.IHash':hash(OkFun2),

  true = HashOkFun1 =/= HashOkFun2,

  {comments, ""}.

-spec apply(config()) -> result().
apply(_Config) ->
  ct:comment("Apply a 0 arity fun with 0 arguments"),
  Fun0 = fun() -> ok end,
  ok = clj_core:apply(Fun0, []),

  ct:comment("Apply a 2 arity fun with 2 arguments"),
  Fun2 = fun(X, Y) -> {X, Y} end,
  {a, b} = clj_core:apply(Fun2, [a, b]),

  ct:comment("Apply a 2 arity fun with 0 arguments"),
  ok = try clj_core:apply(Fun2, []), error
       catch _:_ -> ok end,

  ct:comment("Apply a fun built from a function"),
  FunFunction = fun clojerl_erlang_Fn_SUITE:all/0,
  [_ | _] = clj_core:apply(FunFunction, []),

  ct:comment("Apply a non Clojure fun generated through erl_eval"),
  ok = clj_core:apply(fun() -> ok end, []),

  ct:comment("Invoke a non Clojure named fun generated through erl_eval"),
  ok = clj_core:apply(fun _Ok() -> ok end, []),

  ct:comment("Invoke a Clojure fun"),
  {CljFun, _} = clj_compiler:eval(clj_reader:read(<<"(fn* [] :ok)">>)),
  ok = clj_core:apply(CljFun, []),

  ct:comment("Invoke a Clojure fun with a seq as the argument list"),
  {CljFun2, _} = clj_compiler:eval(clj_reader:read(<<"(fn* [] :ok)">>)),
  ok = clj_core:apply(CljFun2, clj_core:list([])),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Regex = <<"#<.+?/.+?>">>,

  ct:comment("Check the str representation of a fun"),
  Fun0 = fun () -> ok end,
  {match, _} = re:run(clj_core:str(Fun0), Regex),

  {comments, ""}.
