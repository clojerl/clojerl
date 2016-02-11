-module(clojerl_erlang_Fn_SUITE).

-export([all/0, init_per_suite/1]).

-export([ invoke/1
        , str/1
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

-spec invoke(config()) -> result().
invoke(_Config) ->
  ct:comment("Invoke a 0 arity fun with 0 arguments"),
  Fun0 = fun() -> ok end,
  ok = clj_core:invoke(Fun0, []),

  ct:comment("Invoke a 2 arity fun with 2 arguments"),
  Fun2 = fun(X, Y) -> {X, Y} end,
  {a, b} = clj_core:invoke(Fun2, [a, b]),

  ct:comment("Invoke a 2 arity fun with 0 arguments"),
  ok = try clj_core:invoke(Fun2, []), error
       catch _:_ -> ok end,

  ct:comment("Invoke a fun built from a function"),
  FunFunction = fun clojerl_erlang_Fn_SUITE:all/0,
  [_ | _] = clj_core:invoke(FunFunction, []),

  ct:comment("Invoke a non Clojure fun generated through erl_eval"),
  {ok, Tokens, _} = erl_scan:string("fun() -> ok end."),
  {ok, Forms}     = erl_parse:parse_exprs(Tokens), 
  {value, EvalFun, _} = erl_eval:exprs(Forms, []),
  ok = clj_core:invoke(EvalFun, []),

  ct:comment("Invoke a non Clojure named fun generated through erl_eval"),
  {ok, NamedTokens, _} = erl_scan:string("fun Ok() -> ok end."),
  {ok, NamedForms}     = erl_parse:parse_exprs(NamedTokens), 
  {value, EvalNamedFun, _} = erl_eval:exprs(NamedForms, []),
  ok = clj_core:invoke(EvalNamedFun, []),

  ct:comment("Invoke a Clojure fun"),
  {CljFun, _} = clj_compiler:eval(clj_reader:read(<<"(fn* [] :ok)">>)),
  ok = clj_core:invoke(CljFun, []),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  Regex = <<"#<.+?/.+?>">>,

  ct:comment("Check the str representation of a fun"),
  Fun0 = fun () -> ok end,
  {match, _} = re:run(clj_core:str(Fun0), Regex),

  {comments, ""}.
