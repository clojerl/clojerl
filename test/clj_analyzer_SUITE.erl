-module(clj_analyzer_SUITE).

-export([all/0]).

-export([
         constants/1,
         invoke/1,
         def/1,
         symbol/1
        ]).

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-type config() :: list().
-type result() :: {comments, string()}.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec constants(config()) -> result().
constants(_Config) ->
  ct:comment("nil"),
  #{op := constant,
    form := undefined} = analyze_one(<<"nil">>),

  ct:comment("Boolean"),
  #{op := constant,
    form := false} = analyze_one(<<"false">>),

  ct:comment("Boolean"),
  #{op := constant,
    form := true} = analyze_one(<<"true">>),

  ct:comment("Float"),
  #{op := constant,
    form := 1.0} = analyze_one(<<"1.0">>),

  ct:comment("Integer"),
  #{op := constant,
    form := 1} = analyze_one(<<"1">>),

  ct:comment("String"),
  #{op := constant,
    form := <<"hello">>} = analyze_one(<<"\"hello\"">>),

  ct:comment("Regex"),
  {ok, Re} = re:compile(<<".*">>),
  #{op := constant,
    form := Re} = analyze_one(<<"#\".*\"">>),

  ct:comment("Keyword"),
  HelloKeyword = clj_core:keyword(<<"hello">>),
  #{op := constant,
    form := HelloKeyword} = analyze_one(<<":hello">>),

  {comments, ""}.

-spec def(config()) -> result().
def(_Config) ->
  ct:comment("Few arguments"),
  ok = try analyze_one(<<"(def)">>)
       catch _:Reason ->
           <<"Too few arguments to def">> = Reason,
           ok
       end,

  ct:comment("Many arguments"),
  ok = try analyze_one(<<"(def x \"doc\" 1 2)">>)
       catch _:Reason2 ->
           <<"Too many arguments to def">> = Reason2,
           ok
       end,

  ct:comment("Not a symbol"),
  ok = try analyze_one(<<"(def :x \"doc\" 1)">>)
       catch _:Reason3 ->
           <<"First argument to def must be a symbol">> = Reason3,
           ok
       end,

  {comments, ""}.

-spec invoke(config()) -> result().
invoke(_Config) ->
  ct:comment("Can't call nil"),
  ok = try analyze_one(<<"(nil)">>)
       catch _:Reason ->
           <<"Can't call nil">> = Reason,
           ok
       end,

  ct:comment("Call undefined symbol"),
  ok = try analyze_one(<<"(bla)">>)
       catch _:Reason2 ->
           <<"Unable to resolve var: bla in this context">> = Reason2,
           ok
       end,

  ct:comment("Call defined symbol"),
  HelloSymbol = clj_core:symbol(<<"hello">>),
  ListHello = clj_core:list([HelloSymbol]),
  [_,
   #{op := invoke,
     form := ListHello,
     f := #{op := var,
            form := HelloSymbol}}] = analyze_all(<<"(def hello :hello) (hello)">>),

  {comments, ""}.

-spec symbol(config()) -> result().
symbol(_Config) ->
  ct:comment("Unresolved symbol"),
  ok = try analyze_one(<<"hello">>)
       catch _:Reason ->
           <<"Unable to resolve var: hello in this context">> = Reason,
           ok
       end,

  ct:comment("Unresolved symbol"),
  HelloSymbol = clj_core:symbol(<<"hello">>),
  [_,
   #{op := var,
     form := HelloSymbol}] = analyze_all(<<"(def hello 1) hello">>),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

analyze_one(Src) ->
  Form = clj_reader:read(Src),
  NewEnv = clj_analyzer:analyze(clj_env:default(), Form),
  {Expr, _} = clj_env:pop_expr(NewEnv),
  Expr.

analyze_all(Src) ->
  Forms = clj_reader:read_all(Src),
  Fun = fun(Form, EnvAcc) ->
            clj_analyzer:analyze(EnvAcc, Form)
        end,
  NewEnv = lists:foldl(Fun, clj_env:default(), Forms),
  clj_env:exprs(NewEnv).
