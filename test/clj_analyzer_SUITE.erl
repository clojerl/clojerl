-module(clj_analyzer_SUITE).

-export([all/0]).

-export([
         constants/1,
         ns/1,
         def/1,
         quote/1,
         invoke/1,
         symbol/1,
         vector/1,
         map/1,
         set/1
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

-spec ns(config()) -> result().
ns(_Config) ->
  ct:comment("Not a symbol"),
  ok = try analyze_one(<<"(ns 1)">>)
       catch _:_ -> ok
       end,

  ct:comment("Change namespace and analyze keyword"),
  HelloKeyword = clj_core:keyword(<<"bla">>, <<"hello">>),

  [#{op := constant,
     form := HelloKeyword}] = analyze_all(<<"(ns bla) ::hello">>),

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

  ct:comment("Qualified var that doesn't exist"),
  ok = try analyze_one(<<"(def x/y)">>)
       catch _:Reason4 ->
           <<"Can't refer to qualified var that doesn't exist">> = Reason4,
           ok
       end,

  ct:comment("Create def outside current namespace"),
  ok = try analyze_all(<<"(ns bla) (def x 1) (ns user) (def bla/x 2)">>)
       catch _:Reason5 ->
           <<"Can't create defs outside of current ns">> = Reason5,
           ok
       end,

  #{op := def,
    doc := <<"doc string">>} = analyze_one(<<"(def x \"doc string\" 1)">>),

  [_, #{op := def}] = analyze_all(<<"(def x 1) (def y user/x)">>),

  #{op := def} = analyze_one(<<"(def user/x 1)">>),

  [#{op := def}] = analyze_all(<<"(ns bla) (def x 1)">>),

  {comments, ""}.

-spec quote(config()) -> result().
quote(_Config) ->
  ct:comment("Quote with reader macro"),
  #{op := quote,
    expr := #{op := constant}} = analyze_one(<<"'(user/x 1)">>),

  ct:comment("Quote with quote symbol"),
  #{op := quote,
    expr := #{op := constant}} = analyze_one(<<"(quote (user/x 1))">>),

  ct:comment("More than one arg to quote"),
  ok = try analyze_all(<<"(quote 1 2 3)">>)
       catch _:Reason ->
           <<"Wrong number of args to quote, had: 3">> = Reason,
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

-spec vector(config()) -> result().
vector(_Config) ->
  #{op := vector} = analyze_one(<<"[\"hello\" :x 1]">>),

  {comments, ""}.

-spec 'map'(config()) -> result().
map(_Config) ->
  #{op := map} = analyze_one(<<"{:name 1 :lastname 2}">>),

  {comments, ""}.

-spec set(config()) -> result().
set(_Config) ->
  #{op := set} = analyze_one(<<"#{:name :lastname}">>),

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
  Fun = fun(Form, EnvAcc) ->
            clj_analyzer:analyze(EnvAcc, Form)
        end,
  Env = clj_reader:read_fold(Fun, Src),
  clj_env:exprs(Env).
