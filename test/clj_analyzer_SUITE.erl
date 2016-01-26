-module(clj_analyzer_SUITE).

-export([all/0]).

-export([
         constants/1,
         ns/1,
         def/1,
         quote/1,
         fn/1,
         do/1,
         'if'/1,
         'let'/1,
         loop/1,
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

-spec fn(config()) -> result().
fn(_Config) ->
  ct:comment("unnamed fn with one param and one method"),
  #{op := fn,
    methods := [Fn1Method1]
   } = analyze_one(<<"(fn* [x] x)">>),

  #{op := fn_method,
    params := [Fn1Param1]
   } = Fn1Method1,

  XSymbol = clj_core:symbol(<<"x">>),
  #{op := binding,
    name := XSymbol
   } = Fn1Param1,

  ct:comment("named fn with one param and one method"),
  HelloSymbol = clj_core:symbol(<<"hello">>),
  #{op := fn,
    methods := [Fn2Method1],
    local := #{op := binding, name := HelloSymbol}
   } = analyze_one(<<"(fn* hello [x] x)">>),

  #{op := fn_method,
    params := [Fn2Param1]
   } = Fn2Method1,

  #{op := binding,
    name := XSymbol
   } = Fn2Param1,

  ct:comment("fn with two params and two methods (non-variadic)"),
  #{op := fn,
    methods := [Fn3Method1, Fn3Method2]
   } = analyze_one(<<"(fn* ([x] x) ([x y] x y))">>),

  #{op := fn_method,
    params := [Fn3Param1]
   } = Fn3Method1,

  #{op := fn_method,
    params := [Fn3Param1, Fn3Param2]
   } = Fn3Method2,

  #{op := binding,
    name := XSymbol
   } = Fn3Param1,

  YSymbol = clj_core:symbol(<<"y">>),
  #{op := binding,
    name := YSymbol
   } = Fn3Param2,

  ct:comment("fn with two params and two methods (variadic)"),
  #{op := fn,
    methods := [Fn4Method1, Fn4Method2]
   } = analyze_one(<<"(fn* ([x] x) ([x & z] x z))">>),

  #{op := fn_method,
    params := [Fn4Param1]
   } = Fn4Method1,

  #{op := fn_method,
    params := [Fn4Param1, Fn4Param2]
   } = Fn4Method2,

  #{op := binding,
    name := XSymbol
   } = Fn4Param1,

  ZSymbol = clj_core:symbol(<<"z">>),
  #{op := binding,
    name := ZSymbol,
    'variadic?' := true
   } = Fn4Param2,

  ct:comment("fn with two params and one method (variadic)"),
  #{op := fn,
    methods := [Fn5Method1]
   } = analyze_one(<<"(fn* ([x & z] x z))">>),

  #{op := fn_method,
    params := [Fn5Param1, Fn5Param2]
   } = Fn5Method1,

  #{op := binding,
    name := XSymbol
   } = Fn5Param1,

  ZSymbol = clj_core:symbol(<<"z">>),
  #{op := binding,
    name := ZSymbol,
    'variadic?' := true
   } = Fn5Param2,

  ct:comment("fn with two variadic methods"),
  ok = try analyze_one(<<"(fn* ([a b & _] b) ([x & z] x z))">>), error
       catch _:Reason ->
           <<"Can't have more than 1 variadic overload">> = Reason,
           ok
       end,

  ct:comment("fn with fixed arity with more args than variadic"),
  ok = try analyze_one(<<"(fn* ([a b c] a) ([x & z] x z))">>), error
       catch _:Reason2 ->
           <<"Can't have fixed arity overload "
             "with more params than variadic overload">> = Reason2,
           ok
       end,
  ok = try analyze_one(<<"(fn* ([a b] a) ([x & z] x z))">>), error
       catch _:Reason3 ->
           <<"Can't have fixed arity overload "
             "with more params than variadic overload">> = Reason3,
           ok
       end,

  ct:comment("fn with two methods same arity"),
  ok = try analyze_one(<<"(fn* ([a b] b) ([x y] x y))">>), error
       catch _:Reason4 ->
           <<"Can't have 2 or more overloads "
             "with the same arity">> = Reason4,
           ok
       end,

  ct:comment("binding in fn should not leak out of fn* scope"),
  ok = try analyze_all(<<"(fn* ([x y] x y)) x">>), error
       catch _:Reason5 ->
           <<"Unable to resolve var: x in this context">> = Reason5,
           ok
       end,

  {comments, ""}.

-spec do(config()) -> result().
do(_Config) ->
  ct:comment("do with no expressions"),
  #{op := do,
    statements := [],
    ret := #{op := constant, form := undefined}
   } = analyze_one(<<"(do)">>),

  ct:comment("do with 1 expression"),
  #{op := do,
    statements := [],
    ret := KeywordExpr
   } = analyze_one(<<"(do :expr)">>),

  ExprKeyword = clj_core:keyword(<<"expr">>),
  #{op := constant,
   form := ExprKeyword
   } = KeywordExpr,

  ct:comment("do with 3 expression"),
  #{op := do,
    statements := [KeywordExpr, OneExpr],
    ret := RetExpr
   } = analyze_one(<<"(do :expr 1 :ret)">>),

  #{op := constant,
   form := 1
   } = OneExpr,

  RetKeyword = clj_core:keyword(<<"ret">>),
  #{op := constant,
   form := RetKeyword
   } = RetExpr,

  {comments, ""}.

-spec 'if'(config()) -> result().
'if'(_Config) ->
  ct:comment("if with no args"),
  ok = try analyze_one(<<"(if)">>), error
       catch _:Reason ->
           <<"Wrong number of args to if, had: 0">> = Reason,
           ok
       end,
  ok = try analyze_one(<<"(if true)">>), error
       catch _:Reason2 ->
           <<"Wrong number of args to if, had: 1">> = Reason2,
           ok
       end,

  ct:comment("if with then"),
  #{op := 'if',
    test := Test1,
    then := Then1,
    else := Else1
   } = analyze_one(<<"(if true :then)">>),

  #{op := constant,
    form := true} = Test1,

  ThenKeyword = clj_core:keyword(<<"then">>),
  #{op := constant,
    form := ThenKeyword} = Then1,

  #{op := constant,
    form := undefined} = Else1,

  ct:comment("if with then & else"),
  #{op := 'if',
    test := Test2,
    then := Then2,
    else := Else2
   } = analyze_one(<<"(if true :then :else)">>),

  #{op := constant,
    form := true} = Test2,

  #{op := constant,
    form := ThenKeyword} = Then2,

  ElseKeyword = clj_core:keyword(<<"else">>),
  #{op := constant,
    form := ElseKeyword} = Else2,

  {comments, ""}.

-spec 'let'(config()) -> result().
'let'(_Config) ->
  ct:comment("let with zero bindings or body"),
  #{ op       := 'let'
   , bindings := Bindings0
   , body     := Body0
   } = analyze_one(<<"(let* [])">>),
  0 = length(Bindings0),
  #{ statements := []
   , ret        := #{op := constant, form := undefined}
   } = Body0,

  ct:comment("let with bindings and no body"),
  #{op := 'let'
   , bindings := Bindings1
   , body     := Body1
   } = analyze_one(<<"(let* [x 1, y :a])">>),
  2 = length(Bindings1),
  #{ statements := []
   , ret        := #{op := constant, form := undefined}
   } = Body1,

  ct:comment("let with bindings and body should resolve locals"),
  #{ op := 'let'
   , bindings := Bindings2
   , body     := Body2
   } = analyze_one(<<"(let* [x 1, y :a] x y)">>),
  2 = length(Bindings2),
  #{ statements := [_]
   , ret        := #{op := ReturnExprOp2}
   } = Body2,
  local = ReturnExprOp2,

  ct:comment("let with bindings and a single return expression body"),
  #{ op := 'let'
   , bindings := Bindings3
   , body     := Body3
   } = analyze_one(<<"(let* [x 1, _ :a] x)">>),
  2 = length(Bindings3),
  #{ statements := []
   , ret        := #{op := ReturnExprOp3}
   } = Body3,
  local = ReturnExprOp3,

  ct:comment("let with bindings shuold throw unresolved for z symbol"),
  ok = try analyze_one(<<"(let* [x 1 y 2] z)">>)
       catch _:Reason ->
           <<"Unable to resolve var: z in this context">> = Reason,
           ok
       end,

  ct:comment("let with odd number of forms in binding vector"),
  ok = try analyze_one(<<"(let*)">>)
       catch _:Reason2 ->
           <<"let* requires a vector for its bindings, "
             "had: :clojerl.Nil">> = Reason2,
           ok
       end,

  ct:comment("let with no binding vector"),
  ok = try analyze_one(<<"(let* [x 2 y])">>)
       catch _:Reason3 ->
           <<"let* requires an even number of "
             "forms in binding vector, had: 3">> = Reason3,
           ok
       end,

  {comments, ""}.

-spec loop(config()) -> result().
loop(_Config) ->
  ct:comment("loop with zero bindings or body"),
  #{ op       := loop
   , bindings := Bindings0
   , loop_id  := LoopId0
   } = analyze_one(<<"(loop* [])">>),
  0 = length(Bindings0),

  ct:comment("loop with one binding"),
  #{ op       := loop
   , bindings := Bindings1
   , loop_id  := LoopId1
   } = analyze_one(<<"(loop* [x 1])">>),
  1 = length(Bindings1),

  ct:comment("loop with bindings and body"),
  #{ op       := loop
   , bindings := Bindings2
   , loop_id  := LoopId2
   } = analyze_one(<<"(loop* [x 1 y :a] y)">>),
  2 = length(Bindings2),

  true = LoopId0 =/= LoopId1,
  true = LoopId1 =/= LoopId2,
  true = LoopId0 =/= LoopId2,

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
  [ _
  , #{ op   := invoke
     , form := ListHello
     , f    := #{op := var, form := HelloSymbol}
     }
  ] = analyze_all(<<"(def hello :hello) (hello)">>),

  ct:comment("Call something different than a symbol, analyzer shouldn't fail"),
  HelloKeyword = clj_core:keyword(<<"hello">>),
  OneHello = clj_core:list([1, HelloKeyword]),
  #{ op   := invoke
   , form := OneHello
   , f    := #{op := constant, form := 1}
   } = analyze_one(<<"(1 :hello)">>),

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
  Env = clj_reader:read_fold(Fun, Src, #{}),
  clj_env:exprs(Env).
