-module(clj_analyzer_SUITE).

-export([ all/0
        , init_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ constants/1
        , ns/1
        , def/1
        , quote/1
        , fn/1
        , do/1
        , 'if'/1
        , 'case'/1
        , 'let'/1
        , loop/1
        , invoke/1
        , symbol/1
        , vector/1
        , map/1
        , set/1
        , tuple/1
        , throw/1
        , 'try'/1
        , var/1
        , erl_fun/1
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
  clojerl:start(),
  Config.

-spec init_per_testcase(_, config()) -> config().
init_per_testcase(_, Config) ->
  'clojerl.Var':push_bindings(#{}),
  Config.

-spec end_per_testcase(_, config()) -> config().
end_per_testcase(_, Config) ->
  'clojerl.Var':pop_bindings(),
  Config.

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

  ct:comment("Keyword"),
  HelloKeyword = clj_core:keyword(<<"hello">>),
  #{ op   := constant
   , form := HelloKeyword
   } = analyze_one(<<":hello">>),

  {comments, ""}.

-spec ns(config()) -> result().
ns(_Config) ->
  ct:comment("Not a symbol"),
  ok = try analyze_one(<<"(ns 1)">>)
       catch _:_ -> ok
       end,

  ct:comment("Change namespace and analyze keyword"),
  HelloKeyword = clj_core:keyword(<<"clojure.core">>, <<"hello">>),

  [ #{op := invoke}
  , #{op := constant, form := HelloKeyword}
  ] = analyze_all(<<"(ns bla) ::hello">>),

  {comments, ""}.

-spec def(config()) -> result().
def(_Config) ->
  ct:comment("Few arguments"),
  ok = try analyze_one(<<"(def)">>)
       catch _:<<"?:1:1: Too few arguments to def">> ->
           ok
       end,

  ct:comment("Many arguments"),
  ok = try analyze_one(<<"(def x \"doc\" 1 2)">>)
       catch _:<<"?:1:1: Too many arguments to def">> ->
           ok
       end,

  ct:comment("Not a symbol"),
  ok = try analyze_one(<<"(def :x \"doc\" 1)">>)
       catch _:<<"?:1:1: First argument to def must be a symbol">> ->
           ok
       end,

  ct:comment("Qualified var that doesn't exist"),
  ok = try analyze_one(<<"(def x/y)">>)
       catch
         _:<<"?:1:6: Can't refer to qualified var that doesn't exist: x/y">> ->
           ok
       end,

  %% ct:comment("Create def outside current namespace"),
  %% ok = try analyze_all(<<"(ns bla) (def x 1) (ns $user)\n(def bla/x 2)">>)
  %%      catch _:Reason5 ->
  %%          <<"?:2:1: Can't create defs outside of current ns">> = Reason5,
  %%          ok
  %%      end,

  ct:comment("Not a dynamic var but its name suggest otherwise"),
  _ = analyze_one(<<"(def *x* 1)">>),

  #{op := def, name := NameSymbol} = analyze_one(<<"(def x \"doc string\" 1)">>),
  <<"doc string">> = clj_core:get(clj_core:meta(NameSymbol), doc),

  [_, #{op := def}] = analyze_all(<<"(def x 1) (def y clojure.core/x)">>),

  #{op := def} = analyze_one(<<"(def clojure.core/x 1)">>),

  [ #{op := invoke, args := [_]}
  , #{op := def}
  ] = analyze_all(<<"(ns bla) (def x 1)">>),

  ct:comment("Function vars should have fn information in their metadata"),
  #{ op  := def
   , var := Var
   } = analyze_one(<<"(def clojure.core/x (fn* [x] x))">>),

  VarMeta = clj_core:meta(Var),

  #{ 'variadic?'     := false
   , max_fixed_arity := 1
   , variadic_arity  := undefined
   } = VarMeta,

  ct:comment("Vars keep meta from symbol"),
  #{ op := def
   , var := VarWithDynamic
   } = analyze_one(<<"(def ^:dynamic *x* 1)">>),
  #{dynamic := true} = clj_core:meta(VarWithDynamic),

  ct:comment("Vars keep meta from symbol"),
  #{ op  := def
   , var := VarWithDynamicMacro
   } = analyze_one(<<"(def ^{:dynamic true, :macro true} *x* 1)">>),
  #{ dynamic := true
   , macro   := true
   } = clj_core:meta(VarWithDynamicMacro),

  {comments, ""}.

-spec quote(config()) -> result().
quote(_Config) ->
  ct:comment("Quote with reader macro"),
  #{op := quote,
    expr := #{op := constant}} = analyze_one(<<"'(clojure.core/x 1)">>),

  ct:comment("Quote with quote symbol"),
  #{op := quote,
    expr := #{op := constant}} = analyze_one(<<"(quote (clojure.core/x 1))">>),

  ct:comment("More than one arg to quote"),
  ok = try analyze_all(<<"(quote 1 2 3)">>)
       catch _:Reason ->
           <<"?:1:1: Wrong number of args to quote, had: 3">> = Reason,
           ok
       end,

  {comments, ""}.

-spec fn(config()) -> result().
fn(_Config) ->
  ct:comment("unnamed fn with one param and one method"),
  #{ op      := fn
   , methods := [Fn1Method1]
   } = analyze_one(<<"(fn* [x] x)">>),

  #{ op     := fn_method
   , params := [Fn1Param1]
   } = Fn1Method1,

  XSymbol = clj_core:symbol(<<"x">>),
  #{ op   := binding
   , name := XSymbolCheck
   } = Fn1Param1,
  true = clj_core:equiv(XSymbol, XSymbolCheck),

  ct:comment("named fn with one param and one method"),
  #{op      := fn,
    methods := [Fn2Method1],
    local   := #{op := local, name := HelloSymbol}
   } = analyze_one(<<"(fn* hello [x] x)">>),

  true = clj_core:equiv(HelloSymbol, clj_core:symbol(<<"__clj__hello">>)),

  #{ op     := fn_method
   , params := [Fn2Param1]
   } = Fn2Method1,

  #{ op   := binding
   , name := XSymbolCheck2
   } = Fn2Param1,
  true = clj_core:equiv(XSymbol, XSymbolCheck2),

  ct:comment("fn with two params and two methods (non-variadic)"),
  #{ op      := fn
   , methods := [Fn3Method1, Fn3Method2]
   } = analyze_one(<<"(fn* ([x] x) ([x y] x y))">>),

  #{ op     := fn_method
   , params := [Fn3Param1]
   } = Fn3Method1,

  #{ op     := fn_method
   , params := [Fn3Param1b, Fn3Param2]
   } = Fn3Method2,

  %% Check that the params are the same, even though the
  %% locactions are different
  true = clj_core:equiv(Fn3Param1b, Fn3Param1b),

  #{ op   := binding
   , name := XSymbolCheck3
   } = Fn3Param1,
  true = clj_core:equiv(XSymbol, XSymbolCheck3),

  #{ op   := binding
   , name := XSymbolCheck3b
   } = Fn3Param1b,
  true = clj_core:equiv(XSymbol, XSymbolCheck3b),

  YSymbol = clj_core:symbol(<<"y">>),
  #{ op   := binding
   , name := YSymbolCheck
   } = Fn3Param2,
  true = clj_core:equiv(YSymbol, YSymbolCheck),

  ct:comment("fn with two params and two methods (variadic)"),
  #{ op      := fn
   , methods := [Fn4Method1, Fn4Method2]
   } = analyze_one(<<"(fn* ([x] x) ([x & z] x z))">>),

  #{ op     := fn_method
   , params := [Fn4Param1]
   } = Fn4Method1,

  #{ op     := fn_method
   , params := [Fn4Param1b, Fn4Param2]
   } = Fn4Method2,

  true = clj_core:equiv(Fn4Param1, Fn4Param1b),

  #{ op   := binding
   , name := XSymbolCheck4
   } = Fn4Param1,
  true = clj_core:equiv(XSymbol, XSymbolCheck4),

  #{ op   := binding
   , name := XSymbolCheck4b
   } = Fn4Param1b,
  true = clj_core:equiv(XSymbol, XSymbolCheck4b),

  ZSymbol = clj_core:symbol(<<"z">>),
  #{ op          := binding
   , name        := ZSymbolCheck
   , 'variadic?' := true
   } = Fn4Param2,
  true = clj_core:equiv(ZSymbol, ZSymbolCheck),

  ct:comment("fn with two params and one method (variadic)"),
  #{op      := fn,
    methods := [Fn5Method1]
   } = analyze_one(<<"(fn* ([x & z] x z))">>),

  #{op     := fn_method,
    params := [Fn5Param1, Fn5Param2]
   } = Fn5Method1,

  #{ op   := binding
   , name := XSymbolCheck5
   } = Fn5Param1,
  true = clj_core:equiv(XSymbol, XSymbolCheck5),

  ZSymbol = clj_core:symbol(<<"z">>),
  #{ op          := binding
   , name        := ZSymbolCheck2
   , 'variadic?' := true
   } = Fn5Param2,
  true = clj_core:equiv(ZSymbol, ZSymbolCheck2),

  ct:comment("fn with two variadic methods"),
  ok = try analyze_one(<<"(fn* ([a b & _] b) ([x & z] x z))">>), error
       catch _:Reason ->
           <<"?:1:1: Can't have more than 1 variadic overload">> = Reason,
           ok
       end,

  ct:comment("fn with fixed arity with more args than variadic"),
  ok = try analyze_one(<<"(fn* ([a b c] a) ([x & z] x z))">>), error
       catch _:Reason2 ->
           <<"?:1:1: Can't have fixed arity overload "
             "with more params than variadic overload">> = Reason2,
           ok
       end,
  ok = try analyze_one(<<"(fn* ([a b] a) ([x & z] x z))">>), error
       catch _:Reason3 ->
           <<"?:1:1: Can't have fixed arity overload "
             "with more params than variadic overload">> = Reason3,
           ok
       end,

  ct:comment("fn with two methods same arity"),
  ok = try analyze_one(<<"(fn* ([a b] b) ([x y] x y))">>), error
       catch _:Reason4 ->
           <<"?:1:1: Can't have 2 or more overloads "
             "with the same arity">> = Reason4,
           ok
       end,

  ct:comment("binding in fn should not leak out of fn* scope"),
  ok = try analyze_all(<<"(fn* ([zz y] zz y)) zz">>), error
       catch _:<<"?:1:21: Unable to resolve symbol 'zz' in this context">> ->
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
           <<"?:1:1: Wrong number of args to if, had: 0">> = Reason,
           ok
       end,
  ok = try analyze_one(<<"(if true)">>), error
       catch _:Reason2 ->
           <<"?:1:1: Wrong number of args to if, had: 1">> = Reason2,
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

-spec 'case'(config()) -> result().
'case'(_Config) ->
  ct:comment("case with no clauses"),
  #{ op      := 'case'
   , test    := #{op := constant}
   , clauses := []
   , default := undefined
   } = analyze_one(<<"(case* true)">>),

  ct:comment("case with two clauses"),
  #{ op      := 'case'
   , test    := #{op := constant}
   , clauses := [_, _]
   , default := undefined
   } = analyze_one(<<"(case* true true 1 false 0)">>),

  ct:comment("case with two clauses and default"),
  #{ op      := 'case'
   , test    := #{op := constant}
   , clauses := [_, _]
   , default := DefaultExpr
   } = analyze_one(<<"(case* true true 1 false 0 :default)">>),

  true = DefaultExpr =/= undefined,

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
       catch _:<<"?:1:17: Unable to resolve symbol 'z' in this context">> ->
           ok
       end,

  ct:comment("let with no binding vector"),
  ok = try analyze_one(<<"(let*)">>)
       catch _:Reason2 ->
           <<"?:1:1: let* requires a vector for its bindings, "
             "had: :clojerl.Nil">> = Reason2,
           ok
       end,

  ct:comment("let with odd number of forms in binding vector"),
  ok = try analyze_one(<<"(let* [x 2 y])">>)
       catch _:Reason3 ->
           <<"?:1:1: let* requires an even number of "
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

  false = clj_core:equiv(LoopId0, LoopId1),
  false = clj_core:equiv(LoopId1, LoopId2),
  false = clj_core:equiv(LoopId0, LoopId2),

  ct:comment("loop with zero bindings, body and recur"),
  #{ op       := loop
   , bindings := Bindings3
   , loop_id  := _LoopId3
   , body     := _Body3
   } = analyze_one(<<"(loop* [] (recur))">>),
  0 = length(Bindings3),

  ct:comment("loop with two bindings, body and recur"),
  #{ op       := loop
   , bindings := Bindings4
   , loop_id  := _LoopId4
   , body     := _Body4
   } = analyze_one(<<"(loop* [x 1 y 2] (recur 1 2))">>),
  2 = length(Bindings4),

  ct:comment("loop with two bindings and recur mismatch"),
  ok = try analyze_one(<<"(loop* [x 1 y 2] (recur))">>), error
       catch _:_ -> ok end,

  ct:comment("loop with no bindings and recur not in tail position"),
  ok = try analyze_one(<<"(loop* [] (recur) 1)">>), error
       catch _:_ -> ok end,

  {comments, ""}.

-spec invoke(config()) -> result().
invoke(_Config) ->
  ct:comment("Can't call nil"),
  ok = try analyze_one(<<"(nil)">>)
       catch _:Reason ->
           <<"?:1:1: Can't call nil">> = Reason,
           ok
       end,

  ct:comment("Call undefined symbol"),
  ok = try analyze_one(<<"(bla)">>)
       catch _:<<"?:1:2: Unable to resolve symbol 'bla' in this context">> ->
           ok
       end,

  ct:comment("Call defined symbol"),
  HelloSymbol = clj_core:symbol(<<"hello">>),
  ListHello = clj_core:list([HelloSymbol]),
  [ _
  , #{ op   := invoke
     , form := ListHelloCheck
     , f    := #{op := var, form := HelloSymbolCheck}
     }
  ] = analyze_all(<<"(def hello :hello) (hello)">>),
  true = clj_core:equiv(ListHello, ListHelloCheck),
  true = clj_core:equiv(HelloSymbol, HelloSymbolCheck),

  ct:comment("Call something different than a symbol, analyzer shouldn't fail"),
  HelloKeyword = clj_core:keyword(<<"hello">>),
  OneHello = clj_core:list([1, HelloKeyword]),
  #{ op   := invoke
   , form := OneHelloCheck
   , f    := #{op := constant, form := 1}
   } = analyze_one(<<"(1 :hello)">>),
  true = clj_core:equiv(OneHello, OneHelloCheck),

  {comments, ""}.

-spec symbol(config()) -> result().
symbol(_Config) ->
  ct:comment("Unresolved symbol"),
  ok = try analyze_one(<<"hello-world">>), error
       catch _:<<"?:1:1: Unable to resolve symbol "
                 "'hello-world' in this context">> ->
           ok
       end,

  ct:comment("Resolved symbol"),
  HelloSymbol = clj_core:symbol(<<"hello">>),
  [ _
  , #{ op   := var
     , form := HelloSymbolCheck
     }
  ] = analyze_all(<<"(def hello 1) hello">>),
  true = clj_core:equiv(HelloSymbol, HelloSymbolCheck),

  {comments, ""}.

-spec vector(config()) -> result().
vector(_Config) ->
  #{op := vector} = analyze_one(<<"[\"hello\" :x 1]">>),

  #{ op   := 'with-meta'
   , expr := #{op := vector}
   } = analyze_one(<<"^{:meta 'data'} [\"hello\" :x 1]">>),

  {comments, ""}.

-spec 'map'(config()) -> result().
map(_Config) ->
  #{op := map} = analyze_one(<<"{:name 1 :lastname 2}">>),

  #{ op   := 'with-meta'
   , expr := #{op := map}
   } = analyze_one(<<"^{:meta 'data'} {:name 1 :lastname 2}">>),

  {comments, ""}.

-spec set(config()) -> result().
set(_Config) ->
  #{ op    := set
   , items := [_, _]
   } = analyze_one(<<"#{:name :lastname}">>),

  #{ op   := 'with-meta'
   , expr := #{op := set}
   } = analyze_one(<<"^{:meta 'data'} #{:name :lastname}">>),

  {comments, ""}.

-spec tuple(config()) -> result().
tuple(_Config) ->
  #{ op    := tuple
   , items := [_, _]
   } = analyze_one(<<"#[:name :lastname]">>),

  {comments, ""}.


-spec throw(config()) -> result().
throw(_Config) ->
  ct:comment("Throw with a single argument"),
  #{op := throw} = analyze_one(<<"(throw 1)">>),

  ct:comment("Throw with any other amount of arguments fails"),
  ok = try analyze_one(<<"(throw)">>)
       catch _:<<"?:1:1: Wrong number of args to throw, had: 0">> ->
           ok
       end,

  ok = try analyze_one(<<"(throw :a :b)">>)
       catch _:<<"?:1:1: Wrong number of args to throw, had: 2">> ->
           ok
       end,

  ok = try analyze_one(<<"(throw :a :b :c :d)">>)
       catch _:<<"?:1:1: Wrong number of args to throw, had: 4">> ->
           ok
       end,

  {comments, ""}.

-spec 'try'(config()) -> result().
'try'(_Config) ->
  ct:comment("try with no catch and no finally"),
  #{ op      := 'try'
   , body    := #{op := do}
   , catches := []
   , finally := undefined
   } = analyze_one(<<"(try 1)">>),

  ct:comment("try with one catch and no finally"),
  #{ op      := 'try'
   , catches := [Catch1_1]
   , finally := undefined
   } = analyze_one(<<"(try 1 (catch :error err err))">>),

  #{ op    := 'catch'
   , local := #{op := binding, name := ErrName1_1}
   , body  := #{op := do}
   } = Catch1_1,

  true = clj_core:equiv(ErrName1_1, clj_core:symbol(<<"err">>)),

  ct:comment("try with two catches and no finally"),
  #{ op      := 'try'
   , catches := [Catch2_1, Catch2_2]
   , finally := undefined
   } = analyze_one(<<"(try 1"
                     "  (catch :error err-1 err-1)"
                     "  (catch :throw err-2 err-2))">>
                  ),

  #{ op    := 'catch'
   , local := #{op := binding, name := ErrName2_1}
   , class := error
   , body  := #{op := do}
   } = Catch2_1,

  #{ op    := 'catch'
   , local := #{op := binding, name := ErrName2_2}
   , class := throw
   , body  := #{op := do}
   } = Catch2_2,

  true = clj_core:equiv(ErrName2_1, clj_core:symbol(<<"err-1">>)),
  true = clj_core:equiv(ErrName2_2, clj_core:symbol(<<"err-2">>)),

  ct:comment("try, catch and finally"),
  #{ op      := 'try'
   , catches := [Catch3_1]
   , finally := Finally3
   } = analyze_one(<<"(try 1 (catch :error e e) (finally 2))">>),

  #{ op    := 'catch'
   , local := #{op := binding, name := ErrName3_1}
   , class := error
   , body  := #{op := do}
   } = Catch3_1,

  true = clj_core:equiv(ErrName3_1, clj_core:symbol(<<"e">>)),

  #{ op         := do
   , statements := []
   , ret        := #{op := constant, form := 2}
   } = Finally3,

  ct:comment("try, catch with :exit"),
  #{ op      := 'try'
   , catches := [Catch4_1]
   , finally := undefined
   } = analyze_one(<<"(try 1 (catch :exit e e))">>),

  #{ op    := 'catch'
   , local := #{op := binding, name := ErrName4_1}
   , class := exit
   , body  := #{op := do}
   } = Catch4_1,

  true = clj_core:equiv(ErrName4_1, clj_core:symbol(<<"e">>)),

  ct:comment("try, catch with _"),
  #{ op      := 'try'
   , catches := [Catch5_1]
   , finally := undefined
   } = analyze_one(<<"(try 1 (catch _ e e))">>),

  #{ op    := 'catch'
   , local := #{op := binding, name := ErrName5_1}
   , class := UnderscoreSym
   , body  := #{op := do}
   } = Catch5_1,

  true = clj_core:equiv(ErrName5_1, clj_core:symbol(<<"e">>)),
  true = clj_core:equiv(UnderscoreSym, clj_core:symbol(<<"_">>)),

  ct:comment("try, catch with invalid value"),
  ok = try analyze_one(<<"(try 1 (catch bla e e))">>), error
       catch _ -> ok
       end,

  {comments, ""}.

-spec var(config()) -> result().
var(_Config) ->
  ct:comment("Use var with symbol for existing var"),
  [_ , #{op := constant, form := VarX}] = analyze_all(<<"(def x 1) (var x)">>),
  <<"x">> = clj_core:name(VarX),
  'clojerl.Var' = clj_core:type(VarX),

  ct:comment("Use var with symbol for non-existing var"),
  ok = try analyze_all(<<"(var zz)">>), error
       catch _:_ -> ok end,

  {comments, ""}.


-spec erl_fun(config()) -> result().
erl_fun(_Config) ->
  ct:comment("Erlang fun without arity"),
  #{ op       := erl_fun
   , module   := erlang
   , function := is_atom
   , arity    := undefined
   } = analyze_one(<<"erlang/is_atom">>),

  #{ op       := erl_fun
   , module   := erlang
   , function := is_atom
   , arity    := 1
   } = analyze_one(<<"erlang/is_atom.1">>),

  #{ op       := erl_fun
   , module   := io
   , function := 'format.1'
   , arity    := 2
   } = analyze_one(<<"io/format.1.2">>),

  #{ op       := erl_fun
   , module   := erlang
   , function := 'is_atom.'
   , arity    := undefined
   } = analyze_one(<<"erlang/is_atom.">>),

  #{ op       := erl_fun
   , module   := erlang
   , function := 'is_atom.hello'
   , arity    := undefined
   } = analyze_one(<<"erlang/is_atom.hello">>),

  #{ op       := erl_fun
   , module   := erlang
   , function := 'is_atom'
   , arity    := undefined
   } = analyze_one(<<"erlang/is_atom.e">>),

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
