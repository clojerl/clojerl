-module(clj_analyzer_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

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
        , letfn/1
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
        , import/1
        , new/1
        , deftype/1
        , defprotocol/1
        , extend_type/1
        , dot/1
        , 'receive'/1
        , erlang_binary/1
        , on_load/1
        , macroexpand/1
        ]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

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
    form := ?NIL} = analyze_one(<<"nil">>),

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

  ct:comment("Empty list"),
  #{ op   := constant
   , form := EmptyList
   } = analyze_one(<<"()">>),
  0 = clj_core:count(EmptyList),

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
         _:<<"?:1:1: Can't refer to qualified var that doesn't exist: x/y">> ->
           ok
       end,

  %% ct:comment("Create def outside current namespace"),
  %% ok = try analyze_all(<<"(ns bla) (def x 1) (ns $user)\n(def bla/x 2)">>)
  %%      catch _:Reason5 ->
  %%          <<"?:2:1: Can't create defs outside of current ns">> = Reason5,
  %%          ok
  %%      end,

  ct:comment("Var without a bound root value"),
  #{ op   := def
   , var  := _Var
   , init := #{op := constant, form := ?UNBOUND}
   }  = analyze_one(<<"(def x)">>),

  ct:comment("Not a dynamic var but its name suggest otherwise"),
  _ = analyze_one(<<"(def *x* 1)">>),

  #{ op   := def
   , name := NameSymbol
   } = analyze_one(<<"(def x \"doc string\" 1)">>),
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
   , variadic_arity  := ?NIL
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

  ct:comment("Vars keep the arglists"),
  #{ op  := def
   , var := VarWithArgLists
   } = analyze_one(<<"(def ^{:arglists []} *x* 1)">>),
  #{arglists := EmptyVector} = clj_core:meta(VarWithArgLists),
  0 = clj_core:count(EmptyVector),

  ct:comment("Nested defs"),
  #{ op   := def
   , var  := _
   , init := #{op := def}
   }  = analyze_one(<<"(def x (def y 1))">>),

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
  #{ op      := binding
   , pattern := #{name := XSymbolCheck}
   } = Fn1Param1,
  true = clj_core:equiv(XSymbol, XSymbolCheck),

  ct:comment("named fn with one param and one method"),
  #{op      := fn,
    methods := [Fn2Method1],
    local   := #{ op := local, name := HelloSymbol}
   } = analyze_one(<<"(fn* hello [x] x)">>),

  true = clj_core:equiv(HelloSymbol, clj_core:symbol(<<"hello">>)),

  #{ op     := fn_method
   , params := [Fn2Param1]
   } = Fn2Method1,

  #{ op      := binding
   , pattern := #{name := XSymbolCheck2}
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

  #{ op      := binding
   , pattern := #{name := XSymbolCheck3}
   } = Fn3Param1,
  true = clj_core:equiv(XSymbol, XSymbolCheck3),

  #{ op      := binding
   , pattern := #{name := XSymbolCheck3b}
   } = Fn3Param1b,
  true = clj_core:equiv(XSymbol, XSymbolCheck3b),

  YSymbol = clj_core:symbol(<<"y">>),
  #{ op      := binding
   , pattern := #{name := YSymbolCheck}
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

  true = clj_core:equiv( maps:without([env, pattern], Fn4Param1)
                       , maps:without([env, pattern], Fn4Param1b)
                       ),

  true = clj_core:equiv( maps:remove(env, maps:get(pattern, Fn4Param1))
                       , maps:remove(env, maps:get(pattern, Fn4Param1b))
                       ),

  #{ op      := binding
   , pattern := #{name := XSymbolCheck4}
   } = Fn4Param1,
  true = clj_core:equiv(XSymbol, XSymbolCheck4),

  #{ op      := binding
   , pattern := #{name := XSymbolCheck4b}
   } = Fn4Param1b,
  true = clj_core:equiv(XSymbol, XSymbolCheck4b),

  ZSymbol = clj_core:symbol(<<"z">>),
  #{ op          := binding
   , pattern     := #{name := ZSymbolCheck}
   , 'variadic?' := true
   } = Fn4Param2,
  true = clj_core:equiv(ZSymbol, ZSymbolCheck),

  ct:comment("fn with two params and one method (variadic)"),
  #{op      := fn,
    methods := [Fn5Method1]
   } = analyze_one(<<"(fn* ([x & z] x z))">>),

  #{ op     := fn_method
   , params := [Fn5Param1, Fn5Param2]
   , guard  := #{op := constant, form := true}
   } = Fn5Method1,

  #{ op      := binding
   , pattern := #{name := XSymbolCheck5}
   } = Fn5Param1,
  true = clj_core:equiv(XSymbol, XSymbolCheck5),

  ZSymbol = clj_core:symbol(<<"z">>),
  #{ op          := binding
   , pattern     := #{name := ZSymbolCheck2}
   , 'variadic?' := true
   } = Fn5Param2,
  true = clj_core:equiv(ZSymbol, ZSymbolCheck2),

  ct:comment("fn with guards"),
  #{ op      := fn
   , methods := [Fn6Method1, Fn6Method2]
   } = analyze_one(<<"(fn* ([x] {:when false} x) ([y] {:when true} y))">>),

  #{ op     := fn_method
   , params := [_]
   , guard  := #{op := constant, form := false}
   } = Fn6Method1,

  #{ op     := fn_method
   , params := [_]
   , guard  := #{op := constant, form := true}
   } = Fn6Method2,

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

  %% ct:comment("fn with two methods same arity"),
  %% ok = try analyze_one(<<"(fn* ([a b] b) ([x y] x y))">>), error
  %%      catch _:Reason4 ->
  %%          <<"?:1:1: Can't have 2 or more overloads "
  %%            "with the same arity">> = Reason4,
  %%          ok
  %%      end,

  ct:comment("binding in fn should not leak out of fn* scope"),
  ok = try analyze_all(<<"(fn* ([zz y] zz y)) zz">>), error
       catch _:<<"?:?:?: Unable to resolve symbol 'zz' in this context">> ->
           ok
       end,

  {comments, ""}.

-spec do(config()) -> result().
do(_Config) ->
  ct:comment("do with no expressions"),
  #{op := do,
    statements := [],
    ret := #{op := constant, form := ?NIL}
   } = analyze_one(<<"(do)">>),

  ct:comment("do with 1 expression"),
  #{op := do,
    statements := [],
    ret := KeywordExpr
   } = analyze_one(<<"(do :expr)">>),

  ExprKeyword = clj_core:keyword(<<"expr">>),
  #{ op   := constant
   , form := ExprKeyword
   } = KeywordExpr,

  ct:comment("do with 3 expression"),
  #{op := do,
    statements := [KeywordExpr1, OneExpr],
    ret := RetExpr
   } = analyze_one(<<"(do :expr 1 :ret)">>),

  true = maps:remove(env, KeywordExpr) =:= maps:remove(env, KeywordExpr1),

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
    form := ?NIL} = Else1,

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
   , default := ?NIL
   } = analyze_one(<<"(case* true)">>),

  ct:comment("case with two clauses"),
  #{ op      := 'case'
   , test    := #{op := constant}
   , clauses := [_, _]
   , default := ?NIL
   } = analyze_one(<<"(case* true true 1 false 0)">>),

  ct:comment("case with two clauses and default"),
  #{ op      := 'case'
   , test    := #{op := constant}
   , clauses := [_, _]
   , default := DefaultExpr
   } = analyze_one(<<"(case* true true 1 false 0 :default)">>),

  true = DefaultExpr =/= ?NIL,

  ct:comment("case with guards"),
  #{ op      := 'case'
   , test    := #{op := constant}
   , clauses := [{Pattern1, _}, {Pattern2, _}]
   , default := DefaultExpr2
   } = analyze_one(<<"(case* true "
                     "  true {:when false} 1 "
                     "  x    {:when true} 0 "
                     "  :default)">>),

  #{ op    := constant
   , guard := #{op := constant, form := false}
   } = Pattern1,

  #{ op    := local
   , guard := #{op := constant, form := true}
   } = Pattern2,

  true = DefaultExpr2 =/= ?NIL,

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
   , ret        := #{op := constant, form := ?NIL}
   } = Body0,

  ct:comment("let with bindings and no body"),
  #{op := 'let'
   , bindings := Bindings1
   , body     := Body1
   } = analyze_one(<<"(let* [x 1, y :a])">>),
  2 = length(Bindings1),
  #{ statements := []
   , ret        := #{op := constant, form := ?NIL}
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
       catch _:<<"?:1:1: Unable to resolve symbol 'z' in this context">> ->
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

-spec letfn(config()) -> result().
letfn(_Config) ->
  ct:comment("letfn with zero bindings or body"),
  #{ op   := letfn
   , vars := []
   , fns  := []
   } = analyze_one(<<"(letfn* [])">>),

  ct:comment("letfn with one binding"),
  #{ op   := letfn
   , vars := Vars1
   , fns  := Fns1
   } = analyze_one(<<"(letfn* [a (fn* [] :a)] a)">>),
  1 = length(Vars1),
  1 = length(Fns1),

  ct:comment("letfn with two bindings"),
  #{ op   := letfn
   , vars := Vars2
   , fns  := Fns2
   } = analyze_one(<<"(letfn* [a (fn* [] :a) b (fn* [] :b)] [a b])">>),
  2 = length(Vars2),
  2 = length(Fns2),

  ct:comment("letfn with two mutually recursive fns"),
  #{ op   := letfn
   , vars := Vars3
   , fns  := Fns3
   } = analyze_one(<<"(letfn* [a (fn* [] b) b (fn* [] a)] [a b])">>),
  2 = length(Vars3),
  2 = length(Fns3),

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
       catch _:<<"?:1:1: Unable to resolve symbol 'bla' in this context">> ->
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
       catch _:<<"?:?:?: Unable to resolve symbol "
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

  #{ op   := with_meta
   , expr := #{op := vector}
   } = analyze_one(<<"^{:meta 'data'} [\"hello\" :x 1]">>),

  {comments, ""}.

-spec 'map'(config()) -> result().
map(_Config) ->
  #{op := map} = analyze_one(<<"{:name 1 :lastname 2}">>),

  #{ op   := with_meta
   , expr := #{op := map}
   } = analyze_one(<<"^{:meta 'data'} {:name 1 :lastname 2}">>),

  {comments, ""}.

-spec set(config()) -> result().
set(_Config) ->
  #{ op    := set
   , items := [_, _]
   } = analyze_one(<<"#{:name :lastname}">>),

  #{ op   := with_meta
   , expr := #{op := set}
   } = analyze_one(<<"^{:meta 'data'} #{:name :lastname}">>),

  {comments, ""}.

-spec tuple(config()) -> result().
tuple(_Config) ->
  #{ op    := tuple
   , items := [_, _]
   } = analyze_one(<<"#erl [:name :lastname]">>),

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
   , finally := ?NIL
   } = analyze_one(<<"(try 1)">>),

  ct:comment("try with one catch and no finally"),
  #{ op      := 'try'
   , catches := [Catch1_1]
   , finally := ?NIL
   } = analyze_one(<<"(try 1 (catch :error err err))">>),

  #{ op    := 'catch'
   , local := #{ op      := binding
               , pattern := #{name := ErrName1_1}
               }
   , body  := #{op := do}
   } = Catch1_1,

  true = clj_core:equiv(ErrName1_1, clj_core:symbol(<<"err">>)),

  ct:comment("try with two catches and no finally"),
  #{ op      := 'try'
   , catches := [Catch2_1, Catch2_2]
   , finally := ?NIL
   } = analyze_one(<<"(try 1"
                     "  (catch :error err-1 err-1)"
                     "  (catch :throw err-2 err-2))">>
                  ),

  #{ op    := 'catch'
   , local := #{ op      := binding
               , pattern := #{name := ErrName2_1}
               }
   , class := error
   , body  := #{op := do}
   , guard := #{op := constant, form := true}
   } = Catch2_1,

  #{ op    := 'catch'
   , local := #{ op      := binding
               , pattern := #{name := ErrName2_2}
               }
   , class := throw
   , body  := #{op := do}
   , guard := #{op := constant, form := true}
   } = Catch2_2,

  true = clj_core:equiv(ErrName2_1, clj_core:symbol(<<"err-1">>)),
  true = clj_core:equiv(ErrName2_2, clj_core:symbol(<<"err-2">>)),

  ct:comment("try, catch and finally"),
  #{ op      := 'try'
   , catches := [Catch3_1]
   , finally := Finally3
   } = analyze_one(<<"(try 1 (catch :error e e) (finally 2))">>),

  #{ op    := 'catch'
   , local := #{ op      := binding
               , pattern := #{name := ErrName3_1}
               }
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
   , finally := ?NIL
   } = analyze_one(<<"(try 1 (catch :exit e e))">>),

  #{ op    := 'catch'
   , local := #{ op      := binding
               , pattern := #{name := ErrName4_1}
               }
   , class := exit
   , body  := #{op := do}
   } = Catch4_1,

  true = clj_core:equiv(ErrName4_1, clj_core:symbol(<<"e">>)),

  ct:comment("try, catch with _"),
  #{ op      := 'try'
   , catches := [Catch5_1]
   , finally := ?NIL
   } = analyze_one(<<"(try 1 (catch _ e e))">>),

  #{ op    := 'catch'
   , local := #{ op      := binding
               , pattern := #{name := ErrName5_1}
               }
   , class := UnderscoreSym
   , body  := #{op := do}
   } = Catch5_1,

  true = clj_core:equiv(ErrName5_1, clj_core:symbol(<<"e">>)),
  true = clj_core:equiv(UnderscoreSym, clj_core:symbol(<<"_">>)),

  ct:comment("try, catch with guards"),
  #{ op      := 'try'
   , catches := [Catch6_1]
   , finally := ?NIL
   } = analyze_one(<<"(try 1 (catch _ e {:when false} e))">>),

  #{ op    := 'catch'
   , guard := #{op := constant, form := false}
   } = Catch6_1,

  ct:comment("try, catch with invalid value"),
  ok = try analyze_one(<<"(try 1 (catch bla e e))">>), error
       catch _:_ -> ok
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
   , arity    := ?NIL
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
   , arity    := ?NIL
   } = analyze_one(<<"erlang/is_atom.">>),

  #{ op       := erl_fun
   , module   := erlang
   , function := 'is_atom.hello'
   , arity    := ?NIL
   } = analyze_one(<<"erlang/is_atom.hello">>),

  #{ op       := erl_fun
   , module   := erlang
   , function := 'is_atom'
   , arity    := ?NIL
   } = analyze_one(<<"erlang/is_atom.e">>),

  {comments, ""}.

-spec import(config()) -> result().
import(_Config) ->
  ct:comment("import* clojerl.String"),
  #{ op       := import
   , typename := <<"clojerl.String">>
   } = analyze_one(<<"(import* \"clojerl.String\")">>),

  ct:comment("Don't provide a binary to import*"),
  ok = try analyze_one(<<"(import* clojerl.String)">>), error
       catch _:_ -> ok end,

  {comments, ""}.

-spec new(config()) -> result().
new(_Config) ->
  ct:comment("Use new with no args"),
  #{ op   := new
   , type := #{op := type, type := StringSymbol}
   , args := []
   } = analyze_one(<<"(new clojerl.String)">>),
  true                 = clj_core:'symbol?'(StringSymbol),
  <<"clojerl.String">> = clj_core:str(StringSymbol),

  ct:comment("Use new with 1 arg"),
  #{ op   := new
   , type := #{op := type, type := StringSymbol}
   , args := [#{op := constant, form := <<"hello">>}]
   } = analyze_one(<<"(new clojerl.String \"hello\")">>),
  true                 = clj_core:'symbol?'(StringSymbol),
  <<"clojerl.String">> = clj_core:str(StringSymbol),

  {comments, ""}.

-spec deftype(config()) -> result().
deftype(_Config) ->
  ct:comment("Simple deftype*"),
  #{ op        := deftype
   , name      := NameSymbol
   , type      := TypeSymbol
   , fields    := [_, _]
   , protocols := []
   , methods   := []
   } = analyze_one(<<"(deftype* MyType ns.MyType [a b] :implements [])">>),
  true            = clj_core:'symbol?'(NameSymbol),
  <<"MyType">>    = clj_core:str(NameSymbol),
  true            = clj_core:'symbol?'(TypeSymbol),
  <<"ns.MyType">> = clj_core:str(TypeSymbol),

  ct:comment("deftype* with an interface and a method"),
  #{ op        := deftype
   , name      := NameSymbol
   , type      := TypeSymbol
   , fields    := [_, _, _]
   , protocols := [_]
   , methods   := [#{op := fn_method}]
   } = analyze_one(<<"(deftype* MyType ns.MyType [a b c] "
                     " :implements [clojerl.String]"
                     " (str [x] nil)"
                     ")">>),

  {comments, ""}.

-spec defprotocol(config()) -> result().
defprotocol(_Config) ->
  ct:comment("Simple defprotocol*"),
  #{ op           := defprotocol
   , name         := NameSymbol
   , methods_sigs := EmptyMethodsSigs
   } = analyze_one(<<"(defprotocol* some-ns.MyProtocol)">>),
  true = clj_core:'symbol?'(NameSymbol),
  <<"some-ns.MyProtocol">> = clj_core:str(NameSymbol),
  0 = clj_core:count(EmptyMethodsSigs),

  ct:comment("defprotocol* with signatures"),
  #{ op           := defprotocol
   , name         := NameSymbol
   , methods_sigs := MethodsSigs
   } = analyze_one(<<"(defprotocol* some-ns.MyProtocol"
                     "  [f1 1] [f2 4] [f2 2])">>),
  3 = clj_core:count(MethodsSigs),


  {comments, ""}.

-spec extend_type(config()) -> result().
extend_type(_Config) ->
  ct:comment("No protocols"),
  #{ op    := extend_type
   , type  := #{op := type, type := TypeSym}
   , impls := #{}
   } = analyze_one(<<"(extend-type* clojerl.String)">>),
  true = clj_core:'symbol?'(TypeSym),
  <<"clojerl.String">> = clj_core:str(TypeSym),

  ct:comment("Extend one protocol"),
  #{ op    := extend_type
   , type  := #{op := type, type := TypeSym}
   , impls := Impls
   } = analyze_one(<<"(extend-type* clojerl.String "
                     "  clojerl.Stringable "
                     "  (str [this] :string))">>
                  ),

  [Stringable] = maps:keys(Impls),
  #{ op   := type
   , type := StringableSym
   } = Stringable,
  true = clj_core:'symbol?'(StringableSym),
  <<"clojerl.Stringable">> = clj_core:str(StringableSym),

  [#{op := fn_method, name := StrSym}] = maps:get(Stringable, Impls),
  true = clj_core:'symbol?'(StrSym),
  <<"str">> = clj_core:str(StrSym),

  ct:comment("Extend two protocols"),
  #{ op    := extend_type
   , type  := #{op := type, type := TypeSym}
   , impls := Impls2
   } = analyze_one(<<"(extend-type* clojerl.String "
                     "  clojerl.Stringable "
                     "  (str [this] :string) "
                     "  clojerl.IMeta "
                     "  (meta [this] :meta) "
                     "  (with_meta [this meta] :with-meta))">>
                  ),

  NotStringable = fun(#{type := T}) -> not clj_core:equiv(T, StringableSym) end,
  [IMeta] = lists:filter(NotStringable, maps:keys(Impls2)),
  #{ op   := type
   , type := IMetaSym
   } = IMeta,
  true = clj_core:'symbol?'(IMetaSym),
  <<"clojerl.IMeta">> = clj_core:str(IMetaSym),

  [_, _] = maps:get(IMeta, Impls2),

  ct:comment("Use non-existing type"),
  ok = try analyze_one(<<"(extend-type* foo.Bar)">>), error
       catch _:_ -> ok
       end,

  ct:comment("Use non-existing protocol"),
  ok = try
         analyze_one(<<"(extend-type* clojerl.String"
                       "  proto.Foo "
                       "  (foo [this] :foo))">>
                    ),
         error
       catch
         _:_ -> ok
       end,

  {comments, ""}.

-spec dot(config()) -> result().
dot(_Config) ->
  ct:comment("Simple function call"),
  #{ op   := invoke
   } = analyze_one(<<"(. clojerl.String foo)">>),

  #{ op   := invoke
   } = analyze_one(<<"(. clojerl.String (foo))">>),

  #{ op   := invoke
   } = analyze_one(<<"(. clojerl.String foo 1 2)">>),

  #{ op   := 'let'
   , body := #{ op := do, ret := #{op := invoke}}
   } = analyze_one(<<"(let* [x 1] (. x foo))">>),

  ct:comment("Require at least 3 forms"),
  ok = try analyze_one(<<"(. a)">>), error
       catch _:Message ->
           {match, _} = re:run(Message, "Malformed member expression"),
           ok
       end,

  ct:comment("Require only 3 args when the third is a list"),
  ok = try analyze_one(<<"(. clojerl.String (foo) bar)">>), error
       catch _:Message2 ->
           {match, _} = re:run(Message2, "expected single list"),
           ok
       end,

  {comments, ""}.

-spec 'receive'(config()) -> result().
'receive'(_Config) ->
  ct:comment("receive with no clauses"),
  #{ op      := 'receive'
   , clauses := []
   , 'after' := #{ op      := 'after'
                 , timeout := #{op := constant, form := 0}
                 }
   } = analyze_one(<<"(receive* (after 0 :bye))">>),

  ct:comment("receive with no after and 1 clause"),
  #{ op      := 'receive'
   , clauses := [_]
   , 'after' := ?NIL
   } = analyze_one(<<"(receive* 1 :one)">>),

  ct:comment("receive with no after and 2 clauses"),
  #{ op      := 'receive'
   , clauses := [_, _]
   , 'after' := ?NIL
   } = analyze_one(<<"(receive* 1 :one 2 :two)">>),

  ct:comment("receive with 2 clauses and after"),
  #{ op      := 'receive'
   , clauses := [_, _]
   , 'after' := #{ op      := 'after'
                 , timeout := #{op := constant, form := 0}
                 }
   } = analyze_one(<<"(receive* 1 :one 2 :two (after 0 1))">>),

  ct:comment("receive with guards"),
  #{ op      := 'receive'
   , clauses := [{Pattern1, _}, _]
   , 'after' := ?NIL
   } = analyze_one(<<"(receive* 1 {:when false} :one 2 :two)">>),

  #{ op    := constant
   , guard := #{op := constant, form := false}
   } = Pattern1,

  ct:comment("No forms allowed after 'after'"),
  ok = try analyze_one(<<"(receive* (after 0 1) 1)">>), error
       catch _:Message ->
           {match, _} = re:run(Message, "Only one after"),
           ok
       end,

  ct:comment("Uneven number of forms for clauses"),
  ok = try analyze_one(<<"(receive* 1 :one 2 (after 0 1))">>), error
       catch _:Message2 ->
           {match, _} = re:run(Message2, "Expected an even number"),
           ok
       end,

  {comments, ""}.

-spec erlang_binary(config()) -> result().
erlang_binary(_Config) ->
  ct:comment("Empty erl-binary*"),
  #{ op       := erl_binary
   , segments := []
   } = analyze_one(<<"(erl-binary*)">>),

  ct:comment("Single integer erl-binary*"),
  #{ op       := erl_binary
   , segments := [IntSegmentExpr]
   } = analyze_one(<<"(erl-binary* 64)">>),

  #{ op    := binary_segment
   , value := #{op := constant}
   } = IntSegmentExpr,

  ct:comment("Single float erl-binary*"),
  #{ op       := erl_binary
   , segments := [FloatSegmentExpr]
   } = analyze_one(<<"(erl-binary* [1.0 :type :float])">>),

  #{ op    := binary_segment
   , value := #{op := constant}
   } = FloatSegmentExpr,

  ct:comment("Single utf8 erl-binary*"),
  #{ op       := erl_binary
   , segments := [Utf8SegmentExpr]
   } = analyze_one(<<"(erl-binary* [64 :type :utf8])">>),

  #{ op    := binary_segment
   , value := #{op := constant}
   } = Utf8SegmentExpr,

  ct:comment("Single binary erl-binary*"),
  #{ op       := erl_binary
   , segments := [BinSegmentExpr0]
   } = analyze_one(<<"(erl-binary* \"hello\")">>),

  #{ op    := binary_segment
   , value := #{op := constant}
   } = BinSegmentExpr0,

  #{ op       := erl_binary
   , segments := [BinSegmentExpr1]
   } = analyze_one(<<"(erl-binary* [\"hello\"])">>),

  #{ op    := binary_segment
   , value := #{op := constant}
   } = BinSegmentExpr1,

  ct:comment("Single symbol erl-binary*"),
  #{ op       := 'let'
   , body     := #{ op  := do
                  , ret := BinaryExpr
                  }
   } = analyze_one(<<"(let* [a \"hello\"] (erl-binary* a))">>),

  #{ op       := erl_binary
   , segments := [SymSegmentExpr0]
   } = BinaryExpr,

  #{ op    := binary_segment
   , value := #{op := local}
   } = SymSegmentExpr0,

  ct:comment("Single binary with flags erl-binary*"),
  #{ op       := erl_binary
   , segments := [FlagsSegmentExpr0]
   } = analyze_one(<<"(erl-binary* [\"hello\" :flags [:signed]])">>),

  #{ op    := binary_segment
   , value := #{op := constant}
   } = FlagsSegmentExpr0,

  #{ op       := erl_binary
   , segments := [FlagsSegmentExpr1]
   } = analyze_one(<<"(erl-binary* [\"hello\" :flags [:little]])">>),

  #{ op    := binary_segment
   , value := #{op := constant}
   } = FlagsSegmentExpr1,

  ok = try analyze_one(<<"(erl-binary* :hello)">>), error
       catch _:_ -> ok
       end,

  ok = try analyze_one(<<"(erl-binary* {})">>), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec on_load(config()) -> result().
on_load(_Config) ->
  ct:comment("erl-on-load*"),
  #{ op   := on_load
   , body := #{op := do, ret := #{op := constant, form := 1}}
   } = analyze_one(<<"(erl-on-load* 1)">>),

  {comments, ""}.

-spec macroexpand(config()) -> result().
macroexpand(_Config) ->
  ct:comment("dot syntax"),

  List1          = clj_reader:read(<<"(.foo bar 1)">>),
  ExpandedCheck1 = clj_reader:read(<<"(. bar foo 1)">>),
  Expanded1      = clj_analyzer:macroexpand_1(List1, clj_env:default()),
  true           = clj_core:equiv(Expanded1, ExpandedCheck1),

  List2          = clj_reader:read(<<"(Bar. :one \"two\")">>),
  ExpandedCheck2 = clj_reader:read(<<"(new Bar :one \"two\")">>),
  Expanded2      = clj_analyzer:macroexpand_1(List2, clj_env:default()),
  true           = clj_core:equiv(Expanded2, ExpandedCheck2),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

analyze_one(Src) ->
  Form = clj_reader:read(Src),
  NewEnv = clj_analyzer:analyze(Form, clj_env:default()),
  {Expr, _} = clj_env:pop_expr(NewEnv),
  Expr.

analyze_all(Src) ->
  EnvDefault = clj_env:default(),
  Env = clj_reader:read_fold(fun clj_analyzer:analyze/2, Src, #{}, EnvDefault),
  clj_env:exprs(Env).
