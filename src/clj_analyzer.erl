-module(clj_analyzer).

-export([
         analyze/2,
         macroexpand/2,
         macroexpand_1/2,
         is_special/1
        ]).

-define(DEBUG(X), undefined).

-spec analyze(clj_env:env(), any()) -> clj_env:env().
analyze(Env0, Form) ->
  case clj_env:pop_expr(analyze_form(Env0, Form)) of
    {undefined, Env} -> Env;
    {Expr, Env} -> clj_env:push_expr(Env, Expr#{top_level => true})
  end.

-spec is_special('clojerl.Symbol':type()) -> boolean().
is_special(S) ->
  clj_core:'symbol?'(S) andalso
    maps:is_key(clj_core:str(S), special_forms()).

-spec macroexpand_1(clj_env:env(), 'clojerl.List':type()) -> any().
macroexpand_1(Env, Form) ->
  Op       = clj_core:first(Form),
  MacroVar = case clj_core:'symbol?'(Op) of
               true -> lookup_var(Op, false);
               false -> undefined
             end,

  case
    not is_special(Op)
    andalso (MacroVar =/= undefined)
    andalso ('clojerl.Var':is_macro(MacroVar))
  of
    true ->
      Args = [Form, Env] ++ clj_core:seq2(clj_core:rest(Form)),
      try
        Module   = 'clojerl.Var':module(MacroVar),
        Function = 'clojerl.Var':function(MacroVar),
        SeqFun   = fun clj_core:seq/1,
        Args1    = 'clojerl.Var':process_args(MacroVar, Args, SeqFun),
        Arity    = length(Args1),
        FakeFun  = clj_module:fake_fun(Module, Function, Arity),

        clj_core:invoke(FakeFun, Args1)
      catch
        throw:{notfound, _} ->
          clj_core:invoke(MacroVar, Args)
      end;
    false -> Form
  end.

-spec macroexpand(clj_env:env(), 'clojerl.List':type()) ->
  any().
macroexpand(Env, Form) ->
  ExpandedForm = macroexpand_1(Env, Form),
  case clj_core:equiv(ExpandedForm, Form) of
    true -> {Form, Env};
    false -> macroexpand_1(Env, ExpandedForm)
  end.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec special_forms() -> #{'clojerl.Symbol':type() => fun() | undefined}.
special_forms() ->
  #{ <<"def">>        => fun parse_def/2
   , <<"quote">>      => fun parse_quote/2
   , <<"fn*">>        => fun parse_fn/2
   , <<"do">>         => fun parse_do/2
   , <<"if">>         => fun parse_if/2
   , <<"let*">>       => fun parse_let/2
   , <<"loop*">>      => fun parse_loop/2
   , <<"recur">>      => fun parse_recur/2
   , <<"throw">>      => fun parse_throw/2
   , <<"try">>        => fun parse_try/2
   , <<"catch">>      => undefined
   , <<"finally">>    => undefined
   , <<"var">>        => fun parse_var/2

   , <<"letfn*">>     => undefined
   , <<"case*">>      => undefined
   , <<"deftype*">>   => undefined
   , <<"defrecord*">> => undefined

     %% , <<"monitor-enter">>
     %% , <<"monitor-exit">>
     %% , <<"new">>
     %% , <<"&">>
   }.

-spec analyze_forms(clj_env:env(), [any()]) -> clj_env:env().
analyze_forms(Env, Forms) ->
  AnalyzeFun = fun(Form, EnvAcc) -> analyze_form(EnvAcc, Form) end,
  lists:foldl(AnalyzeFun, Env, Forms).

-spec analyze_form(clj_env:env(), any()) -> clj_env:env().
analyze_form(Env, Form) ->
  IsSeq = clj_core:'seq?'(Form),
  case clj_core:type(Form) of
    _ when IsSeq ->
      case clj_core:'empty?'(Form) of
        true  -> analyze_const(Env, Form);
        false -> analyze_seq(Env, clj_core:first(Form), Form)
      end;
    'clojerl.Symbol' ->
      analyze_symbol(Env, Form);
    'clojerl.Vector' ->
      analyze_vector(Env, Form);
    'clojerl.Map' ->
      analyze_map(Env, Form);
    'clojerl.Set' ->
      analyze_set(Env, Form);
    _ ->
      analyze_const(Env, Form)
  end.

%%------------------------------------------------------------------------------
%% Analyze const
%%------------------------------------------------------------------------------

-spec analyze_const(clj_env:env(), any()) -> clj_env:env().
analyze_const(Env, Constant) ->
  Expr = #{ op   => constant
          , env  => ?DEBUG(Env)
          , tag  => clj_core:type(Constant)
          , form => Constant
          },
  clj_env:push_expr(Env, Expr).

%%------------------------------------------------------------------------------
%% Analyze seq
%%------------------------------------------------------------------------------

-spec analyze_seq(clj_env:env(), any(), 'clojerl.List':type()) -> clj_env:env().
analyze_seq(_Env, undefined, List) ->
  clj_utils:throw(<<"Can't call nil">>, clj_reader:location_meta(List));
analyze_seq(Env, Op, List) ->
  IsSymbol     = clj_core:'symbol?'(Op),
  ExpandedList = macroexpand_1(Env, List),
  case clj_core:equiv(List, ExpandedList) of
    true ->
      OpKey = case IsSymbol of
                true  -> clj_core:name(Op);
                false -> Op
              end,
      case maps:get(OpKey, special_forms(), undefined) of
        ParseFun when IsSymbol, ParseFun =/= undefined ->
          ParseFun(Env, List);
        undefined ->
          analyze_invoke(Env, List)
      end;
    false ->
      analyze_form(Env, ExpandedList)
  end.

%%------------------------------------------------------------------------------
%% Parse quote
%%------------------------------------------------------------------------------

-spec parse_quote(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_quote(Env, List) ->
  Count = clj_core:count(List),
  clj_utils:throw_when( Count =/= 2
                      , [<<"Wrong number of args to quote, had: ">>, Count - 1]
                      , clj_reader:location_meta(List)
                      ),

  Second = clj_core:second(List),
  {ConstExpr, NewEnv} = clj_env:pop_expr(analyze_const(Env, Second)),
  Expr = #{ op   => quote
          , env  => ?DEBUG(Env)
          , expr => ConstExpr
          , form => List
          },
  clj_env:push_expr(NewEnv, Expr).

%%------------------------------------------------------------------------------
%% Parse fn*
%%------------------------------------------------------------------------------

-spec parse_fn(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_fn(Env, List) ->
  Op = clj_core:first(List),

  {NameSym, Methods} =
    case clj_core:'symbol?'(clj_core:second(List)) of
      true  -> {clj_core:second(List), clj_core:rest(clj_core:rest(List))};
      false -> {clj_core:gensym(<<"anon__">>), clj_core:rest(List)}
    end,

  %% Check if there is more than one method
  MethodsList = case clj_core:'vector?'(clj_core:first(Methods)) of
                  true  -> [Methods];
                  false -> clj_core:seq2(Methods)
                end,

  Env0 = clj_env:add_locals_scope(Env),

  %% Add the name of the fn as a local with a modified name so that we can
  %% identify the fun as a clojure function when evaluated with erl_eval
  %% (see clojerl.erlang.Fn)
  PrefixNameSym = 'clojerl.erlang.Fn':prefix(NameSym),
  LocalExpr     = #{op => local, name => PrefixNameSym},

  %% If there is a def var we add it to the local scope
  DefNameSym  = get_def_name(Env),
  IsDef       = DefNameSym =/= undefined,
  DefVar      = case IsDef of
                  true  ->
                    DefVarNs    = clj_namespace:current(),
                    DefVarNsSym = clj_namespace:name(DefVarNs),
                    'clojerl.Var':new( clj_core:name(DefVarNsSym)
                                     , clj_core:name(DefNameSym)
                                     );
                  false -> undefined
                end,

  %% If it is a def we only register the var, otherwise register the local.
  Env1 = case IsDef of
           true  ->
             clj_namespace:update_var(DefVar),
             Env0;
           false ->
             clj_env:put_local(Env0, NameSym, LocalExpr)
         end,

  OpMeta      = clj_core:meta(Op),
  OnceKeyword = clj_core:keyword(<<"once">>),
  IsOnce      = clj_core:boolean(clj_core:get(OpMeta, OnceKeyword)),

  %% If this is a var fn then the loop-id should be the function and not
  %% the variable for the named fun.
  LoopId = case get_def_name(Env) of
             undefined -> {fn, PrefixNameSym};
             _         -> {var, DefNameSym}
           end,

  %% Remove def_name do that inner fn* are not influenced by it.
  Env1Bis = remove_def_name(Env1),

  %% If it is a def analyze methods' args but not the body,
  %% we just want arity information first.
  {MethodsExprs, Env2} =
    analyze_fn_methods(Env1Bis, MethodsList, LoopId, IsOnce, not IsDef),

  MethodArityFun = fun (#{fixed_arity := Arity}) -> Arity end,
  IsVariadicFun  = fun (#{'variadic?' := true}) -> true;
                       (_) -> false
                   end,

  AllVariadics = lists:filter(IsVariadicFun, MethodsExprs),
  {IsVariadic, VariadicArity} =
    case AllVariadics of
      [] -> {false, undefined};
      [Variadic | _] -> {true, MethodArityFun(Variadic)}
    end,

  FixedArities = lists:map(MethodArityFun, MethodsExprs -- AllVariadics),
  MaxFixedArity = case FixedArities of
                    [] -> undefined;
                    _ -> lists:max(FixedArities)
                  end,

  %% Validations
  clj_utils:throw_when( length(AllVariadics) >= 2
                      , <<"Can't have more than 1 variadic overload">>
                      , clj_reader:location_meta(List)
                      ),

  DistinctFixedArities = lists:usort(FixedArities),
  clj_utils:throw_when( length(DistinctFixedArities) =/= length(FixedArities)
                      , <<"Can't have 2 or more overloads "
                          "with the same arity">>
                      , clj_reader:location_meta(List)
                      ),

  clj_utils:throw_when( IsVariadic andalso
                        VariadicArity =/= undefined andalso
                        MaxFixedArity =/= undefined andalso
                        MaxFixedArity > VariadicArity
                      , <<"Can't have fixed arity overload "
                          "with more params than variadic overload">>
                      , clj_reader:location_meta(List)
                      ),

  %% Now that we have all the fn info we re-analyze the methods
  %% with the associated var, so that a recursive
  %% call can be correctly resolved.

  {MethodsExprs1, Env4} =
    case IsDef of
      true  ->
        VarMeta = #{ 'variadic?'     => IsVariadic
                   , max_fixed_arity => MaxFixedArity
                   , variadic_arity  => VariadicArity
                   },
        DefVar1 = clj_core:with_meta(DefVar, VarMeta),
        clj_namespace:update_var(DefVar1),

        analyze_fn_methods(Env2, MethodsList, LoopId, IsOnce, true);
      false ->
        {MethodsExprs, Env2}
    end,


  FnExpr = #{ op              => fn
            , env             => ?DEBUG(Env)
            , form            => List
            , 'variadic?'     => IsVariadic
            , max_fixed_arity => MaxFixedArity
            , variadic_arity  => VariadicArity
            , methods         => MethodsExprs1
            , once            => IsOnce
            , local           => LocalExpr
            },

  Env5 = clj_env:remove_locals_scope(Env4),

  clj_env:push_expr(Env5, FnExpr).

-spec analyze_fn_methods( clj_env:env()
                        , ['clojerl.List':type()]
                        , 'clojerl.Symbol':type()
                        , boolean()
                        , boolean()
                        ) ->
 {[map()], clj_env:env()}.
analyze_fn_methods(Env, MethodsList, LoopId, IsOnce, AnalyzeBody) ->
  MethodEnv = maps:put(once, IsOnce, maps:remove(in_try, Env)),
  AnalyzeFnMethodFun = fun(M, EnvAcc) ->
                           analyze_fn_method(EnvAcc, M, LoopId, AnalyzeBody)
                       end,
  Env1 = lists:foldl(AnalyzeFnMethodFun, MethodEnv, MethodsList),
  clj_env:last_exprs(Env1, length(MethodsList)).

-spec analyze_fn_method( clj_env:env()
                       , 'clojerl.List':type()
                       , 'clojerl.Symbol':type()
                       , boolean()
                       ) ->
  clj_env:env().
analyze_fn_method(Env, List, LoopId, AnalyzeBody) ->
  Params = clj_core:first(List),
  clj_utils:throw_when( not clj_core:'vector?'(Params)
                      , <<"Parameter declaration should be a vector">>
                      , clj_reader:location_meta(List)
                      ),

  ParamsList = clj_core:seq2(Params),
  clj_utils:throw_when( not lists:all(fun is_valid_bind_symbol/1, ParamsList)
                      , [ <<"Params must be valid binding symbols, had: ">>
                        , Params
                        ]
                      , clj_reader:location_meta(List)
                      ),

  IsNotAmpersandFun = fun(X) -> clj_core:str(X) =/= <<"&">> end,
  ParamsNames = lists:filter(IsNotAmpersandFun, ParamsList),
  IsVariadic = length(ParamsNames) =/= length(ParamsList),
  Env0 = clj_env:add_locals_scope(Env),
  Env1 = maps:remove(local, Env0),
  Arity = length(ParamsNames),

  ParamExprFun =
    fun(Name, {Id, Exprs}) ->
        ParamExpr = #{ op          => binding
                     , env         => ?DEBUG(Env1)
                     , form        => Name
                     , name        => Name
                     , 'variadic?' => IsVariadic andalso Id == Arity - 1
                     , arg_id      => Id
                     , local       => arg
                     , shadow      => clj_env:get_local(Env1, Name)
                     },
        {Id + 1, [ParamExpr | Exprs]}
    end,
  {_, ParamsExprs} = lists:foldl(ParamExprFun, {0, []}, ParamsNames),

  FixedArity = case IsVariadic of true -> Arity - 1; false -> Arity end,

  {BodyExpr, Env2} =
    case AnalyzeBody of
      true ->
        BodyEnv = clj_env:put_locals(Env1, ParamsExprs),
        BodyEnv1 = clj_env:context(BodyEnv, return),
        BodyEnv2 = clj_env:put(BodyEnv1, loop_id, LoopId),
        BodyEnv3 = clj_env:put(BodyEnv2, loop_locals, length(ParamsExprs)),
        Body = clj_core:rest(List),
        clj_env:pop_expr(analyze_body(BodyEnv3, Body));
      false ->
        {undefined, Env1}
    end,

  %% TODO: check for a single symbol after '&

  FnMethodExpr = #{ op          => fn_method
                  , form        => List
                  , loop_id     => LoopId
                  , env         => ?DEBUG(Env1)
                  , 'variadic?' => IsVariadic
                  , params      => lists:reverse(ParamsExprs)
                  , fixed_arity => FixedArity
                  , body        => BodyExpr
                  },

  Env3 = clj_env:remove_locals_scope(Env2),
  clj_env:push_expr(Env3, FnMethodExpr).

-spec analyze_body(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
analyze_body(Env, List) ->
  DoSym = clj_core:symbol(<<"do">>),
  DoForm = clj_core:cons(DoSym, List),
  analyze_form(Env, DoForm).

-spec is_valid_bind_symbol(any()) -> boolean().
is_valid_bind_symbol(X) ->
  clj_core:'symbol?'(X)
    andalso not clj_core:boolean(clj_core:namespace(X))
    andalso nomatch == re:run(clj_core:name(X), <<"\\.">>).

%%------------------------------------------------------------------------------
%% Parse do
%%------------------------------------------------------------------------------

-spec parse_do(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_do(Env, Form) ->
  StmtEnv = clj_env:context(Env, statement),
  Statements = clj_core:seq2(clj_core:rest(Form)),

  {StatementsList, Return} =
    case Statements of
      [] -> {[], undefined};
      _  -> {lists:droplast(Statements), lists:last(Statements)}
    end,

  Env1 = analyze_forms(StmtEnv, StatementsList),
  {StatementsExprs, Env2} =
    clj_env:last_exprs(Env1, clj_core:count(StatementsList)),

  ReturnEnv = clj_env:context(Env2, return),
  {ReturnExpr, Env3} = clj_env:pop_expr(analyze_form(ReturnEnv, Return)),

  DoExpr = #{ op         => do
            , env        => ?DEBUG(Env)
            , form       => Statements
            , statements => StatementsExprs
            , ret        => ReturnExpr
            },

  clj_env:push_expr(Env3, DoExpr).

%%------------------------------------------------------------------------------
%% Parse if
%%------------------------------------------------------------------------------

-spec parse_if(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_if(Env, Form) ->
  Count = clj_core:count(Form),
  clj_utils:throw_when( Count =/= 3 andalso Count =/= 4
                      , [<<"Wrong number of args to if, had: ">>, Count - 1]
                      , clj_reader:location_meta(Form)
                      ),

  {Test, Then, Else} =
    case Count of
      3 ->
        [_, Test1, Then1] = clj_core:seq2(Form),
        {Test1, Then1, undefined};
      4 ->
        [_, Test1, Then1, Else1] = clj_core:seq2(Form),
        {Test1, Then1, Else1}
    end,

  TestEnv = clj_env:context(Env, expr),
  {TestExpr, Env1} = clj_env:pop_expr(analyze_form(TestEnv, Test)),
  Env2 = analyze_forms(Env1, [Then, Else]),
  {[ThenExpr, ElseExpr], Env3} = clj_env:last_exprs(Env2, 2),

  IfExpr = #{ op   => 'if'
            , form => Form
            , env  => ?DEBUG(Env)
            , test => TestExpr
            , then => ThenExpr
            , else => ElseExpr
            },

  clj_env:push_expr(Env3, IfExpr).

%%------------------------------------------------------------------------------
%% Parse let & parse loop
%%------------------------------------------------------------------------------

-spec parse_let(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_let(Env, Form) ->
  {LetExprExtra, Env1} = analyze_let(Env, Form),
  LetExpr = maps:merge(#{ op   => 'let'
                        , form => Form
                        , env  => ?DEBUG(Env)
                        },
                       LetExprExtra),

  clj_env:push_expr(Env1, LetExpr).

-spec parse_loop(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_loop(Env, Form) ->
  LoopId    = clj_core:gensym(<<"loop_">>),
  OldLoopId = clj_env:get(Env, loop_id),
  Env1 = clj_env:put(Env, loop_id, {loop, LoopId}),

  {LoopExprExtra, Env2} = analyze_let(Env1, Form),
  LoopExpr = maps:merge(#{ op      => loop
                         , form    => Form
                         , env     => ?DEBUG(Env)
                         , loop_id => LoopId
                         },
                        LoopExprExtra),

  Env3 = clj_env:put(Env2, loop_id, OldLoopId),
  clj_env:push_expr(Env3, LoopExpr).

-spec analyze_let(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
analyze_let(Env, Form) ->
  validate_bindings(Form),
  Op = clj_core:first(Form),
  IsLoop = clj_core:equiv(clj_core:symbol(<<"loop*">>), Op),

  PairUp = fun
             PairUp([], Pairs) ->
               lists:reverse(Pairs);
             PairUp([X, Y | Tail], Pairs) ->
               PairUp(Tail, [{X, Y} | Pairs])
           end,
  BindingsVec = clj_core:second(Form),
  BindingsList = clj_core:seq2(BindingsVec),
  BindingPairs = PairUp(BindingsList, []),

  Env1 = clj_env:add_locals_scope(Env),
  Env2 = lists:foldl( fun parse_binding/2
                    , clj_env:put(Env1, is_loop, IsLoop)
                    , BindingPairs
                    ),
  BindingCount = length(BindingPairs),
  {BindingsExprs, Env3} = clj_env:last_exprs(Env2, BindingCount),

  BodyEnv = case IsLoop of
              true ->
                EnvTemp = clj_env:context(Env3, return),
                clj_env:put(EnvTemp, loop_locals, BindingCount);
              false ->
                Env3
            end,
  Body = clj_core:rest(clj_core:rest(Form)),
  {BodyExpr, Env4} = clj_env:pop_expr(analyze_body(BodyEnv, Body)),

  LetExprExtra = #{ body     => BodyExpr
                  , bindings => BindingsExprs
                  },

  Env5 = clj_env:remove_locals_scope(Env4),
  {LetExprExtra, Env5}.

-spec parse_binding({any(), any()}, clj_env:env()) -> clj_env:env().
parse_binding({Name, Init}, Env) ->
  clj_utils:throw_when( not is_valid_bind_symbol(Name)
                      , [<<"Bad binding form: ">>, Name]
                      , clj_reader:location_meta(Name)
                      ),
  OpAtom = case clj_env:get(Env, is_loop) of
             true  -> loop;
             false -> 'let'
           end,
  {InitExpr, Env1} = clj_env:pop_expr(analyze_form(Env, Init)),
  BindExpr = #{ op     => binding
              , env    => ?DEBUG(Env)
              , name   => Name
              , shadow => clj_env:get_local(Env, Name)
              , init   => InitExpr
              , form   => Name
              , local  => OpAtom
              },

  Env2 = clj_env:put_local(Env1, Name, maps:remove(env, BindExpr)),
  clj_env:push_expr(Env2, BindExpr).

-spec validate_bindings('clojerl.List':type()) -> ok.
validate_bindings(Form) ->
  Op = clj_core:first(Form),
  Bindings = clj_core:second(Form),
  clj_utils:throw_when( not clj_core:'vector?'(Bindings),
                       [ Op
                       , <<" requires a vector for its bindings, had: ">>
                       , clj_core:type(Bindings)
                       ]
                      , clj_reader:location_meta(Form)
                      ),

  clj_utils:throw_when( not clj_core:'even?'(clj_core:count(Bindings))
                      , [ Op
                        , <<" requires an even number of forms in binding "
                            "vector, had: ">>
                        , clj_core:count(Bindings)
                        ]
                      , clj_reader:location_meta(Form)
                      ),
  ok.

%%------------------------------------------------------------------------------
%% Parse recur
%%------------------------------------------------------------------------------

-spec parse_recur(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_recur(Env, List) ->
  {LoopType, LoopId} = clj_env:get(Env, loop_id),
  LoopLocals         = clj_env:get(Env, loop_locals),

  clj_utils:throw_when( clj_env:context(Env) =/= return
                        andalso clj_env:context(Env) =/= expr
                      , <<"Can only recur from tail position">>
                      , clj_reader:location_meta(List)
                      ),
  clj_utils:throw_when( LoopLocals =/= clj_core:count(List) - 1
                      , [<<"Mismatched argument count to recur, expected: ">>
                        , LoopLocals
                        , <<" args, had: ">>
                        , clj_core:count(List) - 1
                        ]
                      , clj_reader:location_meta(List)
                      ),

  Env1 = clj_env:context(Env, expr),
  ArgsList = clj_core:seq2(clj_core:rest(List)),
  Env2 = analyze_forms(Env1, ArgsList),
  {ArgsExprs, Env3} = clj_env:last_exprs(Env2, LoopLocals),

  RecurExpr = #{ op         => recur
               , env        => ?DEBUG(Env)
               , form       => List
               , exprs      => ArgsExprs
               , loop_id    => LoopId
               , loop_type  => LoopType
               },

  clj_env:push_expr(Env3, RecurExpr).

%%------------------------------------------------------------------------------
%% Parse def
%%------------------------------------------------------------------------------

-spec parse_def(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_def(Env, List) ->
  Docstring = validate_def_args(List),
  VarSymbol = clj_core:second(List),
  case lookup_var(VarSymbol) of
    undefined ->
      clj_utils:throw( [ <<"Can't refer to qualified var that doesn't exist: ">>
                       , VarSymbol
                       ]
                     , clj_reader:location_meta(VarSymbol)
                     );
    Var ->
      VarNsSym     = clj_core:symbol(clj_core:namespace(Var)),
      CurrentNs    = clj_namespace:current(),
      CurrentNsSym = clj_namespace:name(CurrentNs),
      clj_utils:throw_when( clj_core:namespace(VarSymbol) =/= undefined
                            andalso not clj_core:equiv(CurrentNsSym, VarNsSym)
                          , <<"Can't create defs outside of current ns">>
                          , clj_reader:location_meta(List)
                          ),

      Var1Meta   = clj_core:meta(Var),
      SymbolMeta = clj_core:meta(VarSymbol),
      MetaMaps   = [#{}, Var1Meta, SymbolMeta],
      Var1       = clj_core:with_meta(Var, clj_core:merge(MetaMaps)),

      IsDynamic = 'clojerl.Var':is_dynamic(Var1),
      NameBin   = clj_core:name(VarSymbol),

      NoWarnDynamic = clj_compiler:no_warn_dynamic_var_name(Env),
      clj_utils:warn_when( not NoWarnDynamic
                           andalso not IsDynamic
                           andalso nomatch =/= re:run(NameBin, "\\*.+\\*")
                         , [ <<"Var ">>
                           , NameBin
                           , <<" is not dynamic but its name"
                               " suggests otherwise.~n">>
                           ]
                         , clj_reader:location_meta(VarSymbol)
                         ),

      clj_namespace:update_var(Var1),
      Count = clj_core:count(List),
      Init  = case Docstring of
                undefined when Count =:= 3 -> clj_core:third(List);
                _ when Count =:= 4 -> clj_core:fourth(List);
                _ -> unbound %% TODO: Create type clojerl.Unbound
              end,

      ExprEnv = add_def_name(clj_env:context(Env, expr), VarSymbol),
      {InitExpr, Env1} = clj_env:pop_expr(analyze_form(ExprEnv, Init)),

      Var2 = var_fn_info(Var1, InitExpr),
      clj_namespace:update_var(Var2),

      DefExpr = #{ op      => def
                 , env     => ?DEBUG(Env)
                 , form    => List
                 , name    => VarSymbol
                 , var     => Var2
                 , doc     => Docstring
                 , init    => InitExpr
                 , dynamic => IsDynamic
                 },

      Env2 = restore_def_name(Env1, Env),
      clj_env:push_expr(Env2, DefExpr)
  end.

-spec add_def_name(clj_env:env(), 'clojerl.Symbol':type()) -> clj_env:env().
add_def_name(Env, NameSym) ->
  clj_env:put(Env, def_name, NameSym).

-spec remove_def_name(clj_env:env()) -> clj_env:env().
remove_def_name(Env) ->
  clj_env:remove(Env, def_name).

-spec get_def_name(clj_env:env()) -> 'clojerl.Symbol':type() | undefined.
get_def_name(Env) ->
  clj_env:get(Env, def_name).

-spec restore_def_name(clj_env:env(), clj_env:env()) -> clj_env:env().
restore_def_name(Env, PreviousEnv) ->
  case clj_env:get(PreviousEnv, def_name) of
    undefined ->
      clj_env:remove(Env, def_name);
    NameSym ->
      %% This is just in case there are nested defs
      add_def_name(Env, NameSym)
  end.

-spec var_fn_info('clojerl.Var':type(), map()) -> 'clojerl.Var':type().
var_fn_info(Var, #{op := fn} = Expr) ->
  %% Add information about the associated function
  %% to the var's metadata.
  RemoveKeys = [op, env, methods, form, once, local],
  ExprInfo = maps:without(RemoveKeys, Expr),
  VarMeta = clj_core:meta(Var),
  VarMeta1 = clj_core:merge([VarMeta, ExprInfo, #{'fn?' => true}]),
  clj_core:with_meta(Var, VarMeta1);
var_fn_info(Var, _) ->
  Var.

-spec validate_def_args('clojerl.List':type()) -> undefined | binary().
validate_def_args(List) ->
  Docstring =
    case {clj_core:count(List), clj_core:third(List)} of
      {4, Str} when is_binary(Str) -> Str;
      _ -> undefined
    end,

  case clj_core:count(List) of
    C when C == 2;
           C == 3, Docstring == undefined;
           C == 4, Docstring =/= undefined  ->
      case clj_core:type(clj_core:second(List)) of
        'clojerl.Symbol' -> ok;
        _ -> clj_utils:throw( <<"First argument to def must be a symbol">>
                            , clj_reader:location_meta(List)
                            )
      end,
      Docstring;
    1 ->
      clj_utils:throw( <<"Too few arguments to def">>
                     , clj_reader:location_meta(List)
                     );
    _ ->
      clj_utils:throw( <<"Too many arguments to def">>
                     , clj_reader:location_meta(List)
                     )
  end.

-spec lookup_var('clojerl.Symbol':type()) ->
  'clojerl.Var':type() | undefined.
lookup_var(VarSymbol) ->
  lookup_var(VarSymbol, true).

-spec lookup_var('clojerl.Symbol':type(), boolean()) ->
  'clojerl.Var':type() | undefined.
lookup_var(VarSymbol, true = _CreateNew) ->
  NsSym = case clj_core:namespace(VarSymbol) of
            undefined -> undefined;
            NsStr     -> clj_core:symbol(NsStr)
          end,

  NameSym   = clj_core:symbol(clj_core:name(VarSymbol)),

  CurrentNs    = clj_namespace:current(),
  CurrentNsSym = clj_namespace:name(CurrentNs),

  case clj_core:equiv(CurrentNsSym, NsSym) of
    Equal when Equal; NsSym == undefined ->
      clj_namespace:intern(CurrentNs, NameSym),
      lookup_var(VarSymbol, false);
    false ->
      lookup_var(VarSymbol, false)
  end;
lookup_var(VarSymbol, false) ->
  NsStr = clj_core:namespace(VarSymbol),
  NameStr = clj_core:name(VarSymbol),

  case {NsStr, NameStr} of
    {undefined, NameStr} when NameStr == <<"ns">>;
                              NameStr == <<"in-ns">> ->
      ClojureCoreSym = clj_core:symbol(<<"clojure.core">>, NameStr),
      clj_namespace:find_var(ClojureCoreSym);
    {undefined, _} ->
      CurrentNs    = clj_namespace:current(),
      CurrentNsSym = clj_namespace:name(CurrentNs),
      Symbol = clj_core:symbol(clj_core:name(CurrentNsSym), NameStr),
      clj_namespace:find_var(Symbol);
    {NsStr, NameStr} ->
      Symbol = clj_core:symbol(NsStr, NameStr),
      clj_namespace:find_var(Symbol)
  end.

%%------------------------------------------------------------------------------
%% Parse throw
%%------------------------------------------------------------------------------

-spec parse_throw(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_throw(Env, List) ->
  Count = clj_core:count(List),
  clj_utils:throw_when( Count =/= 2
                      , [ <<"Wrong number of args to throw, had: ">>
                        , Count - 1
                        ]
                      , clj_reader:location_meta(List)
                      ),

  Second = clj_core:second(List),
  ExceptionEnv = clj_env:context(Env, expr),
  {ExceptionExpr, Env1} = clj_env:pop_expr(analyze_form(ExceptionEnv, Second)),

  ThrowExpr = #{ op        => throw
               , env       => ?DEBUG(Env3)
               , form      => List
               , exception => ExceptionExpr
               },

  clj_env:push_expr(Env1, ThrowExpr).

%%------------------------------------------------------------------------------
%% Parse try
%%------------------------------------------------------------------------------

-spec parse_try(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_try(Env, List) ->
  CatchSymbol = clj_core:symbol(<<"catch">>),
  FinallySymbol = clj_core:symbol(<<"finally">>),
  IsCatch = fun(X) ->
                clj_core:'seq?'(X) andalso
                  clj_core:equiv(clj_core:first(X), CatchSymbol)
            end,
  IsFinally = fun(X) ->
                clj_core:'seq?'(X) andalso
                  clj_core:equiv(clj_core:first(X), FinallySymbol)
            end,

  IsNotCatchFinally = fun(X) -> not IsCatch(X) andalso not IsFinally(X) end,

  {Body, CatchFinallyTail} = lists:splitwith( IsNotCatchFinally
                                            , clj_core:seq2(clj_core:rest(List))
                                            ),
  {Catches, FinallyTail}   = lists:splitwith(IsCatch, CatchFinallyTail),
  {Finallies, Tail}        = lists:splitwith(IsFinally, FinallyTail),

  clj_utils:throw_when( Tail =/= []
                      , <<"Only catch or finally clause can follow catch in "
                          "try expression">>
                      , clj_reader:location_meta(List)
                      ),

  clj_utils:throw_when( length(Finallies) > 1
                      , <<"Only one finally clause allowed in try expression">>
                      , clj_reader:location_meta(List)
                      ),

  Env1             = clj_env:put(Env, in_try, true),
  {BodyExpr, Env2} = clj_env:pop_expr(analyze_body(Env1, Body)),

  ParseCatchesFun      = fun(Form, EnvAcc) -> parse_catch(EnvAcc, Form) end,
  Env3                 = clj_env:context(Env2, expr),
  {CatchesExprs, Env4} =
    clj_env:last_exprs( lists:foldl(ParseCatchesFun, Env3, Catches)
                      , length(Catches)
                      ),

  {FinallyExpr, Env5} = case Finallies of
                          [Finally] ->
                            RestFinally = clj_core:rest(Finally),
                            clj_env:pop_expr(analyze_body(Env4, RestFinally));
                          _ ->
                            {undefined, Env4}
                        end,

  TryExpr = #{ op      => 'try'
             , env     => ?DEBUG(Env)
             , form    => List
             , body    => BodyExpr
             , catches => CatchesExprs
             , finally => FinallyExpr
             },

  Env6 = clj_env:remove(Env5, in_try),
  clj_env:push_expr(Env6, TryExpr).

%%------------------------------------------------------------------------------
%% Parse catch
%%------------------------------------------------------------------------------

-spec parse_catch(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_catch(Env, List) ->
  ErrType = clj_core:second(List),
  ErrName = clj_core:third(List),
  Body    = lists:sublist(clj_core:seq2(List), 4, clj_core:count(List)),

  clj_utils:throw_when( not is_valid_bind_symbol(ErrName)
                      , [<<"Bad binding form:">>, ErrName]
                      , clj_reader:location_meta(ErrName)
                      ),

  Env1 = clj_env:remove(Env, in_try),
  Local = #{ op   => binding
           , env  => ?DEBUG(Env1)
           , form => ErrName
           , name => ErrName
           },

  Env2 = clj_env:put_locals(Env1, [Local]),
  {BodyExpr, Env3} = clj_env:pop_expr(analyze_body(Env2, Body)),

  CatchExpr = #{ op    => 'catch'
               , env   => ?DEBUG(Env1)
               , class => ErrType
               , local => Local
               , form  => List
               , body  => BodyExpr
               },

  clj_env:push_expr(Env3, CatchExpr).

%%------------------------------------------------------------------------------
%% Parse var
%%------------------------------------------------------------------------------

-spec parse_var(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_var(Env, List) ->
  Count = clj_core:count(List),
  clj_utils:throw_when( Count =/= 2
                      , [<<"Wrong number of args to var, had: ">>, Count]
                      , clj_reader:location_meta(List)
                      ),

  VarSymbol = clj_core:second(List),

  case resolve(Env, VarSymbol) of
    {{var, Var}, Env1} ->
      VarConstExpr = #{ op   => constant
                      , env  => ?DEBUG(Env)
                      , tag  => clj_core:type(Var)
                      , form => Var
                      },
      clj_env:push_expr(Env1, VarConstExpr);
    {undefined, _} ->
      clj_utils:throw([ <<"Unable to resolve var: ">>
                      , VarSymbol
                      , <<" in this context">>
                      ]
                     , clj_reader:location_meta(VarSymbol)
                     )
  end.

%%------------------------------------------------------------------------------
%% Analyze invoke
%%------------------------------------------------------------------------------

-spec analyze_invoke(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
analyze_invoke(Env, Form) ->
  Env1 = analyze_form(Env, clj_core:first(Form)),

  Args = clj_core:rest(Form),
  Env2 = analyze_forms(Env1, clj_core:seq2(Args)),

  ArgCount = clj_core:count(Args),
  {ArgsExpr, Env3} = clj_env:last_exprs(Env2, ArgCount),
  {FExpr, Env4} = clj_env:pop_expr(Env3),

  InvokeExpr = #{op   => invoke,
                 env  => ?DEBUG(Env4),
                 form => Form,
                 f    => FExpr#{invoke => true},
                 args => ArgsExpr},
  clj_env:push_expr(Env4, InvokeExpr).

%%------------------------------------------------------------------------------
%% Analyze symbol
%%------------------------------------------------------------------------------

-spec analyze_symbol(clj_env:env(), 'clojerl.Symbol':type()) -> clj_env:env().
analyze_symbol(Env, Symbol) ->
  case resolve(Env, Symbol) of
    {undefined, _} ->
      Str = clj_core:str(Symbol),
      clj_utils:throw([ <<"Unable to resolve symbol '">>
                      , Str
                      , <<"' in this context">>
                      ]
                     , clj_reader:location_meta(Symbol)
                     );
    {{local, Local}, Env1} ->
      clj_env:push_expr(Env1, Local#{op => local});
    {{erl_fun, Module, Function, Arity}, Env1} ->
      FunExpr = #{ op       => erl_fun
                 , env      => ?DEBUG(Env1)
                 , form     => Symbol
                 , module   => Module
                 , function => Function
                 , arity    => Arity
                 },
      clj_env:push_expr(Env1, FunExpr);
    {{var, Var}, Env1} ->
      VarExpr = var_expr(Var, Symbol, Env1),
      clj_env:push_expr(Env1, VarExpr)
  end.

-spec var_expr('clojerl.Var':type(), 'clojerl.Symbol':type(), clj_env:env()) ->
  map().
var_expr(Var, Symbol, _Env) ->
 #{ op   => var
  , env  => ?DEBUG(Env)
  , form => Symbol
  , name => Symbol
  , var  => Var
  }.

-type erl_fun() ::  {erl_fun, module(), atom(), integer()}.

-spec resolve(clj_env:env(), 'clojerl.Symbol':env()) ->
  { {var, 'clojerl.Var':type()} | erl_fun() | {local, map()} | undefined
  , clj_env:env()
  }.
resolve(Env, Symbol) ->
  CurrentNs = clj_namespace:current(),
  Local     = clj_env:get_local(Env, Symbol),
  NsStr     = clj_core:namespace(Symbol),
  MappedVar = clj_namespace:mapping(CurrentNs, Symbol),

  case {Local, NsStr, MappedVar} of
    {Local, _, _} when Local =/= undefined ->
      {{local, Local}, Env};
    {_, NsStr, _} when NsStr =/= undefined ->
      case clj_namespace:find_var(Symbol) of
        undefined ->
          %% If there is no var then assume it's a Module:Function pair.
          %% Let's see how this works out.
          {erl_fun(Env, Symbol), Env};
        Var ->
          {{var, Var}, Env}
      end;
    {_, _, MappedVar} when MappedVar =/= undefined ->
      {{var, MappedVar}, Env};
    _ ->
      {undefined, Env}
  end.

-spec erl_fun(clj_env:env(), 'clojerl.Symbol':type()) -> erl_fun().
erl_fun(Env, Symbol) ->
  NsAtom = binary_to_atom(clj_core:namespace(Symbol), utf8),
  {Name, Arity} = erl_fun_arity(clj_core:name(Symbol)),
  NameAtom = binary_to_atom(Name, utf8),

  NoWarnErlFun = clj_compiler:no_warn_symbol_as_erl_fun(Env),
  clj_utils:warn_when( not NoWarnErlFun
                       andalso not is_integer(Arity)
                       andalso Arity =/= <<"e">>
                     , [ <<"'">>, Symbol, <<"'">>
                       , <<" resolved to an Erlang function.">>
                       , <<" Add the suffix '.e' to the symbol's name">>
                       , <<" to remove this warning.">>
                       ]
                     , clj_reader:location_meta(Symbol)
                     ),

  Arity1 = case Arity of
             _ when is_integer(Arity) -> Arity;
             _ -> undefined
           end,

  {erl_fun, NsAtom, NameAtom, Arity1}.

-spec erl_fun_arity(binary()) -> {binary(), undefined | integer()}.
erl_fun_arity(Name) ->
  case binary:split(Name, <<".">>, [global]) of
    [_] -> {Name, undefined};
    Parts ->
      Last = lists:last(Parts),
      case {re:run(Last, <<"\\d+">>), Last} of
        {nomatch, <<"e">>} when length(Parts) > 1 ->
          {iolist_to_binary(lists:droplast(Parts)), <<"e">>};
        {nomatch, _} ->
          {Name, undefined};
        _ ->
          NameParts = clj_utils:binary_join(lists:droplast(Parts), <<".">>),
          Arity = binary_to_integer(Last),
          {iolist_to_binary(NameParts), Arity}
      end
  end.

%%------------------------------------------------------------------------------
%% Analyze vector
%%------------------------------------------------------------------------------

-spec analyze_vector(clj_env:env(), 'clojerl.Vector':type()) -> clj_env:env().
analyze_vector(Env, Vector) ->
  Count = clj_core:count(Vector),
  ExprEnv = clj_env:context(Env, expr),
  Items = clj_core:seq2(Vector),
  Env1 = analyze_forms(ExprEnv, Items),
  {ItemsExpr, Env2} = clj_env:last_exprs(Env1, Count),

  VectorExpr = #{ op    => vector
                , env   => ?DEBUG(Env2)
                , form  => Vector
                , items => ItemsExpr
                },

  clj_env:push_expr(Env2, VectorExpr).

%%------------------------------------------------------------------------------
%% Analyze map
%%------------------------------------------------------------------------------

-spec analyze_map(clj_env:env(), 'clojerl.Map':type()) -> clj_env:env().
analyze_map(Env, Map) ->
  Keys = clj_core:keys(Map),
  Vals = clj_core:vals(Map),

  Count = clj_core:count(Map),
  ExprEnv = clj_env:context(Env, expr),

  Env1 = analyze_forms(ExprEnv, Keys),
  {KeysExpr, Env2} = clj_env:last_exprs(Env1, Count),
  Env3 = analyze_forms(Env2, Vals),
  {ValsExpr, Env4} = clj_env:last_exprs(Env3, Count),

  MapExpr = #{ op   => map
             , env  => ?DEBUG(Env4)
             , form => Map
             , keys => KeysExpr
             , vals => ValsExpr
             },

  clj_env:push_expr(Env4, MapExpr).

%%------------------------------------------------------------------------------
%% Analyze set
%%------------------------------------------------------------------------------

-spec analyze_set(clj_env:env(), 'clojerl.Set':type()) -> clj_env:env().
analyze_set(Env, Set) ->
  ExprEnv = clj_env:context(Env, expr),
  Items = clj_core:seq2(Set),
  Env1 = analyze_forms(ExprEnv, Items),

  Count = clj_core:count(Set),
  {ItemsExpr, Env2} = clj_env:last_exprs(Env1, Count),

  VectorExpr = #{ op    => set
                , env   => ?DEBUG(Env2)
                , form  => Set
                , items => ItemsExpr
                },

  clj_env:push_expr(Env2, VectorExpr).
