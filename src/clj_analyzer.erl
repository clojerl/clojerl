-module(clj_analyzer).

-include("clojerl.hrl").

-export([ analyze/2
        , macroexpand_1/2
        , is_special/1
        ]).

-define(DEBUG(X), X).

-spec analyze(clj_env:env(), any()) -> clj_env:env().
analyze(Env0, Form) ->
  {Expr, Env} =  clj_env:pop_expr(analyze_form(Env0, Form)),
  clj_env:push_expr(Env, Expr#{top_level => true}).

-spec is_special('clojerl.Symbol':type()) -> boolean().
is_special(S) ->
  clj_core:'symbol?'(S) andalso
    maps:is_key(clj_core:str(S), special_forms()).

-spec macroexpand_1(clj_env:env(), 'clojerl.List':type()) -> any().
macroexpand_1(Env, Form) ->
  Op       = clj_core:first(Form),
  MacroVar = case clj_core:'symbol?'(Op) of
               true -> lookup_var(Op, false);
               false -> ?NIL
             end,

  case
    not is_special(Op)
    andalso (MacroVar =/= ?NIL)
    andalso ('clojerl.Var':is_macro(MacroVar))
  of
    true ->
      Args = [Form, Env | clj_core:to_list(clj_core:rest(Form))],
      'clojerl.IFn':apply(MacroVar, Args);
    false -> Form
  end.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec special_forms() -> #{'clojerl.Symbol':type() => fun() | ?NIL}.
special_forms() ->
  #{ <<"def">>          => fun parse_def/2
   , <<"quote">>        => fun parse_quote/2
   , <<"fn*">>          => fun parse_fn/2
   , <<"do">>           => fun parse_do/2
   , <<"if">>           => fun parse_if/2
   , <<"let*">>         => fun parse_let/2
   , <<"letfn*">>       => fun parse_letfn/2
   , <<"loop*">>        => fun parse_loop/2
   , <<"recur">>        => fun parse_recur/2
   , <<"case*">>        => fun parse_case/2
   , <<"throw">>        => fun parse_throw/2
   , <<"try">>          => fun parse_try/2

   , <<"var">>          => fun parse_var/2

   , <<"erl-on-load*">> => fun parse_on_load/2

   , <<"import*">>      => fun parse_import/2
   , <<"new">>          => fun parse_new/2
   , <<"deftype*">>     => fun parse_deftype/2
   , <<"defprotocol*">> => fun parse_defprotocol/2
   , <<"extend-type*">> => fun parse_extend_type/2

     %% These are special forms but they are meant to be parsed under other
     %% special forms. If they are at function position then they should be
     %% analyzed as regular symbols.
   , <<"catch">>        => fun analyze_invoke/2
   , <<"finally">>      => fun analyze_invoke/2
   , <<"&">>            => fun analyze_invoke/2
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
        false -> analyze_seq(Env, Form)
      end;
    'clojerl.Symbol' ->
      analyze_symbol(Env, Form);
    'clojerl.Vector' ->
      analyze_vector(Env, Form);
    'clojerl.Map' ->
      analyze_map(Env, Form);
    'clojerl.Set' ->
      analyze_set(Env, Form);
    'clojerl.erlang.Tuple' when element(1, Form) =/= ?TYPE ->
      analyze_tuple(Env, Form);
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

-spec analyze_seq(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
analyze_seq(Env0, List) ->
  OldLocation   = clj_env:location(Env0),
  MaybeLocation = clj_reader:location_meta(List),
  Env           = clj_env:maybe_update_location(Env0, MaybeLocation),
  Op            = clj_core:first(List),

  clj_utils:error_when( Op =:= ?NIL
                      , <<"Can't call nil">>
                      , clj_env:location(Env)
                      ),

  ExpandedList = macroexpand_1(Env, List),
  Env1 = case clj_core:equiv(List, ExpandedList) of
           true ->
             AnaInvoke = fun analyze_invoke/2,
             Fun = case clj_core:'symbol?'(Op) of
                     true ->
                       maps:get(clj_core:name(Op), special_forms(), AnaInvoke);
                     false ->
                       AnaInvoke
                   end,
             Fun(Env, List);
           false ->
             analyze_form(Env, ExpandedList)
         end,
  clj_env:location(Env1, OldLocation).

%%------------------------------------------------------------------------------
%% Parse quote
%%------------------------------------------------------------------------------

-spec parse_quote(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_quote(Env, List) ->
  Count = clj_core:count(List),
  clj_utils:error_when( Count =/= 2
                      , [<<"Wrong number of args to quote, had: ">>, Count - 1]
                      , clj_env:location(Env)
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
                  false -> clj_core:to_list(Methods)
                end,

  Env0 = clj_env:add_locals_scope(Env),

  %% Add the name of the fn as a local
  LocalExpr     = #{ op   => local
                   , env  => ?DEBUG(Env)
                   , name => NameSym
                   },

  %% If there is a def var we add it to the local scope
  DefNameSym  = get_def_name(Env),
  IsDef       = DefNameSym =/= ?NIL,
  DefVar      = case IsDef of
                  true  ->
                    DefVarNs    = clj_namespace:current(),
                    DefVarNsSym = clj_namespace:name(DefVarNs),
                    'clojerl.Var':?CONSTRUCTOR( clj_core:name(DefVarNsSym)
                                              , clj_core:name(DefNameSym)
                                              );
                  false -> ?NIL
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
             ?NIL -> {fn, NameSym};
             _    -> {var, DefNameSym}
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
      [] -> {false, ?NIL};
      [Variadic | _] -> {true, MethodArityFun(Variadic)}
    end,

  FixedArities = lists:map(MethodArityFun, MethodsExprs -- AllVariadics),
  MaxFixedArity = case FixedArities of
                    [] -> ?NIL;
                    _  -> lists:max(FixedArities)
                  end,

  %% Validations
  clj_utils:error_when( length(AllVariadics) >= 2
                      , <<"Can't have more than 1 variadic overload">>
                      , clj_env:location(Env)
                      ),

  DistinctFixedArities = lists:usort(FixedArities),
  clj_utils:error_when( length(DistinctFixedArities) =/= length(FixedArities)
                      , <<"Can't have 2 or more overloads "
                          "with the same arity">>
                      , clj_env:location(Env)
                      ),

  clj_utils:error_when( IsVariadic andalso
                        VariadicArity =/= ?NIL andalso
                        MaxFixedArity =/= ?NIL andalso
                        MaxFixedArity > VariadicArity
                      , <<"Can't have fixed arity overload "
                          "with more params than variadic overload">>
                      , clj_env:location(Env)
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
                   , 'fn?'           => true
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
  clj_utils:error_when( not clj_core:'vector?'(Params)
                      , <<"Parameter declaration should be a vector">>
                      , clj_env:location(Env)
                      ),

  ParamsList = clj_core:to_list(Params),
  clj_utils:error_when( not lists:all(fun is_valid_bind_symbol/1, ParamsList)
                      , [ <<"Params must be valid binding symbols, had: ">>
                        , Params
                        ]
                      , clj_env:location(Env)
                      ),

  IsNotAmpersandFun = fun(X) -> clj_core:str(X) =/= <<"&">> end,
  ParamsNames       = lists:filter(IsNotAmpersandFun, ParamsList),
  IsVariadic        = length(ParamsNames) =/= length(ParamsList),

  Env0  = clj_env:add_locals_scope(Env),
  Env1  = maps:remove(local, Env0),
  Arity = length(ParamsNames),

  ParamsExprs = analyze_method_params(Env, IsVariadic, Arity, ParamsNames),

  FixedArity  = case IsVariadic of true -> Arity - 1; false -> Arity end,

  OldLoopId     = clj_env:get(Env1, loop_id),
  OldLoopLocals = clj_env:get(Env1, loop_locals),

  {BodyExpr, Env2} =
    case AnalyzeBody of
      true ->
        BodyEnv  = clj_env:put_locals(Env1, ParamsExprs),
        BodyEnv1 = clj_env:context(BodyEnv, return),
        BodyEnv2 = clj_env:put(BodyEnv1, loop_id, LoopId),
        BodyEnv3 = clj_env:put(BodyEnv2, loop_locals, length(ParamsExprs)),
        Body     = clj_core:rest(List),
        clj_env:pop_expr(analyze_body(BodyEnv3, Body));
      false ->
        {?NIL, Env1}
    end,

  %% TODO: check for a single symbol after '&

  FnMethodExpr = #{ op          => fn_method
                  , env         => ?DEBUG(Env1)
                  , form        => List
                  , loop_id     => LoopId
                  , 'variadic?' => IsVariadic
                  , params      => ParamsExprs
                  , fixed_arity => FixedArity
                  , body        => BodyExpr
                  },

  Env3 = clj_env:put(Env2, loop_id, OldLoopId),
  Env4 = clj_env:put(Env3, loop_locals, OldLoopLocals),
  Env5 = clj_env:remove_locals_scope(Env4),
  clj_env:push_expr(Env5, FnMethodExpr).

-spec analyze_method_params(clj_env:env(), ['clojerl.Symbol':type()]) ->
  [any()].
analyze_method_params(Env, ParamsNames) ->
  analyze_method_params(Env, false, -1, ParamsNames).

-spec analyze_method_params( clj_env:env()
                           , boolean()
                           , non_neg_integer()
                           , ['clojerl.Symbol':type()]
                           ) -> [any()].
analyze_method_params(Env, IsVariadic, Arity, ParamsNames) ->
  ParamExprFun =
    fun(Name0, {Id, MappedParams, ParamsExprs}) ->
        %% Check if there is a another
        Name0Bin  = clj_core:str(Name0),
        Count     = maps:get(Name0Bin, MappedParams, -1) + 1,
        Name      = case Count of
                      0 -> Name0;
                      N ->
                        NBin = erlang:integer_to_binary(N),
                        clj_core:symbol(<<Name0Bin/binary, NBin/binary>>)
                    end,
        ParamExpr = #{ op          => binding
                     , env         => ?DEBUG(Env)
                     , form        => Name
                     , name        => Name
                     , 'variadic?' => IsVariadic andalso Id == Arity - 1
                     , arg_id      => Id
                     , local       => arg
                     , shadow      => clj_env:get_local(Env, Name)
                     },
        { Id - 1
        , MappedParams#{Name0Bin => Count}
        , [ParamExpr | ParamsExprs]
        }
    end,
  ParamCount = length(ParamsNames),
  {_, _, ParamsExprs} = lists:foldl(ParamExprFun
                                   , {ParamCount - 1, #{}, []}
                                    %% We reverse the order so if there is
                                    %% any repeated parameter, the last
                                    %% is the one resolved in the body.
                                   , lists:reverse(ParamsNames)
                                   ),
  ParamsExprs.

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
  Statements = clj_core:to_list(clj_core:rest(Form)),

  {StatementsList, Return} =
    case Statements of
      [] -> {[], ?NIL};
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
  clj_utils:error_when( Count =/= 3 andalso Count =/= 4
                      , [<<"Wrong number of args to if, had: ">>, Count - 1]
                      , clj_env:location(Env)
                      ),

  {Test, Then, Else} =
    case Count of
      3 ->
        [_, Test1, Then1] = clj_core:to_list(Form),
        {Test1, Then1, ?NIL};
      4 ->
        [_, Test1, Then1, Else1] = clj_core:to_list(Form),
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
  OldLoopId     = clj_env:get(Env, loop_id),
  OldLoopLocals = clj_env:get(Env, loop_locals),
  LoopId        = clj_core:gensym(<<"loop_">>),

  Env1 = clj_env:put(Env, loop_id, {loop, LoopId}),

  {LoopExprExtra, Env2} = analyze_let(Env1, Form),
  LoopExpr = maps:merge(#{ op      => loop
                         , form    => Form
                         , env     => ?DEBUG(Env)
                         , loop_id => LoopId
                         },
                        LoopExprExtra),

  Env3 = clj_env:put(Env2, loop_id, OldLoopId),
  Env4 = clj_env:put(Env3, loop_locals, OldLoopLocals),
  clj_env:push_expr(Env4, LoopExpr).

-spec analyze_let(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
analyze_let(Env, Form) ->
  validate_bindings(Env, Form),
  Op = clj_core:first(Form),
  IsLoop = clj_core:equiv(clj_core:symbol(<<"loop*">>), Op),

  PairUp = fun
             PairUp([], Pairs) ->
               lists:reverse(Pairs);
             PairUp([X, Y | Tail], Pairs) ->
               PairUp(Tail, [{X, Y} | Pairs])
           end,
  BindingsVec = clj_core:second(Form),
  BindingsList = clj_core:to_list(BindingsVec),
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
  clj_utils:error_when( not is_valid_bind_symbol(Name)
                      , [<<"Bad binding form: ">>, Name]
                      , clj_env:location(Env)
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

-spec validate_bindings(clj_env:env(), 'clojerl.List':type()) -> ok.
validate_bindings(Env, Form) ->
  Op = clj_core:first(Form),
  Bindings = clj_core:second(Form),
  clj_utils:error_when( not clj_core:'vector?'(Bindings),
                       [ Op
                       , <<" requires a vector for its bindings, had: ">>
                       , clj_core:type(Bindings)
                       ]
                      , clj_env:location(Env)
                      ),

  clj_utils:error_when( not clj_core:'even?'(clj_core:count(Bindings))
                      , [ Op
                        , <<" requires an even number of forms in binding "
                            "vector, had: ">>
                        , clj_core:count(Bindings)
                        ]
                      , clj_env:location(Env)
                      ),
  ok.

%%------------------------------------------------------------------------------
%% Parse recur
%%------------------------------------------------------------------------------

-spec parse_recur(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_recur(Env, List) ->
  {LoopType, LoopId} = clj_env:get(Env, loop_id),
  LoopLocals         = clj_env:get(Env, loop_locals),

  clj_utils:error_when( clj_env:context(Env) =/= return
                        andalso clj_env:context(Env) =/= expr
                      , <<"Can only recur from tail position">>
                      , clj_env:location(Env)
                      ),
  clj_utils:error_when( LoopLocals =/= clj_core:count(List) - 1
                      , [<<"Mismatched argument count to recur, expected: ">>
                        , LoopLocals
                        , <<" args, had: ">>
                        , clj_core:count(List) - 1
                        ]
                      , clj_env:location(Env)
                      ),

  Env1 = clj_env:context(Env, expr),
  ArgsList = clj_core:to_list(clj_core:rest(List)),
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
%% Parse letfn
%%------------------------------------------------------------------------------

-spec parse_letfn(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_letfn(Env0, Form) ->
  [ _ %% letfn*
  , FnSpecs
  | Body
  ] = clj_core:to_list(Form),

  {FnNames, FnForms} = partition_fun_defs(clj_core:to_list(FnSpecs)),

  Env  = clj_env:add_locals_scope(Env0),

  BindingFun = fun(FnName) ->
                   #{ op     => local
                    , env    => ?DEBUG(Env)
                    , name   => FnName
                    , shadow => clj_env:get_local(Env, FnName)
                    , form   => FnName
                    }
               end,
  FnNamesExprs = lists:map(BindingFun, FnNames),

  Env1 = clj_env:put_locals(Env, FnNamesExprs),

  {FnsExprs, Env2} = clj_env:last_exprs( analyze_forms(Env1, FnForms)
                                       , length(FnNames)
                                       ),

  {BodyExpr, Env3} = clj_env:pop_expr(analyze_body(Env2, Body)),

  LetFnExpr = #{ op   => letfn
               , env  => ?DEBUG(Env)
               , form => Form
               , vars => FnNamesExprs
               , fns  => FnsExprs
               , body => BodyExpr
               },

  Env4 = clj_env:remove_locals_scope(Env3),

  clj_env:push_expr(Env4, LetFnExpr).

-spec partition_fun_defs([any()]) -> {[any()], [any()]}.
partition_fun_defs(FnSpecs) ->
  partition_fun_defs(FnSpecs, {[], []}).

-spec partition_fun_defs([any()], {[any()], [any()]}) -> {[any()], [any()]}.
partition_fun_defs([], {Names, Funs}) ->
  {lists:reverse(Names), lists:reverse(Funs)};
partition_fun_defs([Name, Fun | Rest], {NamesAcc, FunsAcc}) ->
  partition_fun_defs(Rest, {[Name | NamesAcc], [Fun | FunsAcc]}).

%%------------------------------------------------------------------------------
%% Parse case*
%%------------------------------------------------------------------------------

-spec parse_case(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_case(Env, List) ->
  Test             = clj_core:second(List),
  {TestExpr, Env1} = clj_env:pop_expr(analyze_form(Env, Test)),

  PatternsBodies     = clj_core:rest(clj_core:rest(List)),
  PatternsBodiesList = clj_core:to_list(PatternsBodies),

  { ClausesExprs
  , DefaultExpr
  , Env2
  } = parse_patters_bodies(Env1, PatternsBodiesList),

  CaseExpr = #{ op      => 'case'
              , env     => ?DEBUG(Env)
              , form    => List
              , test    => TestExpr
              , clauses => ClausesExprs
              , default => DefaultExpr
              },

  clj_env:push_expr(Env2, CaseExpr).

-spec parse_patters_bodies(clj_env:env(), [any()]) ->
  {list(), clj_env:env()}.
parse_patters_bodies(Env1, PatternsBodies) ->
  parse_patters_bodies(Env1, PatternsBodies, []).

-spec parse_patters_bodies(clj_env:env(), [any()], [any()]) ->
  {list(), any(), clj_env:env()}.
parse_patters_bodies(Env, [], PatternBodyPairs) ->
  { lists:reverse(PatternBodyPairs)
  , ?NIL
  , Env
  };
parse_patters_bodies(Env, [Default], PatternBodyPairs) ->
  {DefaultExpr, Env1} = clj_env:pop_expr(analyze_form(Env, Default)),

  { lists:reverse(PatternBodyPairs)
  , DefaultExpr
  , Env1
  };
parse_patters_bodies(Env, [Pat, Body | Rest], PatternBodyPairs) ->
  {PatternExpr, Env1} = clj_env:pop_expr(analyze_form(Env, Pat)),
  {BodyExpr, Env2} = clj_env:pop_expr(analyze_form(Env1, Body)),
  parse_patters_bodies( Env2
                      , Rest
                      , [{PatternExpr, BodyExpr} | PatternBodyPairs]
                      ).

%%------------------------------------------------------------------------------
%% Parse def
%%------------------------------------------------------------------------------

-spec parse_def(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_def(Env, List) ->
  Docstring    = validate_def_args(Env, List),
  VarSymbol0   = clj_core:second(List),
  SymbolMeta0  = clj_core:meta(VarSymbol0),
  ArgLists     = clj_core:get(SymbolMeta0, arglists),
  ArgListsMap  = case ArgLists of
                   ?NIL -> #{};
                   ArgLists -> #{arglists => clj_core:second(ArgLists)}
                 end,
  DocstringMap = case Docstring of
                   ?NIL -> #{};
                   Docstring -> #{doc => Docstring}
                 end,

  LocationInfo = clj_env:get(Env, location),

  SymbolMeta   = clj_core:merge([ ArgListsMap
                                , DocstringMap
                                , SymbolMeta0
                                , LocationInfo
                                ]),
  VarSymbol    = clj_core:with_meta(VarSymbol0, SymbolMeta),

  Var0 = lookup_var(VarSymbol),

  clj_utils:error_when( Var0 =:= ?NIL
                      , [ <<"Can't refer to qualified var that doesn't exist: ">>
                        , VarSymbol
                        ]
                      , clj_env:location(Env)
                      ),

  VarNsSym     = clj_core:symbol(clj_core:namespace(Var0)),
  CurrentNs    = clj_namespace:current(),
  CurrentNsSym = clj_namespace:name(CurrentNs),
  clj_utils:error_when( clj_core:namespace(VarSymbol) =/= ?NIL
                        andalso not clj_core:equiv(CurrentNsSym, VarNsSym)
                      , <<"Can't create defs outside of current ns">>
                      , clj_env:location(Env)
                      ),

  NameBin       = clj_core:name(VarSymbol),
  VarMeta       = clj_core:merge([ clj_core:meta(Var0)
                                 , SymbolMeta
                                 , #{ ns   => VarNsSym
                                    , name => clj_core:symbol(NameBin)
                                    }
                                 , ArgListsMap
                                 ]),
  Var           = clj_core:with_meta(Var0, VarMeta),

  IsDynamic     = 'clojerl.Var':is_dynamic(Var),

  NoWarnDynamic = clj_compiler:no_warn_dynamic_var_name(Env),
  clj_utils:warn_when( not NoWarnDynamic
                       andalso not IsDynamic
                       andalso nomatch =/= re:run(NameBin, "\\*.+\\*")
                     , [ <<"Warning: ">>
                       , NameBin
                       , <<" is not dynamic but its name"
                           " suggests otherwise.~n">>
                       ]
                     , clj_env:location(Env)
                     ),

  clj_namespace:update_var(Var),
  Count = clj_core:count(List),
  Init  = case Docstring of
            ?NIL when Count =:= 3 -> clj_core:third(List);
            _ when Count =:= 4 -> clj_core:fourth(List);
            _ -> ?UNBOUND
          end,

  ExprEnv = add_def_name(clj_env:context(Env, expr), VarSymbol),
  {InitExpr, Env1} = clj_env:pop_expr(analyze_form(ExprEnv, Init)),

  Var1 = var_fn_info(Var, InitExpr),
  clj_namespace:update_var(Var1),

  {MetaExpr, Env2} = clj_env:pop_expr(analyze_form(Env1, VarMeta)),

  DefExpr = #{ op      => def
             , env     => ?DEBUG(Env)
             , form    => List
             , name    => VarSymbol
             , var     => Var1
             , init    => InitExpr
             , meta    => MetaExpr
             , dynamic => IsDynamic
             },

  Env3 = restore_def_name(Env2, Env),
  clj_env:push_expr(Env3, DefExpr).

-spec add_def_name(clj_env:env(), 'clojerl.Symbol':type()) -> clj_env:env().
add_def_name(Env, NameSym) ->
  clj_env:put(Env, def_name, NameSym).

-spec remove_def_name(clj_env:env()) -> clj_env:env().
remove_def_name(Env) ->
  clj_env:remove(Env, def_name).

-spec get_def_name(clj_env:env()) -> 'clojerl.Symbol':type() | ?NIL.
get_def_name(Env) ->
  clj_env:get(Env, def_name).

-spec restore_def_name(clj_env:env(), clj_env:env()) -> clj_env:env().
restore_def_name(Env, PreviousEnv) ->
  case clj_env:get(PreviousEnv, def_name) of
    ?NIL ->
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
  ExprInfo   = maps:without(RemoveKeys, Expr),
  VarMeta    = clj_core:meta(Var),
  VarMeta1   = clj_core:merge([VarMeta, ExprInfo, #{'fn?' => true}]),
  clj_core:with_meta(Var, VarMeta1);
var_fn_info(Var, _) ->
  Var.

-spec validate_def_args(clj_env:env(), 'clojerl.List':type()) ->
  ?NIL | binary().
validate_def_args(Env, List) ->
  Docstring =
    case {clj_core:count(List), clj_core:third(List)} of
      {4, Str} when is_binary(Str) -> Str;
      _ -> ?NIL
    end,

  case clj_core:count(List) of
    C when C == 2;
           C == 3, Docstring == ?NIL;
           C == 4, Docstring =/= ?NIL  ->
      case clj_core:type(clj_core:second(List)) of
        'clojerl.Symbol' -> ok;
        _ -> clj_utils:error( <<"First argument to def must be a symbol">>
                            , clj_env:location(Env)
                            )
      end,
      Docstring;
    1 ->
      clj_utils:error( <<"Too few arguments to def">>
                     , clj_env:location(Env)
                     );
    _ ->
      clj_utils:error( <<"Too many arguments to def">>
                     , clj_env:location(Env)
                     )
  end.

-spec lookup_var('clojerl.Symbol':type()) ->
  'clojerl.Var':type() | ?NIL.
lookup_var(VarSymbol) ->
  lookup_var(VarSymbol, true).

-spec lookup_var('clojerl.Symbol':type(), boolean()) ->
  'clojerl.Var':type() | ?NIL.
lookup_var(VarSymbol, true = _CreateNew) ->
  NsSym = case clj_core:namespace(VarSymbol) of
            ?NIL  -> ?NIL;
            NsStr -> clj_core:symbol(NsStr)
          end,

  NameSym   = clj_core:symbol(clj_core:name(VarSymbol)),

  CurrentNs    = clj_namespace:current(),
  CurrentNsSym = clj_namespace:name(CurrentNs),

  case clj_core:equiv(CurrentNsSym, NsSym) of
    Equal when Equal; NsSym == ?NIL ->
      clj_namespace:intern(CurrentNs, NameSym),
      lookup_var(VarSymbol, false);
    false ->
      lookup_var(VarSymbol, false)
  end;
lookup_var(VarSymbol, false) ->
  NsStr = clj_core:namespace(VarSymbol),
  NameStr = clj_core:name(VarSymbol),

  case {NsStr, NameStr} of
    {?NIL, NameStr} when NameStr == <<"ns">>;
                              NameStr == <<"in-ns">> ->
      ClojureCoreSym = clj_core:symbol(<<"clojure.core">>, NameStr),
      clj_namespace:find_var(ClojureCoreSym);
    {?NIL, _} ->
      CurrentNs    = clj_namespace:current(),
      CurrentNsSym = clj_namespace:name(CurrentNs),
      Symbol = clj_core:symbol(clj_core:name(CurrentNsSym), NameStr),
      clj_namespace:find_var(Symbol);
    {NsStr, NameStr} ->
      Symbol = clj_core:symbol(NsStr, NameStr),
      clj_namespace:find_var(Symbol)
  end.

%%------------------------------------------------------------------------------
%% Parse import
%%------------------------------------------------------------------------------

-spec parse_import(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_import(Env, Form) ->
  ArgCount = clj_core:count(Form) - 1,
  clj_utils:error_when( ArgCount > 1
                      , [ <<"Wrong number of args to import*, had: ">>
                        , ArgCount
                        ]
                      , clj_env:location(Env)
                      ),

  TypeName = clj_core:second(Form),
  clj_utils:error_when( not is_binary(TypeName)
                      , <<"Argument to import* must be a string">>
                      , clj_env:location(Env)
                      ),

  NewExpr = #{ op       => import
             , env      => ?DEBUG(Env)
             , form     => Form
             , typename => TypeName
             },

  clj_env:push_expr(Env, NewExpr).

%%------------------------------------------------------------------------------
%% Parse new
%%------------------------------------------------------------------------------

-spec parse_new(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_new(Env, Form) ->
  [_, Type | Args] = clj_core:to_list(Form),

  {TypeExpr, Env1} = clj_env:pop_expr(analyze_form(Env, Type)),

  {ArgsExprs, Env2} =
    clj_env:last_exprs(analyze_forms(Env1, Args), length(Args)),

  NewExpr = #{ op   => new
             , env  => ?DEBUG(Env)
             , form => Form
             , type => TypeExpr
             , args => ArgsExprs
             },

  clj_env:push_expr(Env2, NewExpr).

%%------------------------------------------------------------------------------
%% Parse deftype
%%------------------------------------------------------------------------------

-spec parse_deftype(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_deftype(Env, Form) ->
  [ _ % deftype*
  , Name
  , TypeSym
  , Fields
  , _ % :implements
  , Interfaces
  | Methods
  ] = clj_core:to_list(Form),

  FieldsList  = clj_core:to_list(Fields),
  FieldsExprs = analyze_method_params(Env, FieldsList),

  %% The analyzer adds the fields to the local scope of the methods,
  %% but it is the emitter who will need to pattern match the first argument
  %% so that they are actually available in the methods' body.
  Env1        = clj_env:add_locals_scope(Env),
  Env2        = clj_env:put_locals(Env1, FieldsExprs),

  %% HACK: by emitting the tyep we make the module available, which means the
  %% type gets resolved. But we remove all protocols and methods, thus
  %% generating just a dummy erlang module for the type.
  DeftypeDummyExpr = #{ op        => deftype
                      , env       => ?DEBUG(Env)
                      , form      => Form
                      , name      => Name
                      , type      => TypeSym
                      , fields    => FieldsExprs
                      , protocols => []
                      , methods   => []
                      },
  _ = clj_emitter:emit(clj_env:push_expr(Env2, DeftypeDummyExpr)),

  Env3 = lists:foldl(fun analyze_deftype_method/2, Env2, Methods),
  {MethodsExprs, Env4} = clj_env:last_exprs(Env3, length(Methods)),

  IntfsList   = clj_core:to_list(Interfaces),
  {InterfacesExprs, Env5} = clj_env:last_exprs( analyze_forms(Env4, IntfsList)
                                              , length(IntfsList)
                                              ),

  DeftypeExpr = #{ op        => deftype
                 , env       => ?DEBUG(Env)
                 , form      => Form
                 , name      => Name
                 , type      => TypeSym
                 , fields    => FieldsExprs
                 , protocols => InterfacesExprs
                 , methods   => MethodsExprs
                 },

  Env6 = clj_env:remove_locals_scope(Env5),
  clj_env:push_expr(Env6, DeftypeExpr).

-spec analyze_deftype_method('clojerl.List':type(), clj_env:env()) ->
  clj_env:env().
analyze_deftype_method(Form, Env) ->
  [MethodName, Args | _Body] = clj_core:to_list(Form),

  clj_utils:error_when( not clj_core:'symbol?'(MethodName)
                      , [ <<"Method name must be a symbol, had: ">>
                        , clj_core:type(MethodName)
                        ]
                      , clj_env:location(Env)
                      ),

  clj_utils:error_when( not clj_core:'vector?'(Args)
                      , [ <<"Parameter listing should be a vector, had: ">>
                        , clj_core:type(Args)
                        ]
                      , clj_env:location(Env)
                      ),

  clj_utils:error_when( clj_core:count(Args) < 1
                      , [ <<"Must supply at least one argument for 'this' in: ">>
                        , MethodName
                        ]
                      , clj_env:location(Env)
                      ),

  LoopId = {function, MethodName},
  {MethodExpr, Env1} = clj_env:pop_expr(analyze_fn_method( Env
                                                         , clj_core:rest(Form)
                                                         , LoopId
                                                         , true
                                                         )),

  MethodExpr1 = maps:merge( maps:remove('variadic?', MethodExpr)
                          , #{ op   => method
                             , env  => ?DEBUG(Env)
                             , form => Form
                             , name => MethodName
                             }
                          ),

  clj_env:push_expr(Env1, MethodExpr1).

%%------------------------------------------------------------------------------
%% Parse defprotocol
%%------------------------------------------------------------------------------

-spec parse_defprotocol(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_defprotocol(Env, List) ->
  [ _ % defprotocol*
  , FQNameSym
  | MethodsSigs
  ] = clj_core:to_list(List),

  clj_utils:error_when( not clj_core:'symbol?'(FQNameSym)
                      , [ <<"Protocol name should be a symbol, had: ">>
                        , clj_core:type(FQNameSym)
                        ]
                      , clj_env:location(Env)
                      ),

  %% Refer the name of the protocol (without the namespace) in the
  %% current namespace.
  Name     = clj_core:name(FQNameSym),
  LastName = lists:last(binary:split(Name, <<".">>, [global])),
  NameSym  = clj_core:symbol(LastName),

  clj_namespace:refer(clj_namespace:current(), NameSym, FQNameSym),

  ProtocolExpr = #{ op           => defprotocol
                  , env          => ?DEBUG(Env)
                  , name         => FQNameSym
                  , methods_sigs => MethodsSigs
                  },

  clj_env:push_expr(Env, ProtocolExpr).

%%------------------------------------------------------------------------------
%% Parse extend-type
%%------------------------------------------------------------------------------

-spec parse_extend_type(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_extend_type(Env, List) ->
  [ _ExtendTypeSym % extend-type*
  , Type0
  | ProtosMethods
  ] = clj_core:to_list(List),

  %% Group each protocol name with its implementation functions.
  GroupedProtoMethods = split_when(ProtosMethods, fun clj_core:'symbol?'/1),

  %% Analyze each group and map protocols to their implementations.
  {ProtoImplsMap, Env1} = lists:foldl( fun analyze_extend_methods/2
                                     , {#{}, Env}
                                     , GroupedProtoMethods
                                     ),

  Type = case Type0 of
           ?NIL -> clj_core:symbol(atom_to_binary(?NIL_TYPE, utf8));
           _    -> Type0
         end,
  {TypeExpr, Env2} = clj_env:pop_expr(analyze_form(Env1, Type)),

  #{op := TypeOp} = TypeExpr,
  clj_utils:error_when( TypeOp =/= type
                      , [ <<"The expression of type ">>
                        , clj_core:type(Type)
                        , <<" does not resolve to a type.">>
                        ]
                      , clj_env:location(Env)
                      ),

  ExtendTypeExpr = #{ op    => extend_type
                    , env   => ?DEBUG(Env)
                    , type  => TypeExpr
                    , form  => List
                    , impls => ProtoImplsMap
                    },

  clj_env:push_expr(Env2, ExtendTypeExpr).

%% @doc Returns a list of lists where the first element of
%%      each sublist is the protocol and the rest are
%%      implementation methods.
-spec split_when(list(), fun((any()) -> boolean())) -> list().
split_when(List, Pred) ->
  SplitFun = fun
                   (X, []) -> [[X]];
                   (X, [First | Rest] = All) ->
                     case Pred(X) of
                       true  -> [[X] | All];
                       false -> [[X | First] | Rest]
                     end
                 end,
  lists:map( fun lists:reverse/1
           , lists:foldl(SplitFun, [], List)
           ).

-spec analyze_extend_methods(list(), {map(), clj_env:env()}) ->
  {map(), clj_env:env()}.
analyze_extend_methods([Proto | Methods], {ImplMapAcc, EnvAcc}) ->
  {ProtoExpr, EnvAcc1} = clj_env:pop_expr(analyze_form(EnvAcc, Proto)),

  #{op := ProtoOp} = ProtoExpr,
  clj_utils:error_when( ProtoOp =/= type
                      , [ <<"The symbol ">>
                        , Proto
                        , <<"does not resolve to a protocol">>
                        ]
                      , clj_env:location(EnvAcc)
                      ),

  EnvAcc2 = lists:foldl(fun analyze_deftype_method/2, EnvAcc1, Methods),
  {MethodsExprs, EnvAcc3} = clj_env:last_exprs(EnvAcc2, length(Methods)),

  {ImplMapAcc#{ProtoExpr => MethodsExprs}, EnvAcc3}.

%%------------------------------------------------------------------------------
%% Parse throw
%%------------------------------------------------------------------------------

-spec parse_throw(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_throw(Env, List) ->
  Count = clj_core:count(List),
  clj_utils:error_when( Count =/= 2
                      , [ <<"Wrong number of args to throw, had: ">>
                        , Count - 1
                        ]
                      , clj_env:location(Env)
                      ),

  Second = clj_core:second(List),
  ExceptionEnv = clj_env:context(Env, expr),
  {ExceptionExpr, Env1} = clj_env:pop_expr(analyze_form(ExceptionEnv, Second)),

  ThrowExpr = #{ op        => throw
               , env       => ?DEBUG(Env)
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

  { Body
  , CatchFinallyTail
  } = lists:splitwith( IsNotCatchFinally
                     , clj_core:to_list(clj_core:rest(List))
                     ),
  {Catches, FinallyTail}   = lists:splitwith(IsCatch, CatchFinallyTail),
  {Finallies, Tail}        = lists:splitwith(IsFinally, FinallyTail),

  clj_utils:error_when( Tail =/= []
                      , <<"Only catch or finally clause can follow catch in "
                          "try expression">>
                      , clj_env:location(Env)
                      ),

  clj_utils:error_when( length(Finallies) > 1
                      , <<"Only one finally clause allowed in try expression">>
                      , clj_env:location(Env)
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
                            {?NIL, Env4}
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
  Body    = lists:sublist(clj_core:to_list(List), 4, clj_core:count(List)),

  clj_utils:error_when( not is_valid_bind_symbol(ErrName)
                      , [<<"Bad binding form: ">>, ErrName]
                      , clj_env:location(Env)
                      ),

  clj_utils:error_when( not is_valid_error_type(ErrType)
                      , [<<"Bad error type: ">>, ErrType]
                      , clj_env:location(Env)
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

-spec is_valid_error_type(any()) -> boolean().
is_valid_error_type(error) -> true;
is_valid_error_type(exit)  -> true;
is_valid_error_type(throw) -> true;
is_valid_error_type(Form)  ->
  clj_core:'symbol?'(Form) andalso clj_core:str(Form) =:= <<"_">>.

%%------------------------------------------------------------------------------
%% Parse var
%%------------------------------------------------------------------------------

-spec parse_var(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_var(Env, List) ->
  Count = clj_core:count(List),
  clj_utils:error_when( Count =/= 2
                      , [<<"Wrong number of args to var, had: ">>, Count]
                      , clj_env:location(Env)
                      ),

  VarSymbol = clj_core:second(List),

  case resolve(Env, VarSymbol, false) of
    {{var, Var}, Env1} ->
      VarConstExpr = #{ op   => constant
                      , env  => ?DEBUG(Env)
                      , tag  => clj_core:type(Var)
                      , form => Var
                      },
      clj_env:push_expr(Env1, VarConstExpr);
    {?NIL, _} ->
      clj_utils:error([ <<"Unable to resolve var: ">>
                      , VarSymbol
                      , <<" in this context">>
                      ]
                     , clj_env:location(Env)
                     )
  end.

%%------------------------------------------------------------------------------
%% Analyze invoke
%%------------------------------------------------------------------------------

-spec analyze_invoke(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
analyze_invoke(Env, Form) ->
  Env1 = analyze_form(Env, clj_core:first(Form)),

  Args = clj_core:rest(Form),
  Env2 = analyze_forms(Env1, clj_core:to_list(Args)),

  ArgCount = clj_core:count(Args),
  {ArgsExpr, Env3} = clj_env:last_exprs(Env2, ArgCount),
  {FExpr, Env4} = clj_env:pop_expr(Env3),

  InvokeExpr = #{ op   => invoke
                , env  => ?DEBUG(Env4)
                , form => Form
                , f    => FExpr#{arity => ArgCount}
                , args => ArgsExpr
                },
  clj_env:push_expr(Env4, InvokeExpr).

%%------------------------------------------------------------------------------
%% Analyze symbol
%%------------------------------------------------------------------------------

-spec analyze_symbol(clj_env:env(), 'clojerl.Symbol':type()) -> clj_env:env().
analyze_symbol(Env, Symbol) ->
  case resolve(Env, Symbol) of
    {?NIL, _} ->
      clj_utils:error([ <<"Unable to resolve symbol '">>, Symbol
                      , <<"' in this context">>
                      ]
                     , clj_env:get(Env, location)
                     );
    {{local, Local}, Env1} ->
      clj_env:push_expr(Env1, Local#{op => local, env => Env});
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
      clj_env:push_expr(Env1, VarExpr);
    {{type, Type}, Env1} ->
      TypeExpr = type_expr(Type, Symbol, Env1),
      clj_env:push_expr(Env1, TypeExpr)
  end.

-spec var_expr('clojerl.Var':type(), 'clojerl.Symbol':type(), clj_env:env()) ->
  map().
var_expr(Var, Symbol, Env) ->
 #{ op   => var
  , env  => ?DEBUG(Env)
  , form => Symbol
  , name => Symbol
  , var  => Var
  }.

-spec type_expr('clojerl.Symbol':type()
               , 'clojerl.Symbol':type()
               , clj_env:env()
               ) ->
  map().
type_expr(Type, Symbol, Env) ->
 #{ op   => type
  , env  => ?DEBUG(Env)
  , form => Symbol
  , type => Type
  }.

-type erl_fun() ::  {erl_fun, module(), atom(), integer()}.

-spec resolve(clj_env:env(), 'clojerl.Symbol':env()) ->
  { {var, 'clojerl.Var':type()} | erl_fun() | {local, map()} | ?NIL
  , clj_env:env()
  }.
resolve(Env, Symbol) ->
  resolve(Env, Symbol, true).

-spec resolve(clj_env:env(), 'clojerl.Symbol':env(), boolean()) ->
  { {var, 'clojerl.Var':type()} | erl_fun() | {local, map()} | ?NIL
  , clj_env:env()
  }.
resolve(Env, Symbol, CheckPrivate) ->
  CurrentNs = clj_namespace:current(),
  Local     = clj_env:get_local(Env, Symbol),
  NsStr     = clj_core:namespace(Symbol),
  MappedVal = clj_namespace:mapping(CurrentNs, Symbol),

  if
    Local =/= ?NIL ->
      {{local, Local}, Env};
    NsStr =/= ?NIL ->
      case clj_namespace:find_var(Symbol) of
        ?NIL ->
          %% If there is no var then assume it's a Module:Function pair.
          %% Let's see how this works out.
          {erl_fun(Env, Symbol), Env};
        Var ->
          CurrentNsName = clj_core:name(clj_namespace:name(CurrentNs)),
          clj_utils:error_when( CheckPrivate
                                andalso NsStr =/= CurrentNsName
                                andalso not 'clojerl.Var':is_public(Var)
                              , [Var, <<" is not public">>]
                              , clj_env:location(Env)
                              ),
          {{var, Var}, Env}
      end;
    MappedVal =/= ?NIL ->
      case clj_core:'var?'(MappedVal) of
        true  -> {{var, MappedVal}, Env};
        false -> {{type, MappedVal}, Env}
      end;
    true ->
      case is_maybe_type(Symbol) of
        true  -> {{type, Symbol}, Env};
        false -> {?NIL, Env}
      end
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
                     , clj_env:location(Env)
                     ),

  Arity1 = case Arity of
             _ when is_integer(Arity) -> Arity;
             _ -> ?NIL
           end,

  {erl_fun, NsAtom, NameAtom, Arity1}.

-spec erl_fun_arity(binary()) -> {binary(), ?NIL | integer()}.
erl_fun_arity(Name) ->
  case binary:split(Name, <<".">>, [global]) of
    [_] -> {Name, ?NIL};
    Parts ->
      Last = lists:last(Parts),
      case {re:run(Last, <<"\\d+">>), Last} of
        {nomatch, <<"e">>} when length(Parts) > 1 ->
          {iolist_to_binary(lists:droplast(Parts)), <<"e">>};
        {nomatch, _} ->
          {Name, ?NIL};
        _ ->
          NameParts = 'clojerl.String':join(lists:droplast(Parts), <<".">>),
          Arity = binary_to_integer(Last),
          {iolist_to_binary(NameParts), Arity}
      end
  end.

-spec is_maybe_type('clojerl.Symbol':type()) -> boolean().
is_maybe_type(Symbol) ->
  ?NIL = clj_core:namespace(Symbol),
  Name = clj_core:name(Symbol),
  Re   = <<"([a-z]\\w*\\.)+[A-Z]\\w*">>,
  case re:run(Name, Re, [global, {capture, none}]) of
    match ->
      Module = binary_to_atom(Name, utf8),
      %% Check if the module is either present in clj_module or a compiled
      %% Erlang module.
      clj_module:is_loaded(Module) orelse
        {module, Module} =:= code:ensure_loaded(Module);
    _ ->
      false
  end.

%%------------------------------------------------------------------------------
%% Helper for wrappping expressions with a with-meta if they have any metadata
%%------------------------------------------------------------------------------

-spec wrapping_meta(clj_env:env(), map()) -> clj_env:env().
wrapping_meta(Env, #{form := Form} = Expr) ->
  case clj_reader:remove_location(clj_core:meta(Form)) of
    Meta when Meta =/= ?NIL andalso Meta =/= #{} ->
      {MetaExpr, Env1} = clj_env:pop_expr(analyze_form(Env, Meta)),

      WithMetaExpr = #{ op   => with_meta
                      , env  => ?DEBUG(Env)
                      , form => Form
                      , meta => MetaExpr
                      , expr => Expr
                      },
      clj_env:push_expr(Env1, WithMetaExpr);
    _ ->
      clj_env:push_expr(Env, Expr)
  end.

%%------------------------------------------------------------------------------
%% Analyze vector
%%------------------------------------------------------------------------------

-spec analyze_vector(clj_env:env(), 'clojerl.Vector':type()) -> clj_env:env().
analyze_vector(Env, Vector) ->
  Count = clj_core:count(Vector),
  ExprEnv = clj_env:context(Env, expr),
  Items = clj_core:to_list(Vector),
  Env1 = analyze_forms(ExprEnv, Items),
  {ItemsExpr, Env2} = clj_env:last_exprs(Env1, Count),

  VectorExpr = #{ op    => vector
                , env   => ?DEBUG(Env2)
                , form  => Vector
                , items => ItemsExpr
                },

  wrapping_meta(Env2, VectorExpr).

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

  wrapping_meta(Env4, MapExpr).

%%------------------------------------------------------------------------------
%% Analyze set
%%------------------------------------------------------------------------------

-spec analyze_set(clj_env:env(), 'clojerl.Set':type()) -> clj_env:env().
analyze_set(Env, Set) ->
  ExprEnv = clj_env:context(Env, expr),
  Items = clj_core:to_list(Set),
  Env1 = analyze_forms(ExprEnv, Items),

  Count = clj_core:count(Set),
  {ItemsExpr, Env2} = clj_env:last_exprs(Env1, Count),

  SetExpr = #{ op    => set
             , env   => ?DEBUG(Env2)
             , form  => Set
             , items => ItemsExpr
             },

  wrapping_meta(Env2, SetExpr).

%%------------------------------------------------------------------------------
%% Analyze tuple
%%------------------------------------------------------------------------------

-spec analyze_tuple(clj_env:env(), 'clojerl.erlang.Tuple':type()) ->
  clj_env:env().
analyze_tuple(Env, Tuple) ->
  ExprEnv = clj_env:context(Env, expr),
  Items = erlang:tuple_to_list(Tuple),
  Env1 = analyze_forms(ExprEnv, Items),

  Count = erlang:tuple_size(Tuple),
  {ItemsExpr, Env2} = clj_env:last_exprs(Env1, Count),

  TupleExpr = #{ op    => tuple
               , env   => ?DEBUG(Env2)
               , form  => Tuple
               , items => ItemsExpr
               },

  clj_env:push_expr(Env2, TupleExpr).

%%------------------------------------------------------------------------------
%% On load
%%------------------------------------------------------------------------------

-spec parse_on_load(clj_env:env(), any()) ->
  clj_env:env().
parse_on_load(Env0, List) ->
  Body            = clj_core:rest(List),
  {BodyExpr, Env} = clj_env:pop_expr(analyze_body(Env0, Body)),

  Expr = #{ op    => on_load
          , env   => ?DEBUG(Env0)
          , form  => List
          , body  => BodyExpr
          },

  clj_env:push_expr(Env, Expr).
