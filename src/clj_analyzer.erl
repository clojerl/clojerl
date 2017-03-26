-module(clj_analyzer).

-include("clojerl.hrl").

-export([ analyze/2
        , macroexpand_1/2
        , is_special/1
        ]).

-spec analyze(clj_env:env(), any()) -> clj_env:env().
analyze(Form, Env0) ->
  {Expr, Env} =  clj_env:pop_expr(analyze_form(Form, Env0)),
  clj_env:push_expr(Expr#{top_level => true}, Env).

-spec is_special('clojerl.Symbol':type()) -> boolean().
is_special(S) ->
  clj_core:'symbol?'(S) andalso
    maps:is_key(clj_core:str(S), special_forms()).

-spec macroexpand_1(clj_env:env(), any()) -> any().
macroexpand_1(Form, Env) ->
  Op        = clj_core:first(Form),
  IsSymbol  = clj_core:'symbol?'(Op),
  IsSpecial = is_special(Op),
  MacroVar  = case IsSymbol of
                true -> lookup_var(Op, false);
                false -> ?NIL
              end,

  case
    not IsSpecial
    andalso (MacroVar =/= ?NIL)
    andalso ('clojerl.Var':is_macro(MacroVar))
  of
    true ->
      Args = [Form, Env | clj_core:to_list(clj_core:rest(Form))],
      'clojerl.IFn':apply(MacroVar, Args);
    false ->
      case IsSymbol andalso not IsSpecial of
        true  -> maybe_macroexpand_symbol(Form, Op);
        false -> Form
      end
  end.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec maybe_macroexpand_symbol(any(), 'clojerl.Symbol':type()) -> any().
maybe_macroexpand_symbol(Form, OpSym) ->
  OpBin = clj_core:name(OpSym),
  case 'clojerl.String':char_at(OpBin, 0) of
    <<".">> ->
      DotSym     = clj_core:symbol(<<".">>),
      Length     = 'clojerl.String':count(OpBin),
      NameBin    = 'clojerl.String':substring(OpBin, 1, Length),
      NameSym    = clj_core:symbol(NameBin),
      [_, Target | Args] = clj_core:to_list(Form),

      clj_core:list([DotSym, Target, NameSym | Args]);
    _ ->
      case 'clojerl.String':ends_with(OpBin, <<".">>) of
        true ->
          NewSym  = clj_core:symbol(<<"new">>),
          Length  = 'clojerl.String':count(OpBin),
          NameBin = 'clojerl.String':substring(OpBin, 0, Length - 1),
          NameSym = clj_core:symbol(NameBin),
          [_ | Args]  = clj_core:to_list(Form),

          clj_core:list([NewSym, NameSym | Args]);
        false ->
          Form
      end
  end.

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

   , <<"receive*">>     => fun parse_receive/2
   , <<"erl-binary*">>  => fun parse_erlang_binary/2
   , <<"erl-list*">>    => fun parse_erlang_list/2
   , <<"erl-alias*">>   => fun parse_erlang_alias/2
   , <<"erl-on-load*">> => fun parse_on_load/2

   , <<"import*">>      => fun parse_import/2
   , <<"new">>          => fun parse_new/2
   , <<"deftype*">>     => fun parse_deftype/2
   , <<"defprotocol*">> => fun parse_defprotocol/2
   , <<"extend-type*">> => fun parse_extend_type/2

   , <<".">>            => fun parse_dot/2

     %% These are special forms but they are meant to be parsed under other
     %% special forms. If they are at function position then they should be
     %% analyzed as regular symbols.
   , <<"catch">>        => fun analyze_invoke/2
   , <<"finally">>      => fun analyze_invoke/2
   , <<"&">>            => fun analyze_invoke/2
   }.

-spec analyze_forms([any()], clj_env:env()) -> clj_env:env().
analyze_forms(Forms, Env) ->
  lists:foldl(fun analyze_form/2, Env, Forms).

-spec analyze_form(any(), clj_env:env()) -> clj_env:env().
analyze_form(Form, Env) ->
  IsSeq    = clj_core:'seq?'(Form),
  IsSymbol = clj_core:'symbol?'(Form),
  IsRecord = clj_core:'record?'(Form),
  IsType   = clj_core:'type?'(Form),
  IsVector = clj_core:'vector?'(Form),
  IsMap    = clj_core:'map?'(Form),
  IsErlMap = is_map(Form),
  IsSet    = clj_core:'set?'(Form),
  IsVar    = clj_core:'var?'(Form),
  IsTuple  = erlang:is_tuple(Form),
  if
    IsSeq ->
      case clj_core:'empty?'(Form) of
        true  -> analyze_const(Form, Env);
        false -> analyze_seq(Form, Env)
      end;
    IsSymbol ->
      analyze_symbol(Form, Env);
    IsRecord ->
      analyze_const(Form, Env);
    IsType ->
      analyze_const(Form, Env);
    IsVector ->
      analyze_vector(Form, Env);
    IsErlMap ->
      analyze_erl_map(Form, Env);
    IsMap ->
      analyze_map(Form, Env);
    IsSet ->
      analyze_set(Form, Env);
    IsTuple andalso element(1, Form) =/= ?TYPE ->
      analyze_tuple(Form, Env);
    IsVar ->
      %% The var's metadata should already have been analyzed
      analyze_const(Form, _CheckWrappingMeta = false, Env);
    true ->
      analyze_const(Form, Env)
  end.

%%------------------------------------------------------------------------------
%% Analyze const
%%------------------------------------------------------------------------------

-spec analyze_const(any(), clj_env:env()) -> clj_env:env().
analyze_const(Constant, Env) ->
  analyze_const(Constant, true, Env).

-spec analyze_const(any(), boolean(), clj_env:env()) -> clj_env:env().
analyze_const(Constant, CheckWrappingMeta, Env) ->
  Expr = #{ op   => constant
          , env  => Env
          , tag  => clj_core:type(Constant)
          , form => Constant
          },
  case CheckWrappingMeta of
    true  -> wrapping_meta(Expr, Env);
    false -> clj_env:push_expr(Expr, Env)
  end.

%%------------------------------------------------------------------------------
%% Analyze seq
%%------------------------------------------------------------------------------

-spec analyze_seq('clojerl.List':type(), clj_env:env()) -> clj_env:env().
analyze_seq(List, Env0) ->
  OldLocation   = clj_env:location(Env0),
  MaybeLocation = clj_reader:location_meta(List),
  Env           = clj_env:maybe_update_location(MaybeLocation, Env0),
  Op            = clj_core:first(List),

  clj_utils:error_when( Op =:= ?NIL
                      , <<"Can't call nil">>
                      , clj_env:location(Env)
                      ),

  ExpandedList = macroexpand_1(List, Env),
  Env1 = case clj_core:equiv(List, ExpandedList) of
           true ->
             AnaInvoke = fun analyze_invoke/2,
             Fun = case clj_core:'symbol?'(Op) of
                     true ->
                       maps:get(clj_core:name(Op), special_forms(), AnaInvoke);
                     false ->
                       AnaInvoke
                   end,
             Fun(List, Env);
           false ->
             analyze_form(ExpandedList, Env)
         end,
  clj_env:location(OldLocation, Env1).

%%------------------------------------------------------------------------------
%% Parse quote
%%------------------------------------------------------------------------------

-spec parse_quote('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_quote(List, Env) ->
  Count = clj_core:count(List),
  clj_utils:error_when( Count =/= 2
                      , [<<"Wrong number of args to quote, had: ">>, Count - 1]
                      , clj_env:location(Env)
                      ),

  Second = clj_core:second(List),

  { #{tag := Tag} = ConstExpr
  , NewEnv
  } = clj_env:pop_expr(analyze_const(Second, Env)),

  Expr = #{ op   => quote
          , env  => Env
          , expr => ConstExpr
          , tag  => Tag
          , form => List
          },
  clj_env:push_expr(Expr, NewEnv).

%%------------------------------------------------------------------------------
%% Parse fn*
%%------------------------------------------------------------------------------

-spec parse_fn('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_fn(List, Env) ->
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

  %% Add the name of the fn as a local binding
  LocalExpr     = #{ op   => local
                   , env  => Env
                   , name => NameSym
                   , tag  => maybe_type_tag(NameSym)
                   },

  %% If there is a def var we add it to the local scope
  DefNameSym  = clj_env:get(def_name, Env),
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
             clj_env:put_local(NameSym, LocalExpr, Env0)
         end,

  OpMeta      = clj_core:meta(Op),
  OnceKeyword = clj_core:keyword(<<"once">>),
  IsOnce      = clj_core:boolean(clj_core:get(OpMeta, OnceKeyword)),

  %% If this is a var fn then the loop-id should be the function and not
  %% the variable for the named fun.
  LoopId = case clj_env:get(def_name, Env) of
             ?NIL -> {fn, NameSym};
             _    -> {var, DefNameSym}
           end,

  %% Remove def_name so that inner fn* are not influenced by it.
  Env1Bis = clj_env:push(#{def_name => ?NIL}, Env1),

  %% If it is a def analyze methods' args but not the body,
  %% we just want arity information first.
  {MethodsExprs, Env2} =
    analyze_fn_methods(MethodsList, LoopId, IsOnce, not IsDef, Env1Bis),

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

  %% DistinctFixedArities = lists:usort(FixedArities),
  %% clj_utils:error_when( length(DistinctFixedArities) =/= length(FixedArities)
  %%                     , <<"Can't have 2 or more overloads "
  %%                         "with the same arity">>
  %%                     , clj_env:location(Env)
  %%                     ),

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

        analyze_fn_methods(MethodsList, LoopId, IsOnce, true, Env2);
      false ->
        {MethodsExprs, Env2}
    end,

  FnExpr = #{ op              => fn
            , env             => Env
            , form            => List
            , tag             => maybe_type_tag(NameSym)
            , 'variadic?'     => IsVariadic
            , max_fixed_arity => MaxFixedArity
            , variadic_arity  => VariadicArity
            , methods         => MethodsExprs1
            , once            => IsOnce
            , local           => LocalExpr
            },

  Env5 = clj_env:remove_locals_scope(Env4),
  Env6 = clj_env:pop(Env5),
  clj_env:push_expr(FnExpr, Env6).

-spec analyze_fn_methods( ['clojerl.List':type()]
                        , 'clojerl.Symbol':type()
                        , boolean()
                        , boolean()
                        , clj_env:env()
                        ) ->
 {[map()], clj_env:env()}.
analyze_fn_methods(MethodsList, LoopId, IsOnce, AnalyzeBody, Env) ->
  MethodEnv = clj_env:put(once, IsOnce, clj_env:put(in_try, false, Env)),
  AnalyzeFnMethodFun = fun(M, EnvAcc) ->
                           analyze_fn_method(M, LoopId, AnalyzeBody, EnvAcc)
                       end,

  Env1 = lists:foldl(AnalyzeFnMethodFun, MethodEnv, MethodsList),
  clj_env:last_exprs(length(MethodsList), Env1).

-spec analyze_fn_method( 'clojerl.List':type()
                       , 'clojerl.Symbol':type()
                       , boolean()
                       , clj_env:env()
                       ) ->
  clj_env:env().
analyze_fn_method(List, LoopId, AnalyzeBody, Env0) ->
  Params = clj_core:first(List),
  clj_utils:error_when( not clj_core:'vector?'(Params)
                      , <<"Parameter declaration should be a vector">>
                      , clj_env:location(Env0)
                      ),

  ParamsList = clj_core:to_list(Params),

  IsNotAmpersandFun = fun(X) -> clj_core:str(X) =/= <<"&">> end,
  ParamsOnly        = lists:filter(IsNotAmpersandFun, ParamsList),
  IsVariadic        = length(ParamsOnly) =/= length(ParamsList),

  Env1   = clj_env:add_locals_scope(Env0),
  Env2   = clj_env:push(#{pattern_locals => []}, Env1),
  Arity  = length(ParamsOnly),

  Env3  = analyze_method_params(IsVariadic, Arity, ParamsOnly, Env2),
  {ParamsExprs, Env4} = clj_env:last_exprs(Arity, Env3),

  FixedArity  = case IsVariadic of true -> Arity - 1; false -> Arity end,

  {_, Guard, Body} = case AnalyzeBody of
                       true  -> check_guard(clj_core:rest(List));
                       false -> {false, true, List}
                     end,

  {BodyExpr, Env5} =
    case AnalyzeBody of
      true ->
        LocalExprs = clj_env:get(pattern_locals, [], Env4),
        BodyEnv0   = clj_env:put_locals(lists:reverse(LocalExprs), Env4),
        BodyEnv1   = clj_env:context(return, BodyEnv0),
        BodyEnv2   = clj_env:put(loop_id, LoopId, BodyEnv1),
        BodyEnv3   = clj_env:put(loop_locals, length(ParamsExprs), BodyEnv2),
        clj_env:pop_expr(analyze_body(Body, BodyEnv3));
      false ->
        {?NIL, Env4}
    end,

  {GuardExpr, Env6} =
    case AnalyzeBody of
      true  -> clj_env:pop_expr(analyze_form(Guard, Env5));
      false -> clj_env:pop_expr(analyze_const(true, Env5))
    end,

  %% TODO: check for a single symbol after '&

  FnMethodExpr = #{ op          => fn_method
                  , env         => Env0
                  , form        => List
                  , loop_id     => LoopId
                  , 'variadic?' => IsVariadic
                  , params      => ParamsExprs
                  , guard       => GuardExpr
                  , fixed_arity => FixedArity
                  , body        => BodyExpr
                  },

  Env7  = clj_env:remove_locals_scope(Env6),
  Env8  = clj_env:pop(Env7),
  clj_env:push_expr(FnMethodExpr, Env8).

-spec analyze_method_params(['clojerl.Symbol':type()], clj_env:env()) ->
  [any()].
analyze_method_params(ParamsNames, Env) ->
  analyze_method_params(false, -1, ParamsNames, Env).

-spec analyze_method_params( boolean()
                           , non_neg_integer()
                           , ['clojerl.Symbol':type()]
                           , clj_env:env()
                           ) -> {[any()], clj_env:env()}.
analyze_method_params(IsVariadic, Arity, Params, Env0) ->
  ParamExprFun =
    fun(Pattern, {Id, EnvAcc0}) ->
        {PatExpr, EnvAcc1} = clj_env:pop_expr(parse_pattern(Pattern, EnvAcc0)),
        ParamExpr = #{ op          => binding
                     , env         => Env0
                     , pattern     => PatExpr
                     , 'variadic?' => IsVariadic andalso Id == Arity - 1
                     , arg_id      => Id
                     , local       => arg
                      },
        {Id + 1, clj_env:push_expr(ParamExpr, EnvAcc1)}
    end,

  {_, Env1} = lists:foldl(ParamExprFun, {0, Env0}, Params),
  Env1.

-spec analyze_body('clojerl.List':type(), clj_env:env()) -> clj_env:env().
analyze_body(List, Env) ->
  DoSym = clj_core:symbol(<<"do">>),
  DoForm = clj_core:cons(DoSym, List),
  analyze_form(DoForm, Env).

-spec is_valid_bind_symbol(any()) -> boolean().
is_valid_bind_symbol(X) ->
  clj_core:'symbol?'(X)
    andalso not clj_core:boolean(clj_core:namespace(X))
    andalso nomatch == re:run(clj_core:name(X), <<"\\.">>).

-spec check_guard(any()) -> {boolean(), any(), any()}.
check_guard(List) ->
  Form = clj_core:first(List),
  case clj_core:'map?'(Form) of
    true ->
      Ref = make_ref(),
      case clj_core:get(Form, 'when', Ref) of
        Ref   -> {false, true, List};
        Guard -> {true, Guard, clj_core:rest(List)}
      end;
    false ->
      {false, true, List}
  end.

%%------------------------------------------------------------------------------
%% Parse do
%%------------------------------------------------------------------------------

-spec parse_do('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_do(Form, Env) ->
  StmtEnv = clj_env:context(statement, Env),
  Statements = clj_core:to_list(clj_core:rest(Form)),

  {StatementsList, Return} =
    case Statements of
      [] -> {[], ?NIL};
      _  -> {lists:droplast(Statements), lists:last(Statements)}
    end,

  Env1 = analyze_forms(StatementsList, StmtEnv),
  {StatementsExprs, Env2} =
    clj_env:last_exprs(length(StatementsList), Env1),

  ReturnEnv = clj_env:context(return, Env2),
  {ReturnExpr, Env3} = clj_env:pop_expr(analyze_form(Return, ReturnEnv)),

  DoExpr = #{ op         => do
            , env        => Env
            , form       => Statements
            , statements => StatementsExprs
            , ret        => ReturnExpr
            },

  clj_env:push_expr(DoExpr, Env3).

%%------------------------------------------------------------------------------
%% Parse if
%%------------------------------------------------------------------------------

-spec parse_if('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_if(Form, Env) ->
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

  TestEnv = clj_env:context(expr, Env),
  {TestExpr, Env1} = clj_env:pop_expr(analyze_form(Test, TestEnv)),
  Env2 = analyze_forms([Then, Else], Env1),
  {[ThenExpr, ElseExpr], Env3} = clj_env:last_exprs(2, Env2),

  IfExpr = #{ op   => 'if'
            , form => Form
            , env  => Env
            , test => TestExpr
            , then => ThenExpr
            , else => ElseExpr
            },

  clj_env:push_expr(IfExpr, Env3).

%%------------------------------------------------------------------------------
%% Parse let & parse loop
%%------------------------------------------------------------------------------

-spec parse_let('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_let(Form, Env) ->
  {LetExprExtra, Env1} = analyze_let(Form, Env),
  LetExpr = maps:merge(#{ op   => 'let'
                        , form => Form
                        , env  => Env
                        },
                       LetExprExtra),

  clj_env:push_expr(LetExpr, Env1).

-spec parse_loop('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_loop(Form, Env) ->
  LoopId = clj_core:gensym(<<"loop_">>),
  Env1   = clj_env:push(#{loop_id => {loop, LoopId}}, Env),

  {LoopExprExtra, Env2} = analyze_let(Form, Env1),
  LoopExpr = maps:merge(#{ op      => loop
                         , form    => Form
                         , env     => Env
                         , loop_id => LoopId
                         },
                        LoopExprExtra),

  clj_env:push_expr(LoopExpr, clj_env:pop(Env2)).

-spec analyze_let('clojerl.List':type(), clj_env:env()) -> clj_env:env().
analyze_let(Form, Env0) ->
  validate_bindings(Form, Env0),
  Op     = clj_core:first(Form),
  IsLoop = clj_core:equiv(clj_core:symbol(<<"loop*">>), Op),

  PairUp = fun
             PairUp([], Pairs) ->
               lists:reverse(Pairs);
             PairUp([X, Y | Tail], Pairs) ->
               PairUp(Tail, [{X, Y} | Pairs])
           end,
  BindingsVec   = clj_core:second(Form),
  BindingsList  = clj_core:to_list(BindingsVec),
  BindingPairs  = PairUp(BindingsList, []),

  Env1          = clj_env:add_locals_scope(Env0),
  Env1a         = clj_env:push(#{pattern_locals => []}, Env1),

  Env2 = lists:foldl( fun parse_binding/2
                    , clj_env:put(is_loop, IsLoop, Env1a)
                    , BindingPairs
                    ),
  BindingCount = length(BindingPairs),
  {BindingsExprs, Env3} = clj_env:last_exprs(BindingCount, Env2),

  BodyEnv = case IsLoop of
              true ->
                EnvTemp = clj_env:context(return, Env3),
                clj_env:put(loop_locals, BindingCount, EnvTemp);
              false ->
                Env3
            end,

  Body = clj_core:rest(clj_core:rest(Form)),
  {BodyExpr, Env4} = clj_env:pop_expr(analyze_body(Body, BodyEnv)),

  LetExprExtra = #{ body     => BodyExpr
                  , bindings => BindingsExprs
                  },

  Env5 = clj_env:remove_locals_scope(Env4),
  Env6 = clj_env:pop(Env5),
  {LetExprExtra, Env6}.

-spec parse_binding({any(), any()}, clj_env:env()) -> clj_env:env().
parse_binding({Pattern, Init}, Env0) ->
  OpAtom       = case clj_env:get(is_loop, Env0) of
                   true  -> loop;
                   false -> 'let'
                 end,

  {PatternExpr, Env1} = clj_env:pop_expr(parse_pattern(Pattern, Env0)),
  {InitExpr, Env2}    = clj_env:pop_expr(analyze_form(Init, Env1)),

  BindExpr = #{ op      => binding
              , env     => Env0
              , pattern => PatternExpr
              , init    => InitExpr
              , form    => Pattern
              , local   => OpAtom
              },

  LocalExprs = clj_env:get(pattern_locals, [], Env2),
  Env3       = clj_env:put_locals(lists:reverse(LocalExprs), Env2),
  clj_env:push_expr(BindExpr, Env3).

-spec validate_bindings('clojerl.List':type(), clj_env:env()) -> ok.
validate_bindings(Form, Env) ->
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

-spec parse_recur('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_recur(List, Env) ->
  {LoopType, LoopId} = clj_env:get(loop_id, Env),
  LoopLocals         = clj_env:get(loop_locals, Env),

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

  Env1 = clj_env:context(expr, Env),
  ArgsList = clj_core:to_list(clj_core:rest(List)),
  Env2 = analyze_forms(ArgsList, Env1),
  {ArgsExprs, Env3} = clj_env:last_exprs(LoopLocals, Env2),

  RecurExpr = #{ op         => recur
               , env        => Env
               , form       => List
               , exprs      => ArgsExprs
               , loop_id    => LoopId
               , loop_type  => LoopType
               },

  clj_env:push_expr(RecurExpr, Env3).

%%------------------------------------------------------------------------------
%% Parse letfn
%%------------------------------------------------------------------------------

-spec parse_letfn('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_letfn(Form, Env0) ->
  [ _ %% letfn*
  , FnSpecs
  | Body
  ] = clj_core:to_list(Form),

  {FnNames, FnForms} = partition_fun_defs(clj_core:to_list(FnSpecs)),

  Env  = clj_env:add_locals_scope(Env0),

  BindingFun = fun(FnName) ->
                   #{ op     => local
                    , env    => Env
                    , name   => FnName
                    , tag    => maybe_type_tag(FnName)
                    , shadow => clj_env:get_local(FnName, Env)
                    , form   => FnName
                    }
               end,
  FnNamesExprs = lists:map(BindingFun, FnNames),

  Env1 = clj_env:put_locals(FnNamesExprs, Env),

  {FnsExprs, Env2} = clj_env:last_exprs( length(FnNames)
                                       , analyze_forms(FnForms, Env1)
                                       ),

  {BodyExpr, Env3} = clj_env:pop_expr(analyze_body(Body, Env2)),

  LetFnExpr = #{ op   => letfn
               , env  => Env
               , form => Form
               , vars => FnNamesExprs
               , fns  => FnsExprs
               , body => BodyExpr
               },

  Env4 = clj_env:remove_locals_scope(Env3),

  clj_env:push_expr(LetFnExpr, Env4).

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

-spec parse_case('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_case(List, Env) ->
  Test             = clj_core:second(List),
  {TestExpr, Env1} = clj_env:pop_expr(analyze_form(Test, Env)),

  PatternsBodies     = clj_core:rest(clj_core:rest(List)),
  PatternsBodiesList = clj_core:to_list(PatternsBodies),

  { ClausesExprs
  , DefaultExpr
  , Env2
  } = parse_patterns_bodies(PatternsBodiesList, Env1),

  CaseExpr = #{ op      => 'case'
              , env     => Env
              , form    => List
              , test    => TestExpr
              , clauses => ClausesExprs
              , default => DefaultExpr
              },

  clj_env:push_expr(CaseExpr, Env2).

-spec parse_patterns_bodies([any()], clj_env:env()) ->
  {list(), any(), clj_env:env()}.
parse_patterns_bodies(PatternsBodies, Env1) ->
  parse_patterns_bodies(PatternsBodies, [], Env1).

-spec parse_patterns_bodies([any()], [any()], clj_env:env()) ->
  {list(), any(), clj_env:env()}.
parse_patterns_bodies([], PatternBodyPairs, Env) ->
  { lists:reverse(PatternBodyPairs)
  , ?NIL
  , Env
  };
parse_patterns_bodies([Default], PatternBodyPairs, Env) ->
  {DefaultExpr, Env1} = clj_env:pop_expr(analyze_form(Default, Env)),

  { lists:reverse(PatternBodyPairs)
  , DefaultExpr
  , Env1
  };
parse_patterns_bodies([Pat, GuardOrBody | Rest0], PatternBodyPairs, Env0) ->
  {IsGuard, Guard, _} = check_guard([GuardOrBody]),
  {Body, Rest} = case IsGuard of
                   true ->
                     [BodyTemp | RestTemp] = Rest0,
                     {BodyTemp, RestTemp};
                   false ->
                     {GuardOrBody, Rest0}
                 end,
  Env1 = clj_env:push(#{pattern_locals => []}, Env0),

  {PatternExpr, Env2} = clj_env:pop_expr(parse_pattern(Pat, Env1)),

  LocalExprs = clj_env:get(pattern_locals, [], Env2),
  Env3       = clj_env:put_locals( lists:reverse(LocalExprs)
                                 , clj_env:add_locals_scope(Env2)
                                 ),

  {GuardExpr, Env4}   = clj_env:pop_expr(analyze_form(Guard, Env3)),
  {BodyExpr, Env5}    = clj_env:pop_expr(analyze_form(Body, Env4)),

  Env6                = clj_env:pop(clj_env:remove_locals_scope(Env5)),

  parse_patterns_bodies( Rest
                       , [{ PatternExpr#{guard => GuardExpr}
                          , BodyExpr
                          } | PatternBodyPairs
                         ]
                       , Env6
                       ).

-spec add_pattern_local(any(), clj_env:env()) -> clj_env:env().
add_pattern_local(LocalExpr, Env) ->
  PatternLocals = clj_env:get(pattern_locals, [], Env),
  clj_env:update(pattern_locals, [LocalExpr | PatternLocals], Env).

-spec parse_pattern(any(), clj_env:env()) -> clj_env:env().
parse_pattern(Form, Env) ->
  IsSymbol = clj_core:'symbol?'(Form),
  Mapping  = #{in_pattern => true},
  Env1     = clj_env:push(Mapping, Env),
  Env2     =
    if
      IsSymbol ->
        analyze_symbol(Form, Env1);
      is_map(Form) ->
        Keys  = maps:keys(Form),
        Vals  = maps:values(Form),
        Count = maps:size(Form),

        InnerEnv0 = lists:foldl(fun parse_pattern/2, Env1, Keys ++ Vals),
        {ValsExprs, InnerEnv1} = clj_env:last_exprs(Count, InnerEnv0),
        {KeysExprs, InnerEnv2} = clj_env:last_exprs(Count, InnerEnv1),

        Ast = #{ op      => erl_map
               , env     => Env
               , form    => Form
               , tag     => 'clojerl.erlang.Map'
               , keys    => KeysExprs
               , vals    => ValsExprs
               , pattern => true
               },
        clj_env:push_expr(Ast, InnerEnv2);
      is_tuple(Form), not ?IS_TYPE(Form) ->
        Vals = tuple_to_list(Form),

        InnerEnv0 = lists:foldl(fun parse_pattern/2, Env1, Vals),
        {ValsExprs, InnerEnv1} = clj_env:last_exprs(size(Form), InnerEnv0),

        Ast = #{ op      => tuple
               , env     => Env
               , form    => Form
               , tag     => 'clojerl.erlang.Tuple'
               , items   => ValsExprs
               },
        clj_env:push_expr(Ast, InnerEnv1);
      is_list(Form) ->
        InnerEnv0 = lists:foldl(fun parse_pattern/2, Env1, Form),
        {ValsExprs, InnerEnv1} = clj_env:last_exprs(length(Form), InnerEnv0),

        Ast = #{ op      => erl_list
               , env     => Env
               , form    => Form
               , tag     => 'clojerl.erlang.List'
               , items   => ValsExprs
               },
        clj_env:push_expr(Ast, InnerEnv1);
      is_number(Form);
      is_boolean(Form);
      is_atom(Form);
      is_binary(Form) ->
        analyze_const(Form, Env1);
      true ->
        case
          clj_core:'list?'(Form)
          andalso clj_core:str(clj_core:first(Form))
        of
          X when X =:= <<"erl-binary*">> orelse
                 X =:= <<"erl-list*">> orelse
                 X =:= <<"erl-alias*">> ->
            analyze_form(Form, Env1);
          _ ->
            clj_utils:error( [<<"Invalid pattern: ">>, Form]
                           , clj_env:location(Env)
                           )
        end
    end,
  clj_env:pop(Env2).

%%------------------------------------------------------------------------------
%% Parse def
%%------------------------------------------------------------------------------

-spec parse_def('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_def(List, Env) ->
  Docstring    = validate_def_args(List, Env),
  VarSymbol0   = clj_core:second(List),
  SymbolMeta0  = clj_core:meta(VarSymbol0),
  DocstringMap = case Docstring of
                   ?NIL -> #{};
                   Docstring -> #{doc => Docstring}
                 end,

  LocationInfo = clj_env:location(Env),

  SymbolMeta   = clj_core:merge([ DocstringMap
                                , SymbolMeta0
                                , LocationInfo
                                ]),
  VarSymbol    = clj_core:with_meta(VarSymbol0, SymbolMeta),

  Var0 = lookup_var(VarSymbol),

  clj_utils:error_when( Var0 =:= ?NIL
                      , [ <<"Can't refer to qualified var that "
                            "doesn't exist: ">>
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

  QuoteSym     = clj_core:symbol(<<"quote">>),
  NameBin      = clj_core:name(VarSymbol),
  VarMeta      = clj_core:merge([ clj_core:meta(Var0)
                                , SymbolMeta
                                , #{ ns   => [QuoteSym, VarNsSym]
                                   , name => [ QuoteSym
                                             , clj_core:symbol(NameBin)
                                             ]
                                   }
                                ]),
  Var          = clj_core:with_meta(Var0, VarMeta),
  IsDynamic    = 'clojerl.Var':is_dynamic(Var),

  NoWarnDynamic = clj_compiler:no_warn_dynamic_var_name(Env),
  clj_utils:warn_when( not NoWarnDynamic
                       andalso not IsDynamic
                       andalso nomatch =/= re:run(NameBin, "\\*.+\\*")
                     , [ <<"Warning: ">>
                       , NameBin
                       , <<" is not dynamic but its name"
                           " suggests otherwise.">>
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

  ExprEnv  = clj_env:push(#{def_name => VarSymbol}, clj_env:context(expr, Env)),
  {InitExpr, Env1} = clj_env:pop_expr(analyze_form(Init, ExprEnv)),

  Var1     = var_fn_info(Var, InitExpr),
  VarMeta1 = process_var_meta(Var1, Env),
  Var2     = 'clojerl.Var':with_meta(Var1, VarMeta1),
  clj_namespace:update_var(Var2),

  DefExpr = #{ op      => def
             , env     => Env
             , form    => List
             , name    => VarSymbol
             , tag     => maybe_type_tag(VarSymbol)
             , var     => Var2
             , init    => InitExpr
             , dynamic => IsDynamic
             },

  Env2 = clj_env:pop(Env1),
  clj_env:push_expr(DefExpr, Env2).

process_var_meta(Var, Env) ->
  Env1           = analyze_form(clj_core:meta(Var), Env),
  {Exprs, _Env2} = clj_emitter:emit(Env1),
  VarMeta = clj_compiler:eval_expressions(Exprs, false),

  clj_utils:error_when( not cerl:is_literal_term(VarMeta)
                      , [ <<"Metadata for var ">>
                        , Var
                        , <<" can only contains literals">>
                        ]
                      , clj_env:location(Env)
                      ),

  VarMeta.

-spec var_fn_info('clojerl.Var':type(), map()) -> 'clojerl.Var':type().
var_fn_info(Var, #{op := fn} = Expr) ->
  %% Add information about the associated function
  %% to the var's metadata.
  RemoveKeys = [op, env, methods, form, once, local, tag],
  ExprInfo   = maps:without(RemoveKeys, Expr),
  VarMeta    = clj_core:meta(Var),
  VarMeta1   = clj_core:merge([VarMeta, ExprInfo, #{'fn?' => true}]),
  clj_core:with_meta(Var, VarMeta1);
var_fn_info(Var, _) ->
  Var.

-spec validate_def_args('clojerl.List':type(), clj_env:env()) ->
  ?NIL | binary().
validate_def_args(List, Env) ->
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
      clj_namespace:intern(NameSym, CurrentNs),
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

-spec parse_import('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_import(Form, Env) ->
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
             , env      => Env
             , form     => Form
             , typename => TypeName
             },

  clj_env:push_expr(NewExpr, Env).

%%------------------------------------------------------------------------------
%% Parse new
%%------------------------------------------------------------------------------

-spec parse_new('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_new(Form, Env) ->
  [_, Type | Args] = clj_core:to_list(Form),

  { #{type := TypeSym} = TypeExpr
  , Env1
  } = clj_env:pop_expr(analyze_form(Type, Env)),

  {ArgsExprs, Env2} =
    clj_env:last_exprs(length(Args), analyze_forms(Args, Env1)),

  NewExpr = #{ op   => new
             , env  => Env
             , form => Form
             , tag  => clj_core:keyword(TypeSym)
             , type => TypeExpr
             , args => ArgsExprs
             },

  clj_env:push_expr(NewExpr, Env2).

%%------------------------------------------------------------------------------
%% Parse deftype
%%------------------------------------------------------------------------------

-spec parse_deftype('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_deftype(Form, Env0) ->
  [ _ % deftype*
  , Name
  , TypeSym
  , Fields
  , _ % :implements
  , Interfaces
  | Methods
  ] = clj_core:to_list(Form),

  Env0a       = clj_env:push(#{pattern_locals => []}, Env0),

  FieldsList  = clj_core:to_list(Fields),
  Env1        = analyze_method_params(FieldsList, Env0a),
  {FieldsExprs, Env2} = clj_env:last_exprs(length(FieldsList), Env1),

  %% The analyzer adds the fields to the local scope of the methods,
  %% but it is the emitter who will need to pattern match the first argument
  %% so that they are actually available in the methods' body.
  Env3        = clj_env:add_locals_scope(Env2),
  LocalExprs  = clj_env:get(pattern_locals, [], Env3),
  Env4        = clj_env:put_locals(lists:reverse(LocalExprs), Env3),

  %% HACK: by emitting the type we make the module available, which means the
  %% type gets resolved. But we remove all protocols and methods, thus
  %% generating just a dummy erlang module for the type.
  DeftypeDummyExpr = #{ op        => deftype
                      , env       => Env0
                      , form      => Form
                      , name      => Name
                      , type      => TypeSym
                      , fields    => FieldsExprs
                      , protocols => []
                      , methods   => []
                      },
  _ = clj_emitter:emit(clj_env:push_expr(DeftypeDummyExpr, Env4)),

  Env5 = lists:foldl(fun analyze_deftype_method/2, Env4, Methods),
  {MethodsExprs, Env6} = clj_env:last_exprs(length(Methods), Env5),

  IntfsList   = clj_core:to_list(Interfaces),
  {InterfacesExprs, Env7} = clj_env:last_exprs( length(IntfsList)
                                              , analyze_forms(IntfsList, Env6)
                                              ),

  DeftypeExpr = #{ op        => deftype
                 , env       => Env0
                 , form      => Form
                 , name      => Name
                 , type      => TypeSym
                 , fields    => FieldsExprs
                 , protocols => InterfacesExprs
                 , methods   => MethodsExprs
                 },

  Env8 = clj_env:remove_locals_scope(Env7),
  Env9 = clj_env:pop(Env8),
  clj_env:push_expr(DeftypeExpr, Env9).

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
                      , [ <<"Must supply at least one argument "
                            "for 'this' in: ">>
                        , MethodName
                        ]
                      , clj_env:location(Env)
                      ),

  LoopId = {function, MethodName},
  {MethodExpr, Env1} = clj_env:pop_expr(analyze_fn_method( clj_core:rest(Form)
                                                         , LoopId
                                                         , true
                                                         , Env
                                                         )),

  MethodExpr1 = maps:merge( maps:remove('variadic?', MethodExpr)
                          , #{ op   => fn_method
                             , env  => Env
                             , form => Form
                             , name => MethodName
                             }
                          ),

  clj_env:push_expr(MethodExpr1, Env1).

%%------------------------------------------------------------------------------
%% Parse defprotocol
%%------------------------------------------------------------------------------

-spec parse_defprotocol('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_defprotocol(List, Env) ->
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

  ProtocolExpr = #{ op           => defprotocol
                  , env          => Env
                  , name         => FQNameSym
                  , methods_sigs => MethodsSigs
                  },

  clj_env:push_expr(ProtocolExpr, Env).

%%------------------------------------------------------------------------------
%% Parse extend-type
%%------------------------------------------------------------------------------

-spec parse_extend_type('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_extend_type(List, Env) ->
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
           _    ->
             IsSymbol = clj_core:'symbol?'(Type0),
             case clj_core:str(Type0) of
               <<"default">> when IsSymbol ->
                 clj_core:symbol(atom_to_binary(?DEFAULT_TYPE, utf8));
               _  ->
                 Type0
             end
         end,
  { #{op := TypeOp} = TypeExpr
  , Env2
  } = clj_env:pop_expr(analyze_form(Type, Env1)),

  clj_utils:error_when( TypeOp =/= type
                      , [ <<"The expression of type ">>
                        , clj_core:type(Type)
                        , <<" does not resolve to a type.">>
                        ]
                      , clj_env:location(Env)
                      ),

  ExtendTypeExpr = #{ op    => extend_type
                    , env   => Env
                    , type  => TypeExpr
                    , form  => List
                    , impls => ProtoImplsMap
                    },

  clj_env:push_expr(ExtendTypeExpr, Env2).

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
  { #{op := ProtoOp} = ProtoExpr
  , EnvAcc1
  } = clj_env:pop_expr(analyze_form(Proto, EnvAcc)),

  clj_utils:error_when( ProtoOp =/= type
                      , [ <<"The symbol ">>
                        , Proto
                        , <<"does not resolve to a protocol">>
                        ]
                      , clj_env:location(EnvAcc)
                      ),

  EnvAcc2 = lists:foldl(fun analyze_deftype_method/2, EnvAcc1, Methods),
  {MethodsExprs, EnvAcc3} = clj_env:last_exprs(length(Methods), EnvAcc2),

  {ImplMapAcc#{ProtoExpr => MethodsExprs}, EnvAcc3}.

%%------------------------------------------------------------------------------
%% Parse dot
%%------------------------------------------------------------------------------

-spec parse_dot('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_dot(List, Env) ->
  clj_utils:error_when( clj_core:count(List) < 3
                      , <<"Malformed member expression, expecting "
                          "(. target member ...)">>
                      , clj_env:location(Env)
                      ),

  [_Dot, Target | Args] = clj_core:to_list(List),

  {TargetExpr, Env1} = clj_env:pop_expr(analyze_form(Target, Env)),

  FirstArg = clj_core:first(Args),
  {NameSym, ArgsList} =
    case clj_core:'symbol?'(FirstArg) of
      true  ->
        {FirstArg, clj_core:rest(Args)};
      false ->
        clj_utils:error_when( length(Args) > 1
                              orelse not clj_core:'seq?'(FirstArg)
                            , [ <<"Invalid . expression, expected ">>
                              , <<"single list, got: ">>
                              , FirstArg
                              , <<"and: ">>
                              , length(Args)
                              , <<" arguments">>
                              ]
                            , clj_env:location(Env)
                            ),
        { clj_core:first(FirstArg)
        , clj_core:to_list(clj_core:rest(FirstArg))
        }
    end,

  {ArgsExprs, Env2} = clj_env:last_exprs( length(ArgsList)
                                        , analyze_forms(ArgsList, Env1)
                                        ),

  case TargetExpr of
    #{ op   := type
     , type := TypeSym} ->
      Module     = clj_core:keyword(TypeSym),
      Function   = clj_core:keyword(NameSym),
      ErlFunExpr = erl_fun_expr(List, Module, Function, length(ArgsList), Env),

      InvokeExpr = #{ op   => invoke
                    , env  => Env
                    , form => List
                    , f    => ErlFunExpr
                    , args => ArgsExprs
                    },

      clj_env:push_expr(InvokeExpr, Env2);
    _ ->
      Module          = type_tag(TargetExpr),
      Function        = clj_core:keyword(NameSym),
      ResolveTypeExpr = #{ op       => resolve_type
                         , env      => Env
                         , form     => Target
                         , module   => Module
                         , function => Function
                         },
      WarnOnInferVar  = 'clojerl.Var':?CONSTRUCTOR( <<"clojure.core">>
                                                  , <<"*warn-on-infer*">>
                                                  ),

      clj_utils:warn_when( clj_core:deref(WarnOnInferVar)
                           andalso Module =:= ?NIL
                         , [<<"Cannot infer target type for">>, List]
                         , clj_env:location(Env)
                         ),

      InvokeExpr = #{ op   => invoke
                    , env  => Env
                    , form => List
                    , f    => ResolveTypeExpr
                    , args => [TargetExpr | ArgsExprs]
                    },

      clj_env:push_expr(InvokeExpr, Env2)
  end.

-spec maybe_type_tag('clojerl.Symbol':type()) -> ?NIL | module().
maybe_type_tag(Symbol) ->
  Meta = clj_core:meta(Symbol),
  case clj_core:get(Meta, tag) of
    ?NIL -> ?NIL;
    Tag  -> clj_core:keyword(Tag)
  end.

-spec type_tag(map()) -> ?NIL | module().
type_tag(#{tag := Type}) -> Type;
type_tag(_) -> ?NIL.

%%------------------------------------------------------------------------------
%% Parse throw
%%------------------------------------------------------------------------------

-spec parse_throw('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_throw(List, Env) ->
  Count = clj_core:count(List),
  clj_utils:error_when( Count =/= 2
                      , [ <<"Wrong number of args to throw, had: ">>
                        , Count - 1
                        ]
                      , clj_env:location(Env)
                      ),

  Second = clj_core:second(List),
  ExceptionEnv = clj_env:context(expr, Env),
  {ExceptionExpr, Env1} = clj_env:pop_expr(analyze_form(Second, ExceptionEnv)),

  ThrowExpr = #{ op        => throw
               , env       => Env
               , form      => List
               , exception => ExceptionExpr
               },

  clj_env:push_expr(ThrowExpr, Env1).

%%------------------------------------------------------------------------------
%% Parse try
%%------------------------------------------------------------------------------

-spec parse_try('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_try(List, Env) ->
  CatchSymbol   = clj_core:symbol(<<"catch">>),
  FinallySymbol = clj_core:symbol(<<"finally">>),
  IsCatch       = fun(X) ->
                      clj_core:'seq?'(X) andalso
                        clj_core:equiv(clj_core:first(X), CatchSymbol)
                  end,
  IsFinally     = fun(X) ->
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

  Env1             = clj_env:put(in_try, true, Env),
  {BodyExpr, Env2} = clj_env:pop_expr(analyze_body(Body, Env1)),

  Env3                 = clj_env:context(expr, Env2),
  {CatchesExprs, Env4} =
    clj_env:last_exprs( length(Catches)
                      , lists:foldl(fun parse_catch/2, Env3, Catches)
                      ),

  {FinallyExpr, Env5} = case Finallies of
                          [Finally] ->
                            RestFinally = clj_core:rest(Finally),
                            clj_env:pop_expr(analyze_body(RestFinally, Env4));
                          _ ->
                            {?NIL, Env4}
                        end,

  TryExpr = #{ op      => 'try'
             , env     => Env
             , form    => List
             , body    => BodyExpr
             , catches => CatchesExprs
             , finally => FinallyExpr
             },

  Env6 = clj_env:put(in_try, false, Env5),
  clj_env:push_expr(TryExpr, Env6).

%%------------------------------------------------------------------------------
%% Parse catch
%%------------------------------------------------------------------------------

-spec parse_catch('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_catch(List, Env) ->
  [ _ %% catch
  , ErrType
  , ErrName
  | GuardAndBody
  ] = clj_core:to_list(List),

  clj_utils:error_when( not is_valid_error_type(ErrType)
                      , [<<"Bad error type: ">>, ErrType]
                      , clj_env:location(Env)
                      ),

  Env1 = clj_env:push(#{in_try => false, pattern_locals => []}, Env),

  {ErrExpr, Env2} = clj_env:pop_expr(parse_pattern(ErrName, Env1)),
  Local = #{ op      => binding
           , env     => Env
           , form    => ErrName
           , pattern => ErrExpr
           },

  LocalExprs = clj_env:get(pattern_locals, [], Env2),
  Env3       = clj_env:put_locals(LocalExprs, clj_env:add_locals_scope(Env2)),
  {_, Guard, Body}  = check_guard(GuardAndBody),
  {GuardExpr, Env4} = clj_env:pop_expr(analyze_form(Guard, Env3)),
  {BodyExpr, Env5}  = clj_env:pop_expr(analyze_body(Body, Env4)),

  CatchExpr = #{ op    => 'catch'
               , env   => Env
               , class => ErrType
               , local => Local
               , form  => List
               , guard => GuardExpr
               , body  => BodyExpr
               },

  Env6 = clj_env:pop(clj_env:remove_locals_scope(Env5)),
  clj_env:push_expr(CatchExpr, Env6).

-spec is_valid_error_type(any()) -> boolean().
is_valid_error_type(error) -> true;
is_valid_error_type(exit)  -> true;
is_valid_error_type(throw) -> true;
is_valid_error_type(Form)  ->
  clj_core:'symbol?'(Form) andalso clj_core:str(Form) =:= <<"_">>.

%%------------------------------------------------------------------------------
%% Parse var
%%------------------------------------------------------------------------------

-spec parse_var('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_var(List, Env) ->
  Count = clj_core:count(List),
  clj_utils:error_when( Count =/= 2
                      , [<<"Wrong number of args to var, had: ">>, Count]
                      , clj_env:location(Env)
                      ),

  VarSymbol = clj_core:second(List),

  case resolve(VarSymbol, false, Env) of
    {{var, Var}, Env1} ->
      VarConstExpr = #{ op   => constant
                      , env  => Env
                      , tag  => clj_core:type(Var)
                      , form => Var
                      },
      clj_env:push_expr(VarConstExpr, Env1);
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

-spec analyze_invoke('clojerl.List':type(), clj_env:env()) -> clj_env:env().
analyze_invoke(Form, Env) ->
  FSym = clj_core:first(Form),
  {FExpr, Env1} = clj_env:pop_expr(analyze_form(FSym, Env)),

  Args     = clj_core:to_list(clj_core:rest(Form)),
  ArgCount = length(Args),
  {ArgsExpr, Env2} = clj_env:last_exprs( ArgCount
                                       , analyze_forms(Args, Env1)
                                       ),

  InvokeExpr = #{ op   => invoke
                , env  => Env
                , form => Form
                , tag  => maps:get(tag, FExpr, ?NIL)
                , f    => FExpr#{arity => ArgCount}
                , args => ArgsExpr
                },

  clj_env:push_expr(InvokeExpr, Env2).

%%------------------------------------------------------------------------------
%% Analyze symbol
%%------------------------------------------------------------------------------

-spec analyze_symbol(clj_env:env(), 'clojerl.Symbol':type()) -> clj_env:env().
analyze_symbol(Symbol, Env) ->
  InPattern = clj_env:get(in_pattern, false, Env),
  case InPattern orelse resolve(Symbol, Env) of
    true ->
      clj_utils:error_when( not is_valid_bind_symbol(Symbol)
                          , [<<"Not a valid binding symbol, had: ">>, Symbol]
                          , clj_env:location(Env)
                          ),
      Ast = #{ op     => local
             , env    => Env
             , name   => Symbol
             , tag    => maybe_type_tag(Symbol)
             , shadow => clj_env:get_local(Symbol, Env)
             },
      Env1 = add_pattern_local(Ast, Env),
      clj_env:push_expr(Ast, Env1);
    {?NIL, _} ->
      clj_utils:error([ <<"Unable to resolve symbol '">>, Symbol
                      , <<"' in this context">>
                      ]
                     , clj_env:location(Env)
                     );
    {{local, Local}, Env1} ->
      clj_env:push_expr(Local#{env => Env}, Env1);
    {{erl_fun, Module, Function, Arity}, Env1} ->
      FunExpr = erl_fun_expr(Symbol, Module, Function, Arity, Env1),
      clj_env:push_expr(FunExpr, Env1);
    {{var, Var}, Env1} ->
      VarExpr = var_expr(Var, Symbol, Env1),
      clj_env:push_expr(VarExpr, Env1);
    {{type, Type}, Env1} ->
      TypeExpr = type_expr(Type, Symbol, Env1),
      clj_env:push_expr(TypeExpr, Env1)
  end.

-spec erl_fun_expr( 'clojerl.Symbol':type()
                  , module()
                  , atom()
                  , arity()
                  , clj_env:env()
                  ) -> clj_env:env().
erl_fun_expr(Symbol, Module, Function, Arity, Env) ->
  #{ op       => erl_fun
   , env      => Env
   , form     => Symbol
   , module   => Module
   , function => Function
   , arity    => Arity
   }.

-spec var_expr('clojerl.Var':type(), 'clojerl.Symbol':type(), clj_env:env()) ->
  map().
var_expr(Var, Symbol, Env) ->
 #{ op   => var
  , env  => Env
  , form => Symbol
  , name => Symbol
  , tag  => maybe_type_tag(Var)
  , var  => Var
  }.

-spec type_expr('clojerl.Symbol':type()
               , 'clojerl.Symbol':type()
               , clj_env:env()
               ) ->
  map().
type_expr(Type, Symbol, Env) ->
 #{ op   => type
  , env  => Env
  , form => Symbol
  , type => Type
  , tag  => 'clojerl.Keyword'
  }.

-type erl_fun() ::  {erl_fun, module(), atom(), integer()}.

-spec resolve('clojerl.Symbol':env(), clj_env:env()) ->
  { {var, 'clojerl.Var':type()} | erl_fun() | {local, map()} | ?NIL
  , clj_env:env()
  }.
resolve(Symbol, Env) ->
  resolve(Symbol, true, Env).

-spec resolve('clojerl.Symbol':env(), boolean(), clj_env:env()) ->
  { {var, 'clojerl.Var':type()} | erl_fun() | {local, map()} | ?NIL
  , clj_env:env()
  }.
resolve(Symbol, CheckPrivate, Env) ->
  CurrentNs = clj_namespace:current(),
  Local     = clj_env:get_local(Symbol, Env),
  NsStr     = clj_core:namespace(Symbol),
  MappedVal = clj_namespace:find_mapping(Symbol, CurrentNs),

  if
    Local =/= ?NIL ->
      {{local, Local}, Env};
    MappedVal =/= ?NIL ->
      case clj_core:'var?'(MappedVal) of
        true ->
          CurrentNsName = clj_core:name(clj_namespace:name(CurrentNs)),
          clj_utils:error_when( CheckPrivate
                                andalso NsStr =/= ?NIL
                                andalso NsStr =/= CurrentNsName
                                andalso not 'clojerl.Var':is_public(MappedVal)
                              , [MappedVal, <<" is not public">>]
                              , clj_env:location(Env)
                              ),
          {{var, MappedVal}, Env};
        false ->
          {{type, MappedVal}, Env}
      end;
    NsStr =/= ?NIL ->
      %% If there is no var then assume it's a Module:Function pair.
      %% Let's see how this works out.
      {erl_fun(Symbol, Env), Env};
    true ->
      case is_maybe_type(Symbol) of
        true  -> {{type, Symbol}, Env};
        false -> {?NIL, Env}
      end
  end.

-spec erl_fun('clojerl.Symbol':type(), clj_env:env()) -> erl_fun().
erl_fun(Symbol, Env0) ->
  NsSym          = clj_core:symbol(clj_core:namespace(Symbol)),
  {NsName, Env}  = case resolve(NsSym, Env0) of
                     {{type, TypeSym}, EnvTmp} ->
                       {clj_core:name(TypeSym), EnvTmp};
                     {_, EnvTmp} ->
                       {clj_core:name(NsSym), EnvTmp}
                  end,
  NsAtom        = binary_to_atom(NsName, utf8),
  {Name, Arity} = erl_fun_arity(clj_core:name(Symbol)),
  NameAtom      = binary_to_atom(Name, utf8),

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
is_maybe_type(?NIL) ->
  false;
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

-spec wrapping_meta(map(), clj_env:env()) -> clj_env:env().
wrapping_meta(#{form := Form, tag := Tag} = Expr, Env) ->
  Meta = case clj_core:'meta?'(Form) of
           true  -> clj_reader:remove_location(clj_core:meta(Form));
           false -> ?NIL
         end,
  case Meta of
    Meta when Meta =/= ?NIL andalso Meta =/= #{} ->
      {MetaExpr, Env1} = clj_env:pop_expr(analyze_form(Meta, Env)),
      WithMetaExpr = #{ op   => with_meta
                      , env  => Env
                      , form => Form
                      , tag  => Tag
                      , meta => MetaExpr
                      , expr => Expr
                      },
      clj_env:push_expr(WithMetaExpr, Env1);
    _ ->
      clj_env:push_expr(Expr, Env)
  end.

%%------------------------------------------------------------------------------
%% Analyze vector
%%------------------------------------------------------------------------------

-spec analyze_vector('clojerl.Vector':type(), clj_env:env()) -> clj_env:env().
analyze_vector(Vector, Env) ->
  Count   = clj_core:count(Vector),
  ExprEnv = clj_env:context(expr, Env),
  Items   = clj_core:to_list(Vector),
  Env1    = analyze_forms(Items, ExprEnv),
  {ItemsExpr, Env2} = clj_env:last_exprs(Count, Env1),

  VectorExpr = #{ op    => vector
                , env   => Env2
                , form  => Vector
                , tag   => 'clojerl.Vector'
                , items => ItemsExpr
                },

  wrapping_meta(VectorExpr, Env2).

%%------------------------------------------------------------------------------
%% Analyze map
%%------------------------------------------------------------------------------

-spec analyze_map('clojerl.Map':type(), clj_env:env()) -> clj_env:env().
analyze_map(Map, Env) ->
  Keys = clj_core:to_list(clj_core:keys(Map)),
  Vals = clj_core:to_list(clj_core:vals(Map)),

  Count = clj_core:count(Map),
  ExprEnv = clj_env:context(expr, Env),

  Env1 = analyze_forms(Keys, ExprEnv),
  {KeysExpr, Env2} = clj_env:last_exprs(Count, Env1),
  Env3 = analyze_forms(Vals, Env2),
  {ValsExpr, Env4} = clj_env:last_exprs(Count, Env3),

  MapExpr = #{ op   => map
             , env  => Env4
             , form => Map
             , tag  => 'clojerl.Map'
             , keys => KeysExpr
             , vals => ValsExpr
             },

  wrapping_meta(MapExpr, Env4).

%%------------------------------------------------------------------------------
%% Analyze Erlang map
%%------------------------------------------------------------------------------

-spec analyze_erl_map('clojerl.Map':type(), clj_env:env()) -> clj_env:env().
analyze_erl_map(Map, Env) ->
  Keys  = maps:keys(Map),
  Vals  = maps:values(Map),

  Count = maps:size(Map),
  ExprEnv = clj_env:context(expr, Env),

  Env1 = analyze_forms(Keys, ExprEnv),
  {KeysExpr, Env2} = clj_env:last_exprs(Count, Env1),
  Env3 = analyze_forms(Vals, Env2),
  {ValsExpr, Env4} = clj_env:last_exprs(Count, Env3),

  MapExpr = #{ op   => erl_map
             , env  => Env4
             , form => Map
             , tag  => 'clojerl.erlang.Map'
             , keys => KeysExpr
             , vals => ValsExpr
             },

  wrapping_meta(MapExpr, Env4).

%%------------------------------------------------------------------------------
%% Analyze set
%%------------------------------------------------------------------------------

-spec analyze_set('clojerl.Set':type(), clj_env:env()) -> clj_env:env().
analyze_set(Set, Env) ->
  ExprEnv = clj_env:context(expr, Env),
  Items   = clj_core:to_list(Set),
  Env1    = analyze_forms(Items, ExprEnv),

  Count   = clj_core:count(Set),
  {ItemsExpr, Env2} = clj_env:last_exprs(Count, Env1),

  SetExpr = #{ op    => set
             , env   => Env2
             , form  => Set
             , tag   => 'clojerl.Set'
             , items => ItemsExpr
             },

  wrapping_meta(SetExpr, Env2).

%%------------------------------------------------------------------------------
%% Analyze tuple
%%------------------------------------------------------------------------------

-spec analyze_tuple('clojerl.erlang.Tuple':type(), clj_env:env()) ->
  clj_env:env().
analyze_tuple(Tuple, Env) ->
  ExprEnv = clj_env:context(expr, Env),
  Items   = erlang:tuple_to_list(Tuple),
  Env1    = analyze_forms(Items, ExprEnv),

  Count   = erlang:tuple_size(Tuple),
  {ItemsExpr, Env2} = clj_env:last_exprs(Count, Env1),

  TupleExpr = #{ op    => tuple
               , env   => Env2
               , form  => Tuple
               , tag   => 'clojerl.erlang.Tuple'
               , items => ItemsExpr
               },

  clj_env:push_expr(TupleExpr, Env2).

%%------------------------------------------------------------------------------
%% receive
%%------------------------------------------------------------------------------

-spec parse_receive(any(), clj_env:env()) ->
  clj_env:env().
parse_receive(List, Env) ->
  [ _ %% receive*
  | ClausesAndAfter
  ] = clj_core:to_list(List),

  AfterSymbol = clj_core:symbol(<<"after">>),
  IsNotAfter  = fun(X) ->
                    not (clj_core:'seq?'(X)
                         andalso clj_core:equiv(clj_core:first(X), AfterSymbol))
                end,

  {Clauses, Afters} = lists:splitwith(IsNotAfter, ClausesAndAfter),

  clj_utils:error_when( length(Afters) > 1
                      , <<"Only one after clause allowed in "
                          "receive expression">>
                      , clj_env:location(Env)
                      ),

  {ClausesExprs, Default, Env1} = parse_patterns_bodies(Clauses, Env),

  clj_utils:error_when( Default =/= ?NIL
                      , [ <<"Expected an even number of forms in"
                            " a receive expression, got: ">>
                        , length(ClausesExprs) + 1
                        ]
                      , clj_env:location(Env)
                      ),

  {AfterExpr, Env2} = case Afters of
                        [] ->
                          {?NIL, Env1};
                        [After] ->
                          clj_env:pop_expr(parse_after(After, Env1))
                      end,

  ReceiveExpr = #{ op      => 'receive'
                 , env     => Env
                 , form    => List
                 , clauses => ClausesExprs
                 , 'after' => AfterExpr
                 },

  clj_env:push_expr(ReceiveExpr, Env2).

-spec parse_after(any(), clj_env:env()) -> clj_env:env().
parse_after(List, Env) ->
  [ _ %% after
  , Timeout
  | Body
  ] = clj_core:to_list(List),

  {TimeoutExpr, Env1} = clj_env:pop_expr(analyze_form(Timeout, Env)),
  {BodyExpr, Env2}    = clj_env:pop_expr(analyze_body(Body, Env1)),

  AfterExpr = #{ op      => 'after'
               , env     => Env
               , form    => List
               , timeout => TimeoutExpr
               , body    => BodyExpr
               },

  clj_env:push_expr(AfterExpr, Env2).

%%------------------------------------------------------------------------------
%% Erlang binary
%%------------------------------------------------------------------------------

-spec parse_erlang_binary(any(), clj_env:env()) ->
  clj_env:env().
parse_erlang_binary(List, Env0) ->
  [ _ %% erl-binary*
  | Segments
  ] = clj_core:to_list(List),

  Env1 = lists:foldl(fun parse_segment/2, Env0, Segments),
  {SegmentsExprs, Env2} = clj_env:last_exprs(length(Segments), Env1),

  BinaryExpr = #{ op       => erl_binary
                , env      => Env0
                , form     => List
                , segments => SegmentsExprs
                },

  clj_env:push_expr(BinaryExpr, Env2).

-spec parse_segment(any(), clj_env:env()) ->
  clj_env:env().
parse_segment(Segment0, Env0) ->
  Segment = segment_to_list(Segment0),
  clj_utils:error_when( not is_list(Segment)
                        orelse Segment =:= []
                      , [<<"Invalid segment: ">>, Segment]
                      , clj_env:location(Env0)
                      ),

  [Value | Rest] = Segment,
  Config0 = 'clojerl.Map':?CONSTRUCTOR(Rest),
  Config  = 'clojerl.Map':to_erl_map(Config0),
  Type    = maps:get(type, Config, integer),

  {ValueExpr, Env1} = parse_segment_value(Value, Env0),
  {TypeExpr, Env2}  = parse_segment_type(Config, Env1),
  {SizeExpr, Env3}  = parse_segment_size(Config, Type, Env2),
  {UnitExpr, Env4}  = parse_segment_unit(Config, Type, Env3),
  {FlagsExpr, Env5} = parse_segment_flags(Config, Env4),

  SegmentExpr = #{ op    => binary_segment
                 , env   => Env0
                 , form  => Segment
                 , value => ValueExpr
                 , size  => SizeExpr
                 , unit  => UnitExpr
                 , type  => TypeExpr
                 , flags => FlagsExpr
                 },

  clj_env:push_expr(SegmentExpr, Env5).

-spec segment_to_list(any()) -> list() | invalid.
segment_to_list(Binary) when is_binary(Binary) ->
  [Binary, type,  binary];
segment_to_list(Integer) when is_number(Integer) ->
  [Integer, type, integer];
segment_to_list(X) when ?IS_TYPE(X) ->
  IsVector = clj_core:'vector?'(X),
  IsSymbol = clj_core:'symbol?'(X),
  if
    IsVector -> clj_core:to_list(X);
    IsSymbol -> [X, size, 1, type, integer];
    true     -> invalid
  end;
segment_to_list(_) ->
  invalid.

-spec parse_segment_value(any(), clj_env:env()) ->
  {any(), clj_env:env()}.
parse_segment_value(Value, Env) ->
  Env1 = case clj_env:get(in_pattern, false, Env) of
           true  -> parse_pattern(Value, Env);
           false -> analyze_form(Value, Env)
         end,
  clj_env:pop_expr(Env1).

-spec parse_segment_size(any(), atom(), clj_env:env()) ->
  {any(), clj_env:env()}.
parse_segment_size(Config, Type, Env) ->
  Default = case Type of
              binary  -> all;
              float   -> 64;
              integer -> 8;
              _       -> undefined
            end,
  Size = maps:get(size, Config, Default),
  Env1 = case clj_env:get(in_pattern, false, Env) of
           true  -> parse_pattern(Size, Env);
           false -> analyze_form(Size, Env)
         end,
  clj_env:pop_expr(Env1).

-spec parse_segment_unit(any(), atom(), clj_env:env()) ->
  {any(), clj_env:env()}.
parse_segment_unit(Config, Type, Env) ->
  Default = case Type of
              binary  -> 8;
              float   -> 1;
              integer -> 1;
              _       -> undefined
            end,
  Unit = maps:get(unit, Config, Default),
  clj_env:pop_expr(analyze_const(Unit, Env)).

-spec parse_segment_type(any(), clj_env:env()) ->
  {any(), clj_env:env()}.
parse_segment_type(Config, Env) ->
  Type = maps:get(type, Config, integer),
  clj_utils:error_when( not lists:member(Type, valid_segment_types())
                      , [<<"Invalid binary segment type: ">>, Type]
                      , clj_env:location(Env)
                      ),
  clj_env:pop_expr(analyze_const(Type, Env)).

-spec valid_segment_types() -> [atom()].
valid_segment_types() ->
  [binary, integer, float, utf8, utf16, utf32].

-spec parse_segment_flags(any(), clj_env:env()) ->
  {any(), clj_env:env()}.
parse_segment_flags(Config, Env) ->
  Flags0 = maps:get(flags, Config, []),
  Flags1 = clj_core:to_list(Flags0),
  clj_utils:error_when( not lists:all(fun is_valid_segment_flag/1, Flags1)
                      , [<<"Invalid binary segment flag: ">>, Flags1]
                      , clj_env:location(Env)
                      ),
  Flags2 = case lists:any(fun is_segment_endianness/1, Flags1) of
             true -> Flags1;
             false -> [big | Flags1]
           end,
  Flags3 = case lists:any(fun is_segment_signedness/1, Flags2) of
             true -> Flags2;
             false -> [unsigned | Flags2]
           end,
  clj_env:pop_expr(analyze_const(Flags3, Env)).

-spec is_segment_signedness(any()) -> boolean().
is_segment_signedness(Flag) ->
  lists:member(Flag, valid_segment_signedness()).

-spec is_segment_endianness(any()) -> boolean().
is_segment_endianness(Flag) ->
  lists:member(Flag, valid_segment_endianness()).

-spec is_valid_segment_flag(any()) -> boolean().
is_valid_segment_flag(Flag) ->
  is_segment_signedness(Flag) orelse is_segment_endianness(Flag).

-spec valid_segment_signedness() -> [atom()].
valid_segment_signedness() ->
  [signed, unsigned].

-spec valid_segment_endianness() -> [atom()].
valid_segment_endianness() ->
  [little, big, native].

%%------------------------------------------------------------------------------
%% Erlang list
%%------------------------------------------------------------------------------

-spec parse_erlang_list(any(), clj_env:env()) ->
  clj_env:env().
parse_erlang_list(List, Env0) ->
  [ _ %% erl-list*
  | AllItems
  ] = clj_core:to_list(List),

  AmpersandSym   = clj_core:symbol(<<"&">>),
  IsNotAmpersand = fun(X) -> not clj_core:equiv(AmpersandSym, X) end,
  {Items, Tails} = lists:splitwith(IsNotAmpersand, AllItems),

  clj_utils:error_when( length(Tails) > 0 andalso length(Tails) =/= 2
                      , [<<"There has to be one expression after &, got ">>
                        , length(Tails)
                        ]
                      , clj_env:location(Env0)
                      ),

  {ItemsExprs, Env1} = clj_env:last_exprs( length(Items)
                                         , analyze_forms(Items, Env0)
                                         ),
  {TailExpr, Env2}   = case Tails of
                         [_, Tail] ->
                           clj_env:pop_expr(analyze_form(Tail, Env1));
                         [] ->
                           {undefined, Env1}
                       end,

  ListExpr = #{ op      => erl_list
              , env     => Env0
              , form    => List
              , items   => ItemsExprs
              , tail    => TailExpr
              },

  clj_env:push_expr(ListExpr, Env2).

%%------------------------------------------------------------------------------
%% Erlang match
%%------------------------------------------------------------------------------

-spec parse_erlang_alias(any(), clj_env:env()) ->
  clj_env:env().
parse_erlang_alias(List, Env0) ->
  [ _ %% erl-alias*
  | Args
  ] = clj_core:to_list(List),

  clj_utils:error_when( not clj_env:get(in_pattern, false, Env0)
                      , [<<"Alias not in pattern.">>]
                      , clj_env:location(Env0)
                      ),

  clj_utils:error_when( length(Args) =/= 2
                      , [ <<"Expected only 2 arguments for erl-match*, got:">>
                        , length(Args)
                        ]
                      , clj_env:location(Env0)
                      ),

  [Variable, Pattern]  = Args,

  clj_utils:error_when( not is_valid_bind_symbol(Variable)
                      , [ <<"First argument should be a valid binding symbol:">>
                        , Variable
                        ]
                      , clj_env:location(Env0)
                      ),

  {VariableExpr, Env1} = clj_env:pop_expr(parse_pattern(Variable, Env0)),
  {PatternExpr,  Env2} = clj_env:pop_expr(parse_pattern(Pattern, Env1)),

  MatchExpr   = #{ op       => erl_alias
                 , env      => Env0
                 , form     => List
                 , variable => VariableExpr
                 , pattern  => PatternExpr
                 },

  clj_env:push_expr(MatchExpr, Env2).

%%------------------------------------------------------------------------------
%% On load
%%------------------------------------------------------------------------------

-spec parse_on_load(any(), clj_env:env()) ->
  clj_env:env().
parse_on_load(List, Env0) ->
  Body            = clj_core:rest(List),
  {BodyExpr, Env} = clj_env:pop_expr(analyze_body(Body, Env0)),

  Expr = #{ op    => on_load
          , env   => Env0
          , form  => List
          , body  => BodyExpr
          },

  clj_env:push_expr(Expr, Env).
