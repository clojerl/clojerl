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

-spec is_special(any()) -> boolean().
is_special(S) ->
  maps:is_key(S, special_forms()).

-spec macroexpand_1(clj_env:env(), 'clojerl.List':type()) -> any().
macroexpand_1(Env, Form) ->
  Op = clj_core:first(Form),
  {MacroVar, Env} = lookup_var(Op, false, Env),

  case
    is_special(Op)
    orelse (not clj_core:'symbol?'(Op))
    orelse (MacroVar == undefined)
    orelse (not 'clojerl.Var':is_macro(MacroVar))
  of
    true -> Form;
    false ->
      {MacroVar, Env} = lookup_var(Op, false, Env),
      Var = clj_core:deref(MacroVar),
      Args = [Form, Env] ++ clj_core:seq(clj_core:rest(Form)),
      clj_core:invoke(Var, Args)
  end.

-spec macroexpand(clj_env:env(), 'clojerl.List':type()) -> any().
macroexpand(Env, Form) ->
  case macroexpand_1(Env, Form) of
    Form -> {Form, Env};
    ExpandedForm -> macroexpand_1(Env, ExpandedForm)
  end.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec special_forms() -> #{'clojerl.Symbol':type() => fun() | undefined}.
special_forms() ->
  #{ clj_core:symbol(<<"ns">>)       => fun parse_ns/2
   , clj_core:symbol(<<"def">>)      => fun parse_def/2
   , clj_core:symbol(<<"quote">>)    => fun parse_quote/2
   , clj_core:symbol(<<"fn*">>)      => fun parse_fn/2
   , clj_core:symbol(<<"do">>)       => fun parse_do/2
   , clj_core:symbol(<<"if">>)       => fun parse_if/2
   , clj_core:symbol(<<"let*">>)     => fun parse_let/2
   , clj_core:symbol(<<"loop*">>)    => fun parse_loop/2

   , clj_core:symbol(<<"recur">>)    => undefined
   , clj_core:symbol(<<"case*">>)    => undefined
   , clj_core:symbol(<<"letfn*">>)   => undefined
   , clj_core:symbol(<<"var">>)      => undefined
   , clj_core:symbol(<<"import*">>)  => undefined

   , clj_core:symbol(<<"deftype*">>) => undefined
   , clj_core:symbol(<<"reify*">>)   => undefined

   , clj_core:symbol(<<"throw">>)    => fun parse_throw/2
   , clj_core:symbol(<<"try">>)      => undefined
   , clj_core:symbol(<<"catch">>)    => undefined
   , clj_core:symbol(<<"finally">>)  => undefined

     %% , clj_core:symbol(<<"monitor-enter">>)
     %% , clj_core:symbol(<<"monitor-exit">>)
     %% , clj_core:symbol(<<"new">>)
     %% , clj_core:symbol(<<"&">>)
   }.

-spec analyze_forms(clj_env:env(), [any()]) -> clj_env:env().
analyze_forms(Env, Forms) ->
  AnalyzeFun = fun(Form, EnvAcc) -> analyze_form(EnvAcc, Form) end,
  lists:foldl(AnalyzeFun, Env, Forms).

-spec analyze_form(clj_env:env(), any()) -> clj_env:env().
analyze_form(Env, Form) ->
  IsSeq = clj_core:'seq?'(Form),
  case {clj_core:type(Form), IsSeq} of
    {_, true} ->
      Op = clj_core:first(Form),
      analyze_seq(Env, Op, Form);
    {'clojerl.Symbol', _} ->
      analyze_symbol(Env, Form);
    {'clojerl.Vector', _} ->
      analyze_vector(Env, Form);
    {'clojerl.Map', _} ->
      analyze_map(Env, Form);
    {'clojerl.Set', _} ->
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
analyze_seq(_Env, undefined, _List) ->
  throw(<<"Can't call nil">>);
analyze_seq(Env, Op, List) ->
  case macroexpand_1(Env, List) of
    List ->
      case maps:get(Op, special_forms(), undefined) of
        undefined ->
          analyze_invoke(Env, List);
        ParseFun ->
          ParseFun(Env, List)
      end;
    ExpandedList ->
      analyze_form(Env, ExpandedList)
  end.

%%------------------------------------------------------------------------------
%% Parse ns
%%------------------------------------------------------------------------------

-spec parse_ns(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_ns(Env, List) ->
  Second = clj_core:second(List),
  case clj_core:'symbol?'(Second) of
    true ->
      {_, NewEnv} = clj_env:find_or_create_ns(Env, Second),
      NewEnv;
    false ->
      throw(<<"First argument to ns must a symbol">>)
  end.

%%------------------------------------------------------------------------------
%% Parse quote
%%------------------------------------------------------------------------------

-spec parse_quote(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_quote(Env, List) ->
  case clj_core:count(List) of
    2 -> ok;
    Count ->
      CountBin = integer_to_binary(Count - 1),
      throw(<<"Wrong number of args to quote, had: ", CountBin/binary>>)
  end,
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
      false -> {clj_core:symbol(<<"anon">>), clj_core:rest(List)}
    end,

  MethodsList = case clj_core:'vector?'(clj_core:first(Methods)) of
                  true -> [Methods];
                  false -> clj_core:seq2(Methods)
                end,

  %% Create a var with a modified name and add it as a local binding
  %% with the original name.
  VarNsSym   = clj_env:current_ns(Env),
  NameBin    = clj_core:name(NameSym),
  VarNameSym = clj_core:gensym(<<NameBin/binary, "__fn__">>),
  Var        = 'clojerl.Var':new(VarNsSym, VarNameSym),
  VarExpr    = var_expr(Var, VarNameSym, Env),
  Env1       = clj_env:put_local( clj_env:add_locals_scope(Env)
                                , NameSym
                                , VarExpr
                                ),

  OpMeta = clj_core:meta(Op),
  OnceKeyword = clj_core:keyword(<<"once">>),
  IsOnce = clj_core:boolean(clj_core:get(OpMeta, OnceKeyword)),

  {MethodsExprs, Env2} = analyze_fn_methods(Env1, MethodsList, IsOnce),

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
  clj_utils:throw_when(length(AllVariadics) >= 2,
                       <<"Can't have more than 1 variadic overload">>),

  DistinctFixedArities = lists:usort(FixedArities),
  clj_utils:throw_when(length(DistinctFixedArities) =/= length(FixedArities),
                       <<"Can't have 2 or more overloads "
                         "with the same arity">>),

  clj_utils:throw_when(IsVariadic andalso
                       VariadicArity =/= undefined andalso
                       MaxFixedArity =/= undefined andalso
                       MaxFixedArity > VariadicArity,
                       <<"Can't have fixed arity overload "
                         "with more params than variadic overload">>),

  %% Now that we have all the fn info we re-analyze the methods
  %% with the associated var, so that a recursive
  %% call can be correctly resolved.
  VarMeta = #{ 'variadic?'     => IsVariadic
             , max_fixed_arity => MaxFixedArity
             , variadic_arity  => VariadicArity
             },
  Var1     = clj_core:with_meta(Var, VarMeta),
  VarExpr1 = var_expr(Var1, VarNameSym, Env2),
  Env3     = clj_env:put_local(Env2, NameSym, VarExpr1),
  {MethodsExprs1, Env4} = analyze_fn_methods(Env3, MethodsList, IsOnce),

  FnExpr = #{ op              => fn
            , env             => ?DEBUG(Env)
            , form            => List
            , 'variadic?'     => IsVariadic
            , max_fixed_arity => MaxFixedArity
            , variadic_arity  => VariadicArity
            , methods         => MethodsExprs1
            , once            => IsOnce
            , local           => Var1
            },

  Env5 = clj_env:remove_locals_scope(Env4),

  clj_env:push_expr(Env5, FnExpr).

-spec analyze_fn_methods(clj_env:env(), ['clojerl.List':type()], boolean()) ->
 {[map()], clj_env:env()}.
analyze_fn_methods(Env, MethodsList, IsOnce) ->
  MethodEnv = maps:put(once, IsOnce, maps:remove(in_try, Env)),
  AnalyzeFnMethodFun = fun(M, EnvAcc) -> analyze_fn_method(EnvAcc, M) end,
  Env1 = lists:foldl(AnalyzeFnMethodFun, MethodEnv, MethodsList),
  clj_env:last_exprs(Env1, length(MethodsList)).

-spec analyze_fn_method(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
analyze_fn_method(Env, List) ->
  Params = clj_core:first(List),
  clj_utils:throw_when(not clj_core:'vector?'(Params),
                       <<"Parameter declaration should be a vector">>),

  ParamsList = clj_core:seq2(Params),
  clj_utils:throw_when(not lists:all(fun is_valid_bind_symbol/1, ParamsList),
                       [<<"Params must be valid binding symbols, had: ">>,
                        Params]),

  AmpersandSym = clj_core:symbol(<<"&">>),
  IsAmpersandFun = fun(X) -> X == AmpersandSym end,
  IsVariadic = lists:any(IsAmpersandFun, ParamsList),
  ParamsNames = ParamsList -- [AmpersandSym],
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
                     },
        {Id + 1, [ParamExpr | Exprs]}
    end,
  {_, ParamsExprs} = lists:foldl(ParamExprFun, {0, []}, ParamsNames),

  FixedArity = case IsVariadic of true -> Arity - 1; false -> Arity end,

  LoopId = clj_core:gensym(<<"loop_">>),
  BodyEnv = clj_env:put_locals(Env1, ParamsExprs),
  BodyEnv1 = clj_env:context(BodyEnv, return),
  BodyEnv2 = clj_env:put(BodyEnv1, loop_id, LoopId),
  BodyEnv3 = clj_env:put(BodyEnv2, loop_locals, length(ParamsExprs)),

  Body = clj_core:rest(List),
  {BodyExpr, Env2} = clj_env:pop_expr(analyze_body(BodyEnv3, Body)),

  %% TODO: check for a single symbol after '&

  FnMethodExpr = maps:merge(#{ op          => fn_method
                             , form        => List
                             , loop_id     => LoopId
                             , env         => ?DEBUG(Env1)
                             , 'variadic?' => IsVariadic
                             , params      => lists:reverse(ParamsExprs)
                             , fixed_arity => FixedArity
                             , body        => BodyExpr
                             },
                           case maps:get(local, Env, undefined) of
                             undefined -> #{};
                             Local -> #{local => Local}
                           end),

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
  Statements = clj_core:rest(Form),
  StmtEnv = clj_env:context(Env, statement),
  StatementsList = clj_core:seq2(Statements),
  Env1 = analyze_forms(StmtEnv, StatementsList),
  {AllStatementsExprs, Env2} =
    clj_env:last_exprs(Env1, clj_core:count(Statements)),

  {NilExpr, _} = clj_env:pop_expr(analyze_form(Env, undefined)),
  {StatementsExprs, ReturnExpr} =
    case AllStatementsExprs of
      [] -> {[], NilExpr};
      _ -> {lists:droplast(AllStatementsExprs),
            lists:last(AllStatementsExprs)}
    end,
  DoExpr = #{ op         => do
            , env        => ?DEBUG(Env)
            , form       => Statements
            , statements => StatementsExprs
            , ret        => ReturnExpr
            },

  clj_env:push_expr(Env2, DoExpr).

%%------------------------------------------------------------------------------
%% Parse if
%%------------------------------------------------------------------------------

-spec parse_if(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_if(Env, Form) ->
  Count = clj_core:count(Form),
  clj_utils:throw_when(Count =/= 3 andalso Count =/= 4,
                       [<<"Wrong number of args to if, had: ">>, Count - 1]),

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
            , env  => ?DEBUG(Env3)
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
  {LetExprExtra, _Env2} = analyze_let(Env, Form),
  LetExpr = maps:merge(#{ op   => 'let'
                        , form => Form
                        , env  => ?DEBUG(Env2)
                        },
                       LetExprExtra),

  clj_env:push_expr(Env, LetExpr).

-spec parse_loop(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_loop(Env, Form) ->
  LoopId = clj_core:gensym(<<"loop_">>),
  Env1 = clj_env:put(Env, loop_id, LoopId),
  {LoopExprExtra, Env2} = analyze_let(Env1, Form),
  LoopExpr = maps:merge(#{ op      => loop
                         , form    => Form
                         , env     => ?DEBUG(Env2)
                         , loop_id => LoopId
                         },
                        LoopExprExtra),

  clj_env:push_expr(Env2, LoopExpr).

-spec analyze_let(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
analyze_let(Env, Form) ->
  validate_bindings(Form),
  Op = clj_core:first(Form),
  IsLoop = clj_core:symbol(<<"loop*">>) == Op,

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
                      ),
  OpAtom = case clj_env:get(Env, is_loop) of
             true -> loop;
             false -> 'let'
           end,
  {InitExpr, _} = clj_env:pop_expr(analyze_form(Env, Init)),
  BindExpr = #{ op     => binding
              , env    => ?DEBUG(Env)
              , name   => Name
              , shadow => clj_env:get_local(Env, Name)
              , init   => InitExpr
              , form   => Name
              , local  => OpAtom
              },

  Env2 = clj_env:put_local(Env, Name, maps:remove(env, BindExpr)),
  clj_env:push_expr(Env2, BindExpr).

-spec validate_bindings('clojerl.List':type()) -> ok.
validate_bindings(Form) ->
  Op = clj_core:first(Form),
  Bindings = clj_core:second(Form),
  clj_utils:throw_when(not clj_core:'vector?'(Bindings),
                       [Op,
                        <<" requires a vector for its bindings, had: ">>,
                        clj_core:type(Bindings)]),

  clj_utils:throw_when(not clj_core:'even?'(clj_core:count(Bindings)),
                       [Op,
                        <<" requires an even number of forms in binding "
                          "vector, had: ">>,
                        clj_core:count(Bindings)]),
  ok.

%%------------------------------------------------------------------------------
%% Parse def
%%------------------------------------------------------------------------------

-spec parse_def(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_def(Env, List) ->
  Docstring = validate_def_args(List),
  VarSymbol = clj_core:second(List),
  case lookup_var(VarSymbol, Env) of
    {undefined, _} ->
      throw(<<"Can't refer to qualified var that doesn't exist">>);
    {Var, Env1} ->
      VarNsSym = 'clojerl.Var':namespace(Var),
      clj_utils:throw_when( clj_core:namespace(VarSymbol) =/= undefined
                            andalso clj_env:current_ns(Env1) =/= VarNsSym
                          , <<"Can't create defs outside of current ns">>
                          ),

      Var1Meta   = clj_core:meta(Var),
      SymbolMeta = clj_core:meta(VarSymbol),
      MetaMaps   = [#{}, Var1Meta, SymbolMeta],
      Var1       = clj_core:with_meta(Var, clj_core:merge(MetaMaps)),

      IsDynamic = 'clojerl.Var':is_dynamic(Var1),
      NameBin   = clj_core:name(VarSymbol),

      clj_utils:warn_when( not IsDynamic andalso
                           nomatch =/= re:run(NameBin, "\\*.+\\*")
                         , [ <<"Var ">>
                           , NameBin
                           , <<" is not dynamic but its name"
                               " suggests otherwise.~n">>
                           ]
                         ),

      Env2 = clj_env:update_var(Env1, Var1),
      Init = case Docstring of
               undefined -> clj_core:third(List);
               _ -> clj_core:fourth(List)
             end,

      ExprEnv2 = clj_env:context(Env2, expr),
      {InitExpr, Env3} = clj_env:pop_expr(analyze_form(ExprEnv2, Init)),

      Var2 = var_fn_info(Var1, InitExpr),
      Env4 = clj_env:update_var(Env3, Var2),
      {InitExpr1, Env5} = case InitExpr of
                            #{op := fn} ->
                              %% If init is an fn we need to analyze it
                              %% again to get the associated var resolved
                              %% with the function's info, to get proper
                              %% function calls emitted.
                              clj_env:pop_expr(analyze_form(Env4, Init));
                            _ ->
                              {InitExpr, Env4}
                          end,

      DefExpr = #{ op      => def
                 , env     => ?DEBUG(Env)
                 , form    => List
                 , name    => VarSymbol
                 , var     => Var2
                 , doc     => Docstring
                 , init    => InitExpr1
                 , dynamic => IsDynamic
                 },

      clj_env:push_expr(Env5, DefExpr)
  end.

-spec var_fn_info('clojerl.Var':type(), map()) -> 'clojerl.Var':type().
var_fn_info(Var, #{op := fn} = Expr) ->
  %% Add information about the associated function
  %% to the var's metadata.
  RemoveKeys = [op, env, methods, form, once, local],
  ExprInfo = maps:without(RemoveKeys, Expr),
  VarMeta = clj_core:meta(Var),
  VarMeta1 = clj_core:merge([VarMeta, ExprInfo]),
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
        _ -> throw(<<"First argument to def must be a symbol">>)
      end,
      Docstring;
    1 ->
      throw(<<"Too few arguments to def">>);
    _ ->
      throw(<<"Too many arguments to def">>)
  end.

-spec lookup_var('clojerl.Symbol':type(), clj_env:env()) -> ok.
lookup_var(VarSymbol, Env) ->
  lookup_var(VarSymbol, true, Env).

-spec lookup_var('clojerl.Symbol':type(), boolean(), clj_env:env()) ->
  {'clojerl.Var':type(), clj_env:env()}.
lookup_var(VarSymbol, true = _CreateNew, Env) ->
  case clj_core:'symbol?'(VarSymbol) of
    false -> {undefined, Env};
    true ->
      NsSym = case clj_core:namespace(VarSymbol) of
                undefined -> undefined;
                NsStr -> clj_core:symbol(NsStr)
              end,
      case clj_env:current_ns(Env) of
        CurrentNs when CurrentNs == NsSym; NsSym == undefined ->
          NameSym = clj_core:symbol(clj_core:name(VarSymbol)),
          Fun = fun(Ns) -> clj_namespace:intern(Ns, NameSym) end,
          NewEnv = clj_env:update_ns(Env, CurrentNs, Fun),
          lookup_var(VarSymbol, false, NewEnv);
        _ ->
          lookup_var(VarSymbol, false, Env)
      end
  end;
lookup_var(VarSymbol, false, Env) ->
  case clj_core:'symbol?'(VarSymbol) of
    false -> {undefined, Env};
    true ->

      NsStr = clj_core:namespace(VarSymbol),
      NameStr = clj_core:name(VarSymbol),

      case {NsStr, NameStr} of
        {undefined, Name} when Name == <<"ns">>;
                               Name == <<"in-ns">> ->
          ClojureCoreSym = clj_core:symbol(<<"clojure.core">>, Name),
          Var = clj_env:find_var(Env, ClojureCoreSym),
          {Var, Env};
        {undefined, _} ->
          CurrentNsSym = clj_env:current_ns(Env),
          Symbol = clj_core:symbol(clj_core:name(CurrentNsSym), NameStr),
          Var = clj_env:find_var(Env, Symbol),
          {Var, Env};
        {NsStr, NameStr} ->
          Symbol = clj_core:symbol(NsStr, NameStr),
          Var = clj_env:find_var(Env, Symbol),
          {Var, Env}
      end
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
                      ),

  Second = clj_core:second(List),
  ExceptionEnv = clj_env:context(Env, expr),
  {ExceptionExpr, _} = clj_env:pop_expr(analyze_form(ExceptionEnv, Second)),

  ThrowExpr = #{ op        => throw
               , env       => ?DEBUG(Env3)
               , form      => List
               , exception => ExceptionExpr
               },

  clj_env:push_expr(Env, ThrowExpr).

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
  case {clj_core:namespace(Symbol), clj_env:get_local(Env, Symbol)} of
    %% If the local is actually a var, then just return it
    %% without modifying the op.
    {undefined, #{op := var} = Local} ->
      clj_env:push_expr(Env, Local);
    {undefined, Local} when Local =/= undefined ->
      clj_env:push_expr(Env, Local#{op => local});
    _ ->
      case resolve(Env, Symbol) of
        undefined ->
          Str = clj_core:str(Symbol),
          throw(<<"Unable to resolve var: ", Str/binary, " in this context">>);
        {erl_fun, Module, Function, Arity} ->
          FunExpr = #{ op       => erl_fun
                     , env      => ?DEBUG(Env)
                     , module   => Module
                     , function => Function
                     , arity    => Arity
                     },
          clj_env:push_expr(Env, FunExpr);
        Var ->
          VarExpr = var_expr(Var, Symbol, Env),
          clj_env:push_expr(Env, VarExpr)
      end
  end.

-spec var_expr('clojerl.Var':type(), 'clojerl.Symbol':type(), clj_env:env()) ->
  map().
var_expr(Var, Symbol, _Env) ->
 #{ op   => var
  , env  => ?DEBUG(Env)
  , form => Symbol
  , var  => Var
  }.

-spec resolve(clj_env:env(), 'clojerl.Symbol':env()) ->
  'clojerl.Var':type() | {erl_fun, module(), atom(), integer()} | undefined.
resolve(Env, Symbol) ->
  CurrentNs = clj_env:find_ns(Env, clj_env:current_ns(Env)),
  Local = clj_env:get_local(Env, Symbol),
  NsStr = clj_core:namespace(Symbol),
  UsedVar = clj_namespace:use(CurrentNs, Symbol),
  CurNsVar = clj_namespace:def(CurrentNs, Symbol),

  case {Local, NsStr, UsedVar, CurNsVar} of
    {Local, _, _, _} when Local =/= undefined ->
      Local;
    {_, NsStr, _, _} when NsStr =/= undefined ->
      case clj_env:find_var(Env, Symbol) of
        undefined ->
          %% If there is no var then assume it's a Module:Function pair.
          %% Let's see how this works out.
          NsAtom = binary_to_atom(clj_core:namespace(Symbol), utf8),
          {Name, Arity} = erl_fun_arity(clj_core:name(Symbol)),
          NameAtom = binary_to_atom(Name, utf8),
          {erl_fun, NsAtom, NameAtom, Arity};
        Var ->
          Var
      end;
    {_, _, UsedVar, _} when UsedVar =/= undefined ->
      UsedVar;
    {_, _, _, CurNsVar} when CurNsVar =/= undefined ->
      CurNsVar;
    _ ->
      undefined
  end.

-spec erl_fun_arity(binary()) -> {binary(), undefined | integer()}.
erl_fun_arity(Name) ->
  case binary:split(Name, <<".">>, [global]) of
    [_] -> {Name, undefined};
    Parts ->
      Last = lists:last(Parts),
      case re:run(Last, <<"\\d+">>) of
        nomatch -> {Name, undefined};
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
  Keys = 'clojerl.Map':keys(Map),
  Vals = 'clojerl.Map':vals(Map),

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
