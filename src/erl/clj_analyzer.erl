-module(clj_analyzer).

-dialyzer({nowarn_function, analyze_form/2}).

-include("clojerl.hrl").
-include("clojerl_int.hrl").
-include("clojerl_expr.hrl").

-export([ analyze/2
        , macroexpand_1/2
        , macroexpand/2
        , is_special/1
        ]).

-spec analyze(any(), clj_env:env()) -> clj_env:env().
analyze(Form, Env0) ->
  analyze_form(Form, Env0).

-spec is_special('clojerl.Symbol':type()) -> boolean().
is_special(S) ->
  clj_rt:'symbol?'(S) andalso
    maps:is_key('clojerl.Symbol':str(S), special_forms()).

-spec macroexpand_1(any(), clj_env:env()) -> any().
macroexpand_1(Form, Env) ->
  case clj_rt:'seq?'(Form) of
    true  -> do_macroexpand_1(Form, Env);
    false -> Form
  end.

-spec do_macroexpand_1(any(), clj_env:env()) -> any().
do_macroexpand_1(Form, Env) ->
  Op        = clj_rt:first(Form),
  IsSymbol  = clj_rt:'symbol?'(Op),
  IsSpecial = is_special(Op),
  MacroVar  = case IsSymbol of
                true  -> lookup_var(Op, false);
                false -> ?NIL
              end,

  Expanded  = case
                not IsSpecial
                andalso MacroVar =/= ?NIL
                andalso 'clojerl.Var':is_macro(MacroVar)
              of
                true ->
                  Args = clj_rt:cons( Form
                                    , clj_rt:cons( Env
                                                 , clj_rt:rest(Form)
                                                 )
                                    ),
                  MacroVar1 = 'clojerl.Var':mark_fake_fun(MacroVar),
                  'clojerl.IFn':apply(MacroVar1, Args);
                false ->
                  case IsSymbol andalso not IsSpecial of
                    true  -> maybe_macroexpand_symbol(Form, Op);
                    false -> Form
                  end
              end,
  keep_location_meta(Expanded, Form).

-spec macroexpand(any(), clj_env:env()) -> any().
macroexpand(Form, Env) ->
  case macroexpand_1(Form, Env) of
    Form     -> Form;
    Expanded -> macroexpand(Expanded, Env)
  end.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec maybe_macroexpand_symbol(any(), 'clojerl.Symbol':type()) -> any().
maybe_macroexpand_symbol(Form, OpSym) ->
  OpBin = 'clojerl.Symbol':name(OpSym),
  case 'clojerl.String':char_at(OpBin, 0) of
    <<".">> ->
      DotSym  = clj_rt:symbol(<<".">>),
      Length  = 'clojerl.String':count(OpBin),
      NameBin = 'clojerl.String':substring(OpBin, 1, Length),
      NameSym = clj_rt:symbol(NameBin),
      [_, Target | Args] = clj_rt:to_list(Form),
      keep_tag(Form, clj_rt:list([DotSym, Target, NameSym | Args]));
    _ ->
      case 'clojerl.String':ends_with(OpBin, <<".">>) of
        true ->
          NewSym  = clj_rt:symbol(<<"new">>),
          Length  = 'clojerl.String':count(OpBin),
          NameBin = 'clojerl.String':substring(OpBin, 0, Length - 1),
          NameSym = clj_rt:symbol(NameBin),
          [_ | Args]  = clj_rt:to_list(Form),

          keep_tag(Form, clj_rt:list([NewSym, NameSym | Args]));
        false ->
          Form
      end
  end.

-spec keep_tag(any(), X) -> X.
keep_tag(Form, List) ->
  case clj_rt:'meta?'(Form) of
    true ->
      Meta = clj_rt:meta(Form),
      case clj_rt:get(Meta, tag) of
        ?NIL -> List;
        Tag  -> clj_rt:with_meta(List, #{tag => Tag})
      end;
    false ->
      List
  end.

-spec keep_location_meta(any(), any()) -> any().
keep_location_meta(Form, Form) ->
  Form;
keep_location_meta(Expanded, Form) ->
  case clj_rt:'meta?'(Form) andalso clj_rt:'meta?'(Expanded) of
    true  ->
      LocationMeta = clj_reader:location_meta(Form),
      ExpandedMeta = clj_rt:meta(Expanded),
      Meta         = clj_rt:merge([ExpandedMeta, LocationMeta]),
      clj_rt:with_meta(Expanded, Meta);
    false ->
      Expanded
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
   , <<"erl-fun*">>     => fun parse_erlang_fun/2
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
  IsSeq     = clj_rt:'seq?'(Form),
  IsSymbol  = clj_rt:'symbol?'(Form),
  IsRecord  = clj_rt:'record?'(Form),
  IsType    = clj_rt:'type?'(Form),
  IsVector  = clj_rt:'vector?'(Form),
  IsMap     = clj_rt:'map?'(Form),
  IsErlMap  = is_map(Form) andalso not ?IS_TYPE(Form),
  IsSet     = clj_rt:'set?'(Form),
  IsVar     = clj_rt:'var?'(Form),
  IsTuple   = erlang:is_tuple(Form),
  if
    IsSeq ->
      case clj_rt:'empty?'(Form) of
        true  -> analyze_const(Form, Env);
        false -> analyze_seq(Form, Env)
      end;
    IsSymbol -> analyze_symbol(Form, Env);
    IsRecord -> analyze_const(Form, Env);
    IsType   -> analyze_const(Form, Env);
    IsVector -> analyze_vector(Form, Env);
    IsErlMap -> analyze_erl_map(Form, Env);
    IsMap    -> analyze_map(Form, Env);
    IsSet    -> analyze_set(Form, Env);
    IsTuple  -> analyze_tuple(Form, Env);
    %% The var's metadata should already have been analyzed
    IsVar    -> analyze_const(Form, _CheckWrappingMeta = false, Env);
    true     -> analyze_const(Form, Env)
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
          , tag  => type_expr(Constant, Env)
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
  Mapping = case clj_reader:location_meta(List) of
              ?NIL     -> #{};
              Location -> #{location => Location}
            end,
  Env1    = clj_env:push(Mapping, Env0),
  Env2    = case macroexpand_1(List, Env1) of
              List         -> do_analyze_seq(List, Env1);
              ExpandedList -> analyze_form(ExpandedList, Env1)
            end,

  clj_env:pop(Env2).

-spec clear_def_var(clj_env:env()) -> clj_env:env().
clear_def_var(Env) ->
  clj_env:put(def_var, ?NIL, Env).

%% [#453] Keep def_var when parsing a fn*
-spec maybe_clear_def_var(binary(), clj_env:env()) -> clj_env:env().
maybe_clear_def_var(OpStr, Env0) ->
  case OpStr =:= <<"fn*">> of
    true  -> Env0;
    false -> clear_def_var(Env0)
  end.

-spec do_analyze_seq('clojerl.List':type(), any()) -> clj_env:env().
do_analyze_seq(List, Env0) ->
  Op      = clj_rt:first(List),

  ?ERROR_WHEN( Op =:= ?NIL
             , <<"Can't call nil">>
             , clj_env:location(Env0)
             ),

  case clj_rt:'symbol?'(Op) orelse clj_rt:'var?'(Op) of
    true  ->
      maybe_inline(Op, List, Env0);
    false ->
      analyze_invoke(List, Env0)
  end.

-spec maybe_inline('clojerl.Symbol':type(), any(), clj_env:env()) ->
  clj_env:env().
maybe_inline(Op, List, Env0) ->
  Var   = case clj_rt:'var?'(Op) of
            true  -> Op;
            false -> lookup_var(Op, false)
          end,
  Meta  = case Var of
            ?NIL -> ?NIL;
            Var  -> clj_rt:meta(Var)
          end,
  Rest  = clj_rt:next(List),
  Arity = clj_rt:count(Rest),
  case is_inline(Meta, Arity) of
    ?NIL ->
      OpStr   = 'clojerl.Symbol':str(Op),
      Env1    = maybe_clear_def_var(OpStr, Env0),
      Default = fun analyze_invoke/2,
      Fun     = maps:get(OpStr, special_forms(), Default),
      Fun(List, Env1);
    InlineExpr ->
      ?DEBUG({inlining, List}),
      Fun   = inline_fun(Var, InlineExpr),
      Form0 = clj_rt:apply(Fun, Rest),
      Form1 = keep_tag(List, Form0),
      Form2 = keep_location_meta(Form1, List),
      analyze_form(Form2, Env0)
  end.

-spec is_inline(any(), integer()) -> ?NIL | 'clojerl.List':type().
is_inline(Meta, Arity) ->
  Inline = clj_rt:get(Meta, inline),
  InlineArities = clj_rt:get(Meta, 'inline-arities'),
  case Inline of
    ?NIL ->
      ?NIL;
    Inline when InlineArities =/= ?NIL ->
      Result = clj_rt:apply(InlineArities, [Arity]),
      case clj_rt:boolean(Result) of
        true  -> Inline;
        false -> ?NIL
      end;
    _ ->
      Inline
  end.

%% Keep a cached version of the inline function to avoid having to eval
%% all the time.
%% NOTE: We use the process dictionary since the process will die once
%% it's done compiling the file.
-spec inline_fun('clojerl.Var':type(), any()) -> any().
inline_fun(Var, InlineExpr) ->
  Key = {inline_fun, 'clojerl.Var':str(Var)},
  case get(Key) of
    ?NIL ->
      Fun = eval_inline(InlineExpr),
      put(Key, Fun),
      Fun;
    Fun ->
      Fun
  end.

-spec eval_inline(any()) -> any().
eval_inline(Expr) ->
  Env1           = analyze_form(Expr, clj_env:default()),
  {Exprs, _Env2} = clj_emitter:emit(Env1),
  clj_compiler:eval_expressions(Exprs, true).

%%------------------------------------------------------------------------------
%% Parse quote
%%------------------------------------------------------------------------------

-spec parse_quote('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_quote(List, Env) ->
  Count = clj_rt:count(List),
  ?ERROR_WHEN( Count =/= 2
             , [<<"Wrong number of args to quote, had: ">>, Count - 1]
             , clj_env:location(Env)
             ),

  Second = clj_rt:second(List),

  { #{tag := TagExpr} = ConstExpr
  , NewEnv
  } = clj_env:pop_expr(analyze_const(Second, Env)),

  Expr = #{ op   => quote
          , env  => Env
          , form => List
          , tag  => TagExpr
          , expr => ConstExpr
          },
  clj_env:push_expr(Expr, NewEnv).

%%------------------------------------------------------------------------------
%% Parse fn*
%%------------------------------------------------------------------------------

-spec parse_fn('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_fn(List, Env) ->
  Op = clj_rt:first(List),

  {NameSym, Methods} =
    case clj_rt:'symbol?'(clj_rt:second(List)) of
      true  -> {clj_rt:second(List), clj_rt:rest(clj_rt:rest(List))};
      false -> {clj_rt:gensym(<<"anon__">>), clj_rt:rest(List)}
    end,

  %% Check if there is more than one method
  MethodsList = case clj_rt:'vector?'(clj_rt:first(Methods)) of
                  true  -> [Methods];
                  false -> clj_rt:to_list(Methods)
                end,

  {TagExpr, Env0} = fetch_type_tag(NameSym, clj_env:add_locals_scope(Env)),
  %% Add the name of the fn as a local binding
  LocalExpr     = #{ op         => local
                   , env        => Env
                   , form       => NameSym
                   , tag        => TagExpr
                   , name       => NameSym
                   , shadow     => ?NIL
                   , underscore => is_underscore(NameSym)
                   , id         => erlang:unique_integer()
                   },

  %% If there is a def var we add it to the local scope
  DefVar      = clj_env:get(def_var, Env),
  IsDef       = DefVar =/= ?NIL,
  DefNameSym  = case IsDef of
                  true  -> clj_rt:symbol('clojerl.Var':name(DefVar));
                  false -> ?NIL
                end,
  %% If it is a def we register the var, otherwise register the local.
  Env1 = case DefVar of
           ?NIL ->
             clj_env:put_local(NameSym, LocalExpr, Env0);
           DefVar  ->
             'clojerl.Namespace':update_var(DefVar),
             %% Register a local that maps the symbol fn to the var
             {VarExpr, Env0Tmp} = var_expr(DefVar, DefNameSym, Env0),
             clj_env:put_local(NameSym, VarExpr, Env0Tmp)
         end,

  OpMeta      = clj_rt:meta(Op),
  IsOnce      = clj_rt:boolean(clj_rt:get(OpMeta, once)),

  %% If this is a var fn then the loop-id should be the function and not
  %% the variable for the named fun.
  {LoopType, LoopId} = case IsDef of
                         true  -> {var, DefNameSym};
                         false -> {fn, NameSym}
                       end,

  %% Remove def_var so that inner fn* are not influenced by it.
  Env1Bis = clj_env:push(#{def_var => ?NIL}, Env1),

  %% If it is a def analyze methods' args but not the body,
  %% we just want arity information first.
  {MethodsExprs, Env2} = analyze_fn_methods( MethodsList
                                           , LoopType
                                           , LoopId
                                           , IsOnce
                                           , not IsDef
                                           , Env1Bis
                                           ),

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

  MinFixedArity = case FixedArities of
                    [] -> ?NIL;
                    _  -> lists:min(FixedArities)
                  end,

  %% Validations
  ?ERROR_WHEN( length(AllVariadics) >= 2
             , <<"Can't have more than 1 variadic overload">>
             , clj_env:location(Env)
             ),

  %% DistinctFixedArities = lists:usort(FixedArities),
  %% ?ERROR_WHEN( length(DistinctFixedArities) =/= length(FixedArities)
  %%                     , <<"Can't have 2 or more overloads "
  %%                         "with the same arity">>
  %%                     , clj_env:location(Env)
  %%                     ),

  ?ERROR_WHEN( IsVariadic andalso
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
        VarMeta0 = #{ 'variadic?'     => IsVariadic
                    , max_fixed_arity => MaxFixedArity
                    , variadic_arity  => VariadicArity
                    , 'fn?'           => true
                    },
        VarMeta1 = clj_rt:merge([clj_rt:meta(DefVar), VarMeta0]),
        DefVar1  = clj_rt:with_meta(DefVar, VarMeta1),

        'clojerl.Namespace':update_var(DefVar1),

        analyze_fn_methods(MethodsList, LoopType, LoopId, IsOnce, true, Env2);
      false ->
        {MethodsExprs, Env2}
    end,

  {TagExpr, Env5} = fetch_type_tag(NameSym, Env4),

  %% A fn can be emitted as an Erlang function when it is
  %% defined with a single fixed arity.
  IsErlangFun = MinFixedArity =:= MaxFixedArity andalso not IsVariadic,

  FnExpr = #{ op              => fn
            , env             => Env
            , form            => List
            , tag             => TagExpr
            , 'variadic?'     => IsVariadic
            , min_fixed_arity => MinFixedArity
            , max_fixed_arity => MaxFixedArity
            , variadic_arity  => VariadicArity
            , 'erlang-fn?'    => IsErlangFun
            , methods         => MethodsExprs1
            , once            => IsOnce
            , local           => LocalExpr
            },

  Env6 = clj_env:remove_locals_scope(Env5),
  Env7 = clj_env:pop(Env6),
  clj_env:push_expr(FnExpr, Env7).

-spec analyze_fn_methods( ['clojerl.List':type()]
                        , loop_type()
                        , loop_id()
                        , boolean()
                        , boolean()
                        , clj_env:env()
                        ) ->
 {[expr()], clj_env:env()}.
analyze_fn_methods(MethodsList, LoopType, LoopId, IsOnce, AnalyzeBody, Env) ->
  MethodEnv = clj_env:put(once, IsOnce, clj_env:put(in_try, false, Env)),
  AnalyzeFnMethodFun = fun(M, EnvAcc) ->
                           analyze_fn_method( M
                                            , LoopType
                                            , LoopId
                                            , AnalyzeBody
                                            , EnvAcc
                                            )
                       end,

  Env1 = lists:foldl(AnalyzeFnMethodFun, MethodEnv, MethodsList),
  clj_env:last_exprs(length(MethodsList), Env1).

-spec analyze_fn_method( 'clojerl.List':type()
                       , loop_type()
                       , loop_id()
                       , boolean()
                       , clj_env:env()
                       ) ->
  clj_env:env().
analyze_fn_method(List, LoopType, LoopId, AnalyzeBody, Env0) ->
  Params = clj_rt:first(List),
  ?ERROR_WHEN( not clj_rt:'vector?'(Params)
             , <<"Parameter declaration should be a vector">>
             , clj_env:location(Env0)
             ),

  ParamsList = clj_rt:to_list(Params),

  ParamsOnly        = lists:filter(fun is_not_ampersand/1, ParamsList),
  IsVariadic        = length(ParamsOnly) =/= length(ParamsList),

  Env1   = clj_env:add_locals_scope(Env0),
  Env2   = clj_env:push(#{pattern_locals => []}, Env1),
  Arity  = length(ParamsOnly),

  Env3  = analyze_method_params(IsVariadic, Arity, ParamsOnly, Env2),
  {ParamsExprs, Env4} = clj_env:last_exprs(Arity, Env3),

  FixedArity  = case IsVariadic of true -> Arity - 1; false -> Arity end,

  {_, Guard, Body} = case AnalyzeBody of
                       true  -> check_guard(clj_rt:rest(List));
                       false -> {false, true, List}
                     end,

  {BodyExpr, Env5} =
    case AnalyzeBody of
      true ->
        LocalExprs = clj_env:get(pattern_locals, [], Env4),
        BodyEnv0   = clj_env:put_locals(lists:reverse(LocalExprs), Env4),
        Mapping    = #{ context     => return
                      , loop_id     => LoopId
                      , loop_type   => LoopType
                      , loop_locals => length(ParamsExprs)
                      },
        BodyEnv1   = clj_env:push(Mapping, BodyEnv0),
        BodyEnv2   = analyze_body(Body, BodyEnv1),
        clj_env:pop_expr(clj_env:pop(BodyEnv2));
      false ->
        {?NIL, Env4}
    end,

  {GuardExpr, Env6} =
    case AnalyzeBody of
      true  -> clj_env:pop_expr(analyze_form(Guard, Env5));
      false -> clj_env:pop_expr(analyze_const(true, Env5))
    end,

  %% TODO: check for a single symbol after '&
  {TagExpr, Env7} = fetch_type_tag(Params, Env6),

  FnMethodExpr = #{ op          => fn_method
                  , env         => Env0
                  , form        => List
                  , tag         => TagExpr
                  , loop_id     => LoopId
                  , loop_type   => LoopType
                  , 'variadic?' => IsVariadic
                  , params      => ParamsExprs
                  , guard       => GuardExpr
                  , fixed_arity => FixedArity
                  , body        => BodyExpr
                  },

  Env8  = clj_env:remove_locals_scope(Env7),
  Env9  = clj_env:pop(Env8),
  clj_env:push_expr(FnMethodExpr, Env9).

-spec is_not_ampersand(any()) -> boolean().
is_not_ampersand(?NIL) -> true;
is_not_ampersand(X)    ->
  not ( clj_rt:'symbol?'(X)
        andalso 'clojerl.Symbol':namespace(X) == ?NIL
        andalso 'clojerl.Symbol':name(X) == <<"&">>
      ).

-spec analyze_method_params(['clojerl.Symbol':type()], clj_env:env()) ->
  clj_env:env().
analyze_method_params(ParamsNames, Env) ->
  analyze_method_params(false, -1, ParamsNames, Env).

-spec analyze_method_params( boolean()
                           , -1 | non_neg_integer()
                           , ['clojerl.Symbol':type()]
                           , clj_env:env()
                           ) -> clj_env:env().
analyze_method_params(IsVariadic, Arity, Params, Env0) ->
  ParamExprFun =
    fun(Pattern, {Id, EnvAcc0}) ->
        {PatExpr, EnvAcc1} = clj_env:pop_expr(parse_pattern(Pattern, EnvAcc0)),
        ParamExpr = #{ op          => binding
                     , env         => Env0
                     , form        => Pattern
                     , tag         => type_tag(PatExpr)
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
  DoSym = clj_rt:symbol(<<"do">>),
  DoForm = clj_rt:cons(DoSym, List),
  analyze_form(DoForm, Env).

-spec is_valid_bind_symbol(any()) -> boolean().
is_valid_bind_symbol(X) ->
  clj_rt:'symbol?'(X)
    andalso not clj_rt:boolean(clj_rt:namespace(X))
    andalso nomatch == re:run('clojerl.Symbol':name(X), <<"\\.">>).

-spec check_guard(any()) -> {boolean(), any(), any()}.
check_guard(List) ->
  Form = clj_rt:first(List),
  case clj_rt:'map?'(Form) of
    true ->
      Ref = make_ref(),
      case clj_rt:get(Form, 'when', Ref) of
        Ref   -> {false, true, List};
        Guard -> {true, Guard, clj_rt:rest(List)}
      end;
    false ->
      {false, true, List}
  end.

%%------------------------------------------------------------------------------
%% Parse do
%%------------------------------------------------------------------------------

-spec parse_do('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_do(Form, Env0) ->
  Statements = clj_rt:to_list(clj_rt:rest(Form)),

  {StatementsList, Return} =
    case Statements of
      [] -> {[], ?NIL};
      _  -> {lists:droplast(Statements), lists:last(Statements)}
    end,

  Env1 = clj_env:push(#{context => statement}, Env0),
  Env2 = analyze_forms(StatementsList, Env1),
  {StatementsExprs, Env3} = clj_env:last_exprs(length(StatementsList), Env2),

  Env4 = clj_env:pop(Env3),
  {ReturnExpr, Env5} = clj_env:pop_expr(analyze_form(Return, Env4)),

  DoExpr = #{ op         => do
            , env        => Env0
            , form       => Statements
            , tag        => type_tag(ReturnExpr)
            , statements => StatementsExprs
            , ret        => ReturnExpr
            },

  clj_env:push_expr(DoExpr, Env5).

%%------------------------------------------------------------------------------
%% Parse if
%%------------------------------------------------------------------------------

-spec parse_if('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_if(Form, Env0) ->
  Count = clj_rt:count(Form),
  ?ERROR_WHEN( Count =/= 3 andalso Count =/= 4
             , [<<"Wrong number of args to if, had: ">>, Count - 1]
             , clj_env:location(Env0)
             ),

  {Test, Then, Else} =
    case Count of
      3 ->
        [_, Test1, Then1] = clj_rt:to_list(Form),
        {Test1, Then1, ?NIL};
      4 ->
        [_, Test1, Then1, Else1] = clj_rt:to_list(Form),
        {Test1, Then1, Else1}
    end,

  Env1 = clj_env:push(#{context => expr}, Env0),
  {TestExpr, Env2} = clj_env:pop_expr(analyze_form(Test, Env1)),

  Env3 = analyze_forms([Then, Else], clj_env:pop(Env2)),
  {[ThenExpr, ElseExpr], Env4} = clj_env:last_exprs(2, Env3),

  TagExpr = case type_tag(ThenExpr) of
              TagExprCase when ?NO_TAG =:= TagExprCase;
                               ?RECUR_TAG =:= TagExprCase ->
                type_tag(ElseExpr);
              TagExprCase ->
                TagExprCase
            end,

  IfExpr = #{ op   => 'if'
            , form => Form
            , env  => Env0
            , test => TestExpr
            , then => ThenExpr
            , else => ElseExpr
            , tag  => TagExpr
            },

  clj_env:push_expr(IfExpr, Env4).

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
  LoopId = clj_rt:gensym(<<"loop_">>),
  Env1   = clj_env:push(#{loop_id => LoopId, loop_type => loop}, Env),

  {LoopExprExtra, Env2} = analyze_let(Form, Env1),
  LoopExpr = maps:merge(#{ op        => loop
                         , form      => Form
                         , env       => Env
                         , loop_id   => LoopId
                         , loop_type => loop
                         },
                        LoopExprExtra),

  clj_env:push_expr(LoopExpr, clj_env:pop(Env2)).

-spec analyze_let('clojerl.List':type(), clj_env:env()) ->
  {map(), clj_env:env()}.
analyze_let(Form, Env0) ->
  validate_bindings(Form, Env0),
  Op     = clj_rt:first(Form),
  IsLoop = clj_rt:equiv(clj_rt:symbol(<<"loop*">>), Op),

  PairUp = fun
             PairUp([], Pairs) ->
               lists:reverse(Pairs);
             PairUp([X, Y | Tail], Pairs) ->
               PairUp(Tail, [{X, Y} | Pairs])
           end,
  BindingsVec   = clj_rt:second(Form),
  BindingsList  = clj_rt:to_list(BindingsVec),
  BindingPairs  = PairUp(BindingsList, []),

  Env1          = clj_env:add_locals_scope(Env0),
  Env1a         = clj_env:push(#{pattern_locals => []}, Env1),

  Env2 = lists:foldl( fun parse_binding/2
                    , clj_env:put(is_loop, IsLoop, Env1a)
                    , BindingPairs
                    ),
  BindingCount = length(BindingPairs),
  {BindingsExprs, Env3} = clj_env:last_exprs(BindingCount, Env2),

  Mapping = case IsLoop of
              true ->
                #{ context     => return
                 , loop_locals => BindingCount
                 };
              false ->
                #{}
            end,

  BodyEnv = clj_env:push(Mapping, Env3),
  Body    = clj_rt:rest(clj_rt:rest(Form)),
  {BodyExpr, Env4} = clj_env:pop_expr(analyze_body(Body, BodyEnv)),

  LetExprExtra = #{ body     => BodyExpr
                  , bindings => BindingsExprs
                  , tag      => type_tag(BodyExpr)
                  },

  Env5 = clj_env:remove_locals_scope(Env4),
  Env6 = clj_env:pop(clj_env:pop(Env5)),
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
              , tag     => type_tag(PatternExpr)
              },

  LocalExprs = clj_env:get(pattern_locals, [], Env2),
  Env3       = clj_env:put_locals(lists:reverse(LocalExprs), Env2),
  clj_env:push_expr(BindExpr, Env3).

-spec validate_bindings('clojerl.List':type(), clj_env:env()) -> ok.
validate_bindings(Form, Env) ->
  Op = clj_rt:first(Form),
  Bindings = clj_rt:second(Form),
  ?ERROR_WHEN( not clj_rt:'vector?'(Bindings),
               [ Op
               , <<" requires a vector for its bindings, had: ">>
               , clj_rt:type(Bindings)
               ]
             , clj_env:location(Env)
             ),

  ?ERROR_WHEN( not clj_rt:'even?'(clj_rt:count(Bindings))
             , [ Op
               , <<" requires an even number of forms in binding "
                   "vector, had: ">>
               , clj_rt:count(Bindings)
               ]
             , clj_env:location(Env)
             ),
  ok.

%%------------------------------------------------------------------------------
%% Parse recur
%%------------------------------------------------------------------------------

-spec parse_recur('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_recur(List, Env) ->
  LoopId     = clj_env:get(loop_id, Env),
  LoopType   = clj_env:get(loop_type, Env),
  LoopLocals = clj_env:get(loop_locals, Env),

  ?ERROR_WHEN( clj_env:context(Env) =/= return
             , <<"Can only recur from tail position">>
             , clj_env:location(Env)
             ),
  ?ERROR_WHEN( LoopLocals =/= clj_rt:count(List) - 1
             , [<<"Mismatched argument count to recur, expected: ">>
               , LoopLocals
               , <<" args, had: ">>
               , clj_rt:count(List) - 1
               ]
             , clj_env:location(Env)
             ),

  Env1     = clj_env:push(#{context => expr}, Env),
  ArgsList = clj_rt:to_list(clj_rt:rest(List)),
  Env2     = analyze_forms(ArgsList, Env1),
  {ArgsExprs, Env3} = clj_env:last_exprs(LoopLocals, Env2),

  RecurExpr = #{ op         => recur
               , env        => Env
               , form       => List
               , exprs      => ArgsExprs
               , loop_id    => LoopId
               , loop_type  => LoopType
               , tag        => ?RECUR_TAG
               },

  clj_env:push_expr(RecurExpr, clj_env:pop(Env3)).

%%------------------------------------------------------------------------------
%% Parse letfn
%%------------------------------------------------------------------------------

-spec parse_letfn('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_letfn(Form, Env0) ->
  [ _ %% letfn*
  , FnSpecs
  | Body
  ] = clj_rt:to_list(Form),

  {FnNames, FnForms} = partition_fun_defs(clj_rt:to_list(FnSpecs)),
  Env = clj_env:add_locals_scope(Env0),
  BindingFun = fun(FnName, {FnNamesExprsAcc, EnvAcc0}) ->
                   {TagExpr, EnvAcc1} = fetch_type_tag(FnName, EnvAcc0),
                   FnNameExpr = #{ op         => local
                                 , env        => Env
                                 , form       => FnName
                                 , tag        => TagExpr
                                 , name       => FnName
                                 , shadow     => clj_env:get_local(FnName, Env)
                                 , underscore => is_underscore(FnName)
                                 , id         => erlang:unique_integer()
                                 },
                   { [FnNameExpr|FnNamesExprsAcc]
                   , clj_env:put_locals([FnNameExpr], EnvAcc1)
                   }
               end,
  {FnNamesExprs, Env1} = lists:foldr(BindingFun, {[], Env}, FnNames),
  {FnsExprs, Env2}     = clj_env:last_exprs( length(FnNames)
                                           , analyze_forms(FnForms, Env1)
                                           ),

  {BodyExpr, Env3} = clj_env:pop_expr(analyze_body(Body, Env2)),

  LetFnExpr = #{ op   => letfn
               , env  => Env
               , form => Form
               , tag  => type_tag(BodyExpr)
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
  Test             = clj_rt:second(List),
  {TestExpr, Env1} = clj_env:pop_expr(analyze_form(Test, Env)),

  PatternsBodies     = clj_rt:rest(clj_rt:rest(List)),
  PatternsBodiesList = clj_rt:to_list(PatternsBodies),

  { ClausesExprs
  , DefaultExpr
  , Env2
  } = parse_patterns_bodies(PatternsBodiesList, Env1),

  BodyExprs = [B || {_, B} <- ClausesExprs],
  Exprs     = case DefaultExpr of
                ?NIL -> BodyExprs;
                _    -> [DefaultExpr | BodyExprs]
              end,

  CaseExpr = #{ op      => 'case'
              , env     => Env
              , form    => List
              , tag     => resolve_exprs_type_tag(Exprs)
              , test    => TestExpr
              , clauses => ClausesExprs
              , default => DefaultExpr
              },

  clj_env:push_expr(CaseExpr, Env2).

-spec parse_patterns_bodies([any()], clj_env:env()) ->
  {[{expr(), expr()}], expr() | ?NIL, clj_env:env()}.
parse_patterns_bodies(PatternsBodies, Env1) ->
  parse_patterns_bodies(PatternsBodies, [], Env1).

-spec parse_patterns_bodies([any()], [any()], clj_env:env()) ->
  {[{expr(), expr()}], expr() | ?NIL, clj_env:env()}.
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
  IsSymbol = clj_rt:'symbol?'(Form),
  IsType   = ?IS_TYPE(Form),
  Mapping  = #{in_pattern => true},
  Env1     = clj_env:push(Mapping, Env),
  Env2     =
    if
      IsSymbol ->
        analyze_symbol(Form, Env1);
      is_map(Form) andalso not IsType ->
        Keys  = maps:keys(Form),
        Vals  = maps:values(Form),
        Count = maps:size(Form),

        InnerEnv0 = lists:foldl(fun parse_pattern/2, Env1, Keys ++ Vals),
        {ValsExprs, InnerEnv1} = clj_env:last_exprs(Count, InnerEnv0),
        {KeysExprs, InnerEnv2} = clj_env:last_exprs(Count, InnerEnv1),

        Ast = #{ op      => erl_map
               , env     => Env
               , form    => Form
               , keys    => KeysExprs
               , vals    => ValsExprs
               , pattern => true
               , tag     => type_expr(Form, Env)
               },
        clj_env:push_expr(Ast, InnerEnv2);
      is_tuple(Form) ->
        Vals = tuple_to_list(Form),

        InnerEnv0 = lists:foldl(fun parse_pattern/2, Env1, Vals),
        {ValsExprs, InnerEnv1} = clj_env:last_exprs(size(Form), InnerEnv0),

        Ast = #{ op      => tuple
               , env     => Env
               , form    => Form
               , tag     => type_expr(Form, Env)
               , items   => ValsExprs
               },
        clj_env:push_expr(Ast, InnerEnv1);
      is_list(Form) ->
        InnerEnv0 = lists:foldl(fun parse_pattern/2, Env1, Form),
        {ValsExprs, InnerEnv1} = clj_env:last_exprs(length(Form), InnerEnv0),

        Ast = #{ op      => erl_list
               , env     => Env
               , form    => Form
               , tag     => type_expr(Form, Env)
               , items   => ValsExprs
               , tail    => ?NIL
               },
        clj_env:push_expr(Ast, InnerEnv1);
      is_number(Form);
      is_boolean(Form);
      is_atom(Form);
      is_binary(Form) ->
        analyze_const(Form, Env1);
      true ->
        case
          clj_rt:'list?'(Form)
          andalso clj_rt:str(clj_rt:first(Form))
        of
          X when X =:= <<"erl-binary*">> orelse
                 X =:= <<"erl-list*">> orelse
                 X =:= <<"erl-alias*">> ->
            analyze_form(Form, Env1);
          _ ->
            ?ERROR( [<<"Invalid pattern: ">>, Form]
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
  VarSymbol0   = clj_rt:second(List),
  SymbolMeta0  = clj_rt:meta(VarSymbol0),
  DocstringMap = case Docstring of
                   ?NIL -> #{};
                   Docstring -> #{doc => Docstring}
                 end,

  LocationInfo = clj_env:location(Env),

  SymbolMeta   = clj_rt:merge([ DocstringMap
                              , SymbolMeta0
                              , LocationInfo
                              ]),
  VarSymbol    = clj_rt:with_meta(VarSymbol0, SymbolMeta),

  Var0         = lookup_var(VarSymbol),

  ?ERROR_WHEN( Var0 =:= ?NIL
             , [ <<"Can't refer to qualified var that doesn't exist: ">>
               , VarSymbol
               ]
             , clj_env:location(Env)
             ),

  VarNsSym     = clj_rt:symbol('clojerl.Var':namespace(Var0)),
  CurrentNs    = 'clojerl.Namespace':current(),
  CurrentNsSym = 'clojerl.Namespace':name(CurrentNs),
  ?ERROR_WHEN( 'clojerl.Symbol':namespace(VarSymbol) =/= ?NIL andalso
               not 'clojerl.Symbol':equiv(CurrentNsSym, VarNsSym)
             , <<"Can't create defs outside of current ns">>
             , clj_env:location(Env)
             ),

  QuoteSym     = clj_rt:symbol(<<"quote">>),
  NameBin      = 'clojerl.Symbol':name(VarSymbol),
  VarMeta      = clj_rt:merge([ clj_rt:meta(Var0)
                              , SymbolMeta
                              , #{ ns   => [QuoteSym, VarNsSym]
                                 , name => [ QuoteSym
                                           , clj_rt:symbol(NameBin)
                                           ]
                                 }
                              ]),
  Var1         = 'clojerl.Var':with_meta(Var0, VarMeta),
  IsDynamic    = 'clojerl.Var':is_dynamic(Var1),

  NoWarnDynamic = clj_compiler:no_warn_dynamic_var_name(Env),
  ?WARN_WHEN( not NoWarnDynamic
              andalso not IsDynamic
              andalso nomatch =/= re:run(NameBin, "\\*.+\\*")
            , [ <<"Warning: ">>
              , NameBin
              , <<" is not dynamic but its name"
                  " suggests otherwise.">>
              ]
            , clj_env:location(Env)
            ),

  'clojerl.Namespace':update_var(Var1),

  Count = clj_rt:count(List),
  Init  = case Docstring of
            ?NIL when Count =:= 3 -> clj_rt:third(List);
            _    when Count =:= 4 -> clj_rt:fourth(List);
            _                     -> ?UNBOUND
          end,

  %% Use Var for def_var that doesn't contain `arglists`, since the value
  %% for `arglists` will be a quoted list until it is evaluated, and this
  %% will make signature_tag/4 fail.
  ExprEnv0 = clj_env:push( #{def_var => Var0, context => expr, in_def => true}
                         , Env
                         ),
  %% Reset locals to avoid resolving symbol that are not bound in the def.
  ExprEnv1 = clj_env:save_locals_scope(ExprEnv0),
  {InitExpr, Env1} = clj_env:pop_expr(analyze_form(Init, ExprEnv1)),

  Var2     = var_fn_info(Var1, InitExpr),
  Var3     = eval_var_meta(Var2, Env1),
  'clojerl.Namespace':update_var(Var3),

  {TagExpr, Env2} = fetch_type_tag( VarSymbol
                                  , clj_env:restore_locals_scope(Env1)
                                  ),

  DefExpr = #{ op      => def
             , env     => Env
             , form    => List
             , name    => VarSymbol
             , var     => Var3
             , init    => InitExpr
             , dynamic => IsDynamic
             , tag     => TagExpr
             },

  clj_env:push_expr(DefExpr, clj_env:pop(Env2)).

-spec eval_var_meta('clojerl.Var':type(), clj_env:env()) ->
  'clojerl.Var':type().
eval_var_meta(Var, Env) ->
  VarMeta0       = 'clojerl.Var':meta(Var),
  Env1           = analyze_form(VarMeta0, Env),
  {Exprs, _Env2} = clj_emitter:emit(Env1),
  VarMeta1       = clj_compiler:eval_expressions(Exprs, true),

  assert_only_literals(Var, VarMeta1, Env),

  'clojerl.Var':with_meta(Var, VarMeta1).

-spec assert_only_literals('clojerl.Var':type(), any(), clj_env:env()) -> any().
assert_only_literals(Var, Value, Env) ->
  ?ERROR_WHEN( not cerl:is_literal_term(Value)
             , [ <<"Metadata for var ">>
               , Var
               , <<" can only contains literals">>
               ]
             , clj_env:location(Env)
             ).

-spec var_fn_info('clojerl.Var':type(), expr()) ->
  'clojerl.Var':type().
var_fn_info(Var, #{op := fn} = Expr) ->
  %% Add information about the associated function
  %% to the var's metadata.
  %% All information should be literal values.
  RemoveKeys = [op, env, methods, form, once, local, tag],
  ExprInfo   = maps:without(RemoveKeys, Expr),
  VarMeta    = clj_rt:meta(Var),
  VarMeta1   = clj_rt:merge([VarMeta, ExprInfo, #{'fn?' => true}]),

  clj_rt:with_meta(Var, VarMeta1);
var_fn_info(Var, _) ->
  Var.

-spec validate_def_args('clojerl.List':type(), clj_env:env()) ->
  ?NIL | binary().
validate_def_args(List, Env) ->
  Docstring =
    case {clj_rt:count(List), clj_rt:third(List)} of
      {4, Str} when is_binary(Str) -> Str;
      _ -> ?NIL
    end,

  case clj_rt:count(List) of
    C when C == 2;
           C == 3, Docstring == ?NIL;
           C == 4, Docstring =/= ?NIL  ->
      ?ERROR_WHEN( not clj_rt:'symbol?'(clj_rt:second(List))
                 , <<"First argument to def must be a symbol">>
                 , clj_env:location(Env)
                 ),
      Docstring;
    1 ->
      ?ERROR( <<"Too few arguments to def">>
            , clj_env:location(Env)
            );
    _ ->
      ?ERROR( <<"Too many arguments to def">>
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
  NsSym        = case clj_rt:namespace(VarSymbol) of
                   ?NIL  -> ?NIL;
                   NsStr -> clj_rt:symbol(NsStr)
                 end,
  NameSym      = clj_rt:symbol('clojerl.Symbol':name(VarSymbol)),

  CurrentNs    = 'clojerl.Namespace':current(),
  CurrentNsSym = 'clojerl.Namespace':name(CurrentNs),

  case 'clojerl.Symbol':equiv(CurrentNsSym, NsSym) of
    Equal when Equal orelse NsSym =:= ?NIL ->
      'clojerl.Namespace':intern(CurrentNs, NameSym),
      lookup_var(VarSymbol, false);
    false ->
      lookup_var(VarSymbol, false)
  end;
lookup_var(VarSymbol, false = _CreateNew) ->
  NsStr   = 'clojerl.Symbol':namespace(VarSymbol),
  NameStr = 'clojerl.Symbol':name(VarSymbol),

  case {NsStr, NameStr} of
    {?NIL, NameStr} when NameStr =:= <<"ns">> orelse
                         NameStr =:= <<"in-ns">> ->
      ClojureCoreSym = clj_rt:symbol(<<"clojure.core">>, NameStr),
      'clojerl.Namespace':find_var(ClojureCoreSym);
    {?NIL, _} ->
      CurrentNs    = 'clojerl.Namespace':current(),
      CurrentNsSym = 'clojerl.Namespace':name(CurrentNs),
      Symbol = clj_rt:symbol('clojerl.Symbol':name(CurrentNsSym), NameStr),
      'clojerl.Namespace':find_var(Symbol);
    {NsStr, NameStr} ->
      Symbol = clj_rt:symbol(NsStr, NameStr),
      'clojerl.Namespace':find_var(Symbol)
  end.

%%------------------------------------------------------------------------------
%% Parse import
%%------------------------------------------------------------------------------

-spec parse_import('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_import(Form, Env) ->
  ArgCount = clj_rt:count(Form) - 1,
  ?ERROR_WHEN( ArgCount > 1
             , [ <<"Wrong number of args to import*, had: ">>
               , ArgCount
               ]
             , clj_env:location(Env)
             ),

  TypeName = clj_rt:second(Form),
  ?ERROR_WHEN( not is_binary(TypeName)
             , [ <<"Argument to import* must be a string, got: ">>
               , TypeName
               ]
             , clj_env:location(Env)
             ),

  NewExpr = #{ op       => import
             , env      => Env
             , form     => Form
             , tag      => type_expr(?NIL, Env)
             , typename => TypeName
             },

  clj_env:push_expr(NewExpr, Env).

%%------------------------------------------------------------------------------
%% Parse new
%%------------------------------------------------------------------------------

-spec parse_new('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_new(Form, Env) ->
  [_, Type | Args]  = clj_rt:to_list(Form),

  {TypeExpr, Env1}  = clj_env:pop_expr(analyze_form(Type, Env)),
  {ArgsExprs, Env2} =
    clj_env:last_exprs(length(Args), analyze_forms(Args, Env1)),

  NewExpr = #{ op   => new
             , env  => Env
             , form => Form
             , type => TypeExpr
             , args => ArgsExprs
             , tag  => TypeExpr
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
  , Protocols
  | OptsAndMethods
  ] = clj_rt:to_list(Form),

  IsNotSeq    = fun(X) -> not clj_rt:'seq?'(X) end,
  {Opts, Methods} = lists:splitwith(IsNotSeq, OptsAndMethods),
  Env0a       = clj_env:push(#{pattern_locals => []}, Env0),

  FieldsList  = clj_rt:to_list(Fields),
  Env1        = analyze_method_params(FieldsList, Env0a),
  {FieldsExprs, Env2} = clj_env:last_exprs(length(FieldsList), Env1),

  %% The analyzer adds the fields to the local scope of the methods,
  %% but it is the emitter who will need to pattern match the first argument
  %% so that they are actually available in the methods' body.
  Env3        = clj_env:add_locals_scope(Env2),
  LocalExprs  = clj_env:get(pattern_locals, [], Env3),
  Env4        = clj_env:put_locals(lists:reverse(LocalExprs), Env3),

  TypeModule  = clj_rt:keyword(TypeSym),
  Type        = 'erlang.Type':?CONSTRUCTOR(TypeModule),

  'clojerl.Namespace':import_type(atom_to_binary(TypeModule, utf8), false),

  %% HACK: by emitting the type we make the module available, which means the
  %% type gets resolved. But we remove all protocols and methods, thus
  %% generating just a dummy erlang module for the type.
  DeftypeDummyExpr = #{ op        => deftype
                      , env       => Env0
                      , form      => Form
                      , tag       => type_expr(?NIL, Env0)
                      , name      => Name
                      , type      => Type
                      , fields    => FieldsExprs
                      , protocols => []
                      , methods   => []
                      , opts      => []
                      },
  _ = clj_emitter:emit(clj_env:push_expr(DeftypeDummyExpr, Env4)),

  Env5 = lists:foldl(fun analyze_deftype_method/2, Env4, Methods),
  {MethodsExprs, Env6} = clj_env:last_exprs(length(Methods), Env5),

  ProtocolsList = clj_rt:to_list(Protocols),
  {ProtocolsExprs, Env7} = clj_env:last_exprs( length(ProtocolsList)
                                             , analyze_forms( ProtocolsList
                                                            , Env6
                                                            )
                                             ),

  DeftypeExpr = #{ op        => deftype
                 , env       => Env0
                 , form      => Form
                 , tag       => type_expr(?NIL, Env0)
                 , name      => Name
                 , type      => Type
                 , fields    => FieldsExprs
                 , protocols => ProtocolsExprs
                 , methods   => MethodsExprs
                 , opts      => Opts
                 },

  Env8 = clj_env:remove_locals_scope(Env7),
  Env9 = clj_env:pop(Env8),
  clj_env:push_expr(DeftypeExpr, Env9).

-spec analyze_deftype_method('clojerl.List':type(), clj_env:env()) ->
  clj_env:env().
analyze_deftype_method(Form, Env) ->
  [MethodName, Args | _Body] = clj_rt:to_list(Form),

  ?ERROR_WHEN( not clj_rt:'symbol?'(MethodName)
             , [ <<"Method name must be a symbol, had: ">>
               , clj_rt:type(MethodName)
               ]
             , clj_env:location(Env)
             ),

  ?ERROR_WHEN( not clj_rt:'vector?'(Args)
             , [ <<"Parameter listing should be a vector, had: ">>
               , clj_rt:type(Args)
               ]
             , clj_env:location(Env)
             ),

  ?ERROR_WHEN( clj_rt:count(Args) < 1
             , [ <<"Must supply at least one argument "
                   "for 'this' in: ">>
               , MethodName
               ]
             , clj_env:location(Env)
             ),

  {MethodExpr, Env1} = clj_env:pop_expr(analyze_fn_method( clj_rt:rest(Form)
                                                         , fn_method
                                                         , MethodName
                                                         , true
                                                         , Env
                                                         )),

  MethodExpr1 = maps:merge( maps:remove('variadic?', MethodExpr)
                          , #{ op   => fn_method
                             , env  => Env
                             , form => Form
                             , tag  => type_tag(MethodExpr)
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
  ] = clj_rt:to_list(List),

  ?ERROR_WHEN( not clj_rt:'symbol?'(FQNameSym)
             , [ <<"Protocol name should be a symbol, had: ">>
               , clj_rt:type(FQNameSym)
               ]
             , clj_env:location(Env)
             ),

  [validate_method_sig(MethodSig, Env) || MethodSig <- MethodsSigs],

  ProtocolExpr = #{ op           => defprotocol
                  , env          => Env
                  , name         => FQNameSym
                  , tag          => type_expr(?NIL, Env)
                  , methods_sigs => MethodsSigs
                  },

  clj_env:push_expr(ProtocolExpr, Env).

-spec validate_method_sig(any(), clj_env:env()) -> ok.
validate_method_sig(MethodSig, Env) ->
  ?ERROR_WHEN( not clj_rt:'vector?'(MethodSig)
             , [ <<"Method signature should be a vector, had: ">>
               , clj_rt:type(MethodSig)
               ]
             , clj_env:location(Env)
             ),

  Name  = clj_rt:first(MethodSig),

  ?ERROR_WHEN( not clj_rt:'symbol?'(Name)
             , [ <<"Method name should be a symbol, had: ">>
               , clj_rt:type(Name)
               ]
             , clj_env:location(Env)
             ),

  Arity = clj_rt:second(MethodSig),

  ?ERROR_WHEN( not clj_rt:'number?'(Arity)
             , [ <<"Method name should be a number, had: ">>
               , clj_rt:type(Arity)
               ]
             , clj_env:location(Env)
             ),

  ok.

%%------------------------------------------------------------------------------
%% Parse extend-type
%%------------------------------------------------------------------------------

-spec parse_extend_type('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_extend_type(List, Env) ->
  [ _ExtendTypeSym % extend-type*
  , Type0
  | ProtosMethods
  ] = clj_rt:to_list(List),

  %% Group each protocol name with its implementation functions.
  GroupedProtoMethods = split_when(ProtosMethods, fun clj_rt:'symbol?'/1),

  %% Analyze each group and map protocols to their implementations.
  {ProtoImplsMap, Env1} = lists:foldl( fun analyze_extend_methods/2
                                     , {#{}, Env}
                                     , GroupedProtoMethods
                                     ),

  Type = case Type0 of
           ?NIL -> clj_rt:symbol(atom_to_binary(?NIL_TYPE, utf8));
           _    ->
             IsSymbol = clj_rt:'symbol?'(Type0),
             case clj_rt:str(Type0) of
               <<"default">> when IsSymbol ->
                 clj_rt:symbol(atom_to_binary(?DEFAULT_TYPE, utf8));
               _  ->
                 Type0
             end
         end,
  { #{op := TypeOp} = TypeExpr
  , Env2
  } = clj_env:pop_expr(analyze_form(Type, Env1)),

  ?ERROR_WHEN( TypeOp =/= type
             , [ <<"The expression of type ">>
               , clj_rt:type(Type)
               , <<" does not resolve to a type.">>
               ]
             , clj_env:location(Env)
             ),

  ExtendTypeExpr = #{ op    => extend_type
                    , env   => Env
                    , form  => List
                    , tag   => type_expr(?NIL, Env)
                    , type  => TypeExpr
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

  ?ERROR_WHEN( ProtoOp =/= type
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
parse_dot(Form, Env) ->
  ?ERROR_WHEN( clj_rt:count(Form) < 3
             , <<"Malformed member expression, expecting "
                 "(. target member ...)">>
             , clj_env:location(Env)
             ),

  [_Dot, Target | Args] = clj_rt:to_list(Form),

  {TargetExpr, Env1} = clj_env:pop_expr(analyze_form(Target, Env)),

  FirstArg = clj_rt:first(Args),
  {NameSym, ArgsList} =
    case clj_rt:'symbol?'(FirstArg) of
      true  ->
        {FirstArg, clj_rt:rest(Args)};
      false ->
        ?ERROR_WHEN( length(Args) > 1
                     orelse not clj_rt:'seq?'(FirstArg)
                   , [ <<"Invalid . expression, expected single list, got: ">>
                     , FirstArg, <<"and: ">>, length(Args), <<" arguments">>
                     ]
                   , clj_env:location(Env)
                   ),
        { clj_rt:first(FirstArg)
        , clj_rt:to_list(clj_rt:rest(FirstArg))
        }
    end,

  {ArgsExprs, Env2} = clj_env:last_exprs( length(ArgsList)
                                        , analyze_forms(ArgsList, Env1)
                                        ),
  {TagExpr, Env3}   = fetch_type_tag(Form, Env2),

  case TargetExpr of
    #{ op   := type
     , type := Type} ->
      Module     = 'erlang.Type':module(Type),
      Function   = clj_rt:keyword(NameSym),
      ErlFunExpr = erl_fun_expr(Form, Module, Function, length(ArgsList), Env),

      InvokeExpr = #{ op   => invoke
                    , env  => Env
                    , form => Form
                    , f    => ErlFunExpr
                    , args => ArgsExprs
                    , tag  => TagExpr
                    },

      clj_env:push_expr(InvokeExpr, Env3);
    _ ->
      Module          = type_tag_module(TargetExpr),
      Function        = clj_rt:keyword(NameSym),

      WarnOnInferVar  = 'clojerl.Var':?CONSTRUCTOR( <<"clojure.core">>
                                                  , <<"*warn-on-infer*">>
                                                  ),
      WarnOnInfer     = clj_rt:deref(WarnOnInferVar),

      ?WARN_WHEN( WarnOnInfer andalso Module =:= ?NO_TAG
                , [<<"Cannot infer target type in ">>, Form]
                , clj_env:location(Env)
                ),

      Count           = length(ArgsExprs) + 1,
      IsExported      = erlang:function_exported(Module, Function, Count),
      ?WARN_WHEN( WarnOnInfer
                  andalso not IsExported
                  andalso Module =/= ?NO_TAG
                , [ <<"Cannot infer target type in ">>, Form
                  , <<" there is no function ">>, NameSym
                  , <<" of arity ">>, Count
                  , <<" for type ">>
                  , 'erlang.Type':?CONSTRUCTOR(Module)
                  ]
                , clj_env:location(Env)
                ),

      FunExpr         = case IsExported of
                          true ->
                            erl_fun_expr(Form, Module, Function, Count, Env);
                          false ->
                            #{ op       => resolve_type
                             , env      => Env
                             , form     => Target
                             , tag      => ?NO_TAG
                             , function => Function
                             }
                        end,

      InvokeExpr      = #{ op   => invoke
                         , env  => Env
                         , form => Form
                         , tag  => TagExpr
                         , f    => FunExpr
                         , args => [TargetExpr | ArgsExprs]
                         },

      clj_env:push_expr(InvokeExpr, Env3)
  end.

%% @doc Adds the type tag of the provided form if it is other than ?NO_TAG
-spec add_type_tag(any(), expr(), clj_env:env()) ->
  {expr(), clj_env:env()}.
add_type_tag(Form, Expr, Env0) ->
  case fetch_type_tag(Form, Env0) of
    {?NO_TAG, Env1} -> {Expr, Env1};
    {TagExpr, Env1} -> {Expr#{tag => TagExpr}, Env1}
  end.

%% @doc Find the type tag for the form. Returns the tag if found or ?NO_TAG
-spec fetch_type_tag(any(), clj_env:env()) ->
  {?NO_TAG | expr(), clj_env:env()}.
fetch_type_tag(Form, Env) ->
  Meta = clj_rt:'meta?'(Form) andalso clj_rt:meta(Form),
  case Meta =/= false andalso clj_rt:get(Meta, tag) of
    X when X =:= false; X =:= ?NIL ->
      {?NO_TAG, Env};
    Tag ->
      resolve_type_tag(Tag, Env)
  end.

-spec resolve_type_tag(any(), clj_env:env()) ->
  {any(), clj_env:env()}.
resolve_type_tag(Tag, Env) ->
  Symbol = case clj_rt:type_module(Tag) of
             'clojerl.Symbol' ->
               Tag;
             'erlang.Type' ->
               Module = 'erlang.Type':module(Tag),
               clj_rt:symbol(atom_to_binary(Module, utf8));
             'clojerl.String' ->
               clj_rt:symbol(Tag);
             _ ->
               ?ERROR( [ <<"Invalid tag, expected Type, ">>
                       , <<"Symbol or String got: ">>
                       , clj_rt:type(Tag)
                       ]
                     , clj_env:location(Env)
                     )
           end,

  do_resolve_type_tag(Symbol, Env).

-spec do_resolve_type_tag('clojerl.Symbol':type(), clj_env:env()) ->
  {any(), clj_env:env()}.
do_resolve_type_tag(Symbol, Env) ->
  Mapping = #{in_pattern => false},
  Env1    = clj_env:push(Mapping, Env),
  Env2    = analyze_form(Symbol, Env1),
  clj_env:pop_expr(clj_env:pop(Env2)).

-spec resolve_exprs_type_tag([expr()]) -> ?NO_TAG | expr().
resolve_exprs_type_tag(Exprs) ->
  FoldFun = fun
              (_Expr, ?NO_TAG) -> ?NO_TAG;
              (Expr, ?NIL)     -> type_tag(Expr);
              (Expr, TagExpr) ->
                case type_tag(Expr) of
                  TagExpr -> TagExpr;
                  _       -> ?NO_TAG
                end
            end,

  lists:foldl(FoldFun, ?NIL, Exprs).

-spec type_tag(expr()) -> ?NO_TAG | expr().
type_tag(Expr) when is_map(Expr) ->
  maps:get(tag, Expr, ?NO_TAG).

-spec type_tag_module(expr()) -> ?NO_TAG | module().
type_tag_module(#{tag := #{op := type, type := Type}}) ->
  'erlang.Type':module(Type);
type_tag_module(_) ->
  ?NO_TAG.

%%------------------------------------------------------------------------------
%% Parse throw
%%------------------------------------------------------------------------------

-spec parse_throw('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_throw(List, Env0) ->
  Count      = clj_rt:count(List),
  ?ERROR_WHEN( Count < 2 orelse Count > 3
             , [<<"Wrong number of args to throw, had: ">>, Count - 1]
             , clj_env:location(Env0)
             ),

  Exception  = clj_rt:second(List),
  Env1       = clj_env:push(#{context => expr}, Env0),

  {ExceptionExpr, Env2}  = clj_env:pop_expr(analyze_form(Exception, Env1)),
  {StacktraceExpr, Env3} = case clj_rt:third(List) of
                             ?NIL -> {?NIL, Env2};
                             Stacktrace ->
                               clj_env:pop_expr(analyze_form(Stacktrace, Env2))
                           end,

  ThrowExpr = #{ op         => throw
               , env        => Env0
               , form       => List
               , tag        => type_tag(ExceptionExpr)
               , exception  => ExceptionExpr
               , stacktrace => StacktraceExpr
               },

  clj_env:push_expr(ThrowExpr, clj_env:pop(Env3)).

%%------------------------------------------------------------------------------
%% Parse try
%%------------------------------------------------------------------------------

-spec parse_try('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_try(List, Env) ->
  CatchSymbol     = clj_rt:symbol(<<"catch">>),
  FinallySymbol   = clj_rt:symbol(<<"finally">>),
  IsCatch         = fun(X) ->
                        clj_rt:'seq?'(X) andalso
                          clj_rt:equiv(clj_rt:first(X), CatchSymbol)
                    end,
  IsFinally       = fun(X) ->
                        clj_rt:'seq?'(X) andalso
                          clj_rt:equiv(clj_rt:first(X), FinallySymbol)
                    end,

  IsNotCatchFinally = fun(X) -> not IsCatch(X) andalso not IsFinally(X) end,

  { Body
  , CatchFinallyTail
  } = lists:splitwith( IsNotCatchFinally
                     , clj_rt:to_list(clj_rt:rest(List))
                     ),
  {Catches, FinallyTail}   = lists:splitwith(IsCatch, CatchFinallyTail),
  {Finallies, Tail}        = lists:splitwith(IsFinally, FinallyTail),

  ?ERROR_WHEN( Tail =/= []
             , <<"Only catch or finally clause can follow catch in "
                 "try expression">>
             , clj_env:location(Env)
             ),

  ?ERROR_WHEN( length(Finallies) > 1
             , <<"Only one finally clause allowed in try expression">>
             , clj_env:location(Env)
             ),

  Mapping          = #{in_try => true, context => expr},
  Env1             = clj_env:push(Mapping, Env),
  {BodyExpr, Env2} = clj_env:pop_expr(analyze_body(Body, Env1)),

  Env3             = clj_env:put(context, expr, Env2),
  {CatchesExprs, Env4} =
    clj_env:last_exprs( length(Catches)
                      , lists:foldl(fun parse_catch/2, Env3, Catches)
                      ),

  Env5             = clj_env:put(context, statement, Env4),
  {FinallyExpr, Env6} = case Finallies of
                          [Finally] ->
                            RestFinally = clj_rt:rest(Finally),
                            clj_env:pop_expr(analyze_body(RestFinally, Env5));
                          _ ->
                            {?NIL, Env5}
                        end,

  TryExpr = #{ op      => 'try'
             , env     => Env
             , form    => List
             , tag     => type_tag(BodyExpr)
             , body    => BodyExpr
             , catches => CatchesExprs
             , finally => FinallyExpr
             },

  clj_env:push_expr(TryExpr, clj_env:pop(Env6)).

%%------------------------------------------------------------------------------
%% Parse catch
%%------------------------------------------------------------------------------

-spec parse_catch('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_catch(List, Env0) ->
  [ _ %% catch
  , ErrType
  , ErrName
  | RestList
  ] = clj_rt:to_list(List),

  ?ERROR_WHEN( not is_valid_error_type(ErrType)
             , [<<"Bad error type: ">>, ErrType]
             , clj_env:location(Env0)
             ),

  Env1 = clj_env:push(#{in_try => false, pattern_locals => []}, Env0),

  {TypeExpr,    Env2} = parse_catch_type(ErrType, Env1),
  {ErrNameExpr, Env3} = parse_catch_var(TypeExpr, ErrName, Env2),
  {StackExpr, GuardAndBody, Env4} = parse_catch_stack(RestList, Env3),
  NameExpr = #{ op      => binding
              , env     => Env0
              , form    => ErrName
              , pattern => ErrNameExpr
              , tag     => type_tag(ErrNameExpr)
              },

  LocalExprs = clj_env:get(pattern_locals, [], Env4),
  Env5      = clj_env:put_locals(LocalExprs, clj_env:add_locals_scope(Env4)),

  {_, Guard, Body}  = check_guard(GuardAndBody),
  {GuardExpr, Env6} = clj_env:pop_expr(analyze_form(Guard, Env5)),
  {BodyExpr, Env7}  = clj_env:pop_expr(analyze_body(Body, Env6)),

  CatchExpr = #{ op         => 'catch'
               , env        => Env0
               , form       => List
               , tag        => type_tag(BodyExpr)
               , class      => TypeExpr
               , local      => NameExpr
               , stacktrace => StackExpr
               , guard      => GuardExpr
               , body       => BodyExpr
               },

  Env8 = clj_env:pop(clj_env:remove_locals_scope(Env7)),
  clj_env:push_expr(CatchExpr, Env8).

-spec parse_catch_type(atom() | 'clojerl.Symbol':type(), clj_env:env()) ->
  {expr(), clj_env:env()}.
parse_catch_type(ErrType, Env0) ->
  case clj_rt:'symbol?'(ErrType) andalso resolve(ErrType, Env0) of
    {{type, Type}, Env1} ->
      IError     = 'erlang.Type':?CONSTRUCTOR('clojerl.IError'),
      TypeModule = 'erlang.Type':module(Type),
      ?ERROR_WHEN( not 'clojerl.IError':?EXTENDS(TypeModule)
                 , [<<"Type ">>, Type, <<" does not implement ">>, IError]
                 , clj_env:location(Env0)
                 ),
      {type_expr(Type, ErrType, Env1), Env1};
    _ ->
      ?WARN_WHEN( not is_atom(ErrType)
                  andalso starts_with_uppercase(clj_rt:str(ErrType))
                , [ <<"The symbol ">>, ErrType
                  , <<" could not be resolved as a type. ">>
                  , <<"If meant to be a binding symbol, start its name ">>
                  , <<"with lower case to remove this warning">>
                  ]
                , clj_env:location(Env0)
                ),
      {ErrTypeExpr, Env1} = clj_env:pop_expr(parse_pattern(ErrType, Env0)),
      TypeLocal = #{ op      => binding
                   , env     => Env0
                   , form    => ErrType
                   , tag     => type_tag(ErrTypeExpr)
                   , pattern => ErrTypeExpr
                   },
      {TypeLocal, Env1}
  end.

-spec starts_with_uppercase(binary()) -> boolean().
starts_with_uppercase(<<First/utf8, _/binary>>) ->
  $A =< First andalso First =< $Z.

-spec parse_catch_var(expr(), any(), clj_env:env()) ->
  {expr(), clj_env:env()}.
parse_catch_var(#{op := binding}, ErrName, Env) ->
  clj_env:pop_expr(parse_pattern(ErrName, Env));
parse_catch_var(#{op := type, type := Type}, ErrName, Env0) ->
  ?ERROR_WHEN( not is_valid_bind_symbol(ErrName)
             , [ <<"Expected valid binding symbol "
                   "when error type is specified, got: ">>
               , ErrName
               ]
             , clj_env:location(Env0)
             ),

  {VariableExpr, Env1} = clj_env:pop_expr(parse_pattern(ErrName, Env0)),

  TypeModule           = 'erlang.Type':module(Type),
  MapExpr              = #{?TYPE => TypeModule},
  {PatternExpr0, Env2} = clj_env:pop_expr(analyze_erl_map(MapExpr, Env1)),
  PatternExpr1         = PatternExpr0#{pattern => true},

  AliasExpr            = #{ op       => erl_alias
                          , env      => Env0
                          , form     => ErrName
                          , tag      => type_tag(ErrName)
                          , variable => VariableExpr
                          , pattern  => PatternExpr1
                          },

  {AliasExpr, Env2}.

-spec parse_catch_stack([any()], clj_env:env()) ->
  {expr() | ?NIL, [any()], clj_env:env()}.
parse_catch_stack([stack, StackName | GuardAndBody], Env0) ->
  {StackNameExpr, Env1} = clj_env:pop_expr(parse_pattern(StackName, Env0)),
  StackExpr = #{ op      => binding
               , env     => Env0
               , form    => StackName
               , pattern => StackNameExpr
               , tag     => type_tag(StackNameExpr)
               },
  {StackExpr, GuardAndBody, Env1};
parse_catch_stack(GuardAndBody, Env) ->
  {?NIL, GuardAndBody, Env}.

-spec is_valid_error_type(any()) -> boolean().
is_valid_error_type(error) -> true;
is_valid_error_type(exit)  -> true;
is_valid_error_type(throw) -> true;
is_valid_error_type(Form)  -> clj_rt:'symbol?'(Form).

%%------------------------------------------------------------------------------
%% Parse var
%%------------------------------------------------------------------------------

-spec parse_var('clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_var(List, Env) ->
  Count = clj_rt:count(List),
  ?ERROR_WHEN( Count =/= 2
             , [<<"Wrong number of args to var, had: ">>, Count]
             , clj_env:location(Env)
             ),

  VarSymbol = clj_rt:second(List),

  case resolve(VarSymbol, false, Env) of
    {{var, Var}, Env1} ->
      VarConstExpr = #{ op   => constant
                      , env  => Env
                      , form => Var
                      , tag  => type_expr(Var, Env)
                      },
      clj_env:push_expr(VarConstExpr, Env1);
    {_, _} ->
      ?ERROR([ <<"Unable to resolve var: ">>
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
  FSym          = clj_rt:first(Form),
  {FExpr, Env1} = clj_env:pop_expr(analyze_form(FSym, Env)),

  Args          = clj_rt:to_list(clj_rt:rest(Form)),
  ArgCount      = length(Args),
  {ArgsExpr, Env2} = clj_env:last_exprs( ArgCount
                                       , analyze_forms(Args, Env1)
                                       ),

  {FormTagExpr, Env3} = fetch_type_tag(Form, Env2),
  {TagExpr, Env4} = case FExpr of
                      _ when FormTagExpr =/= ?NO_TAG ->
                        {FormTagExpr, Env3};
                      #{op := fn, tag := Tag0} ->
                        {Tag0, Env3};
                      #{op := var, var := Var, tag := Tag0} ->
                        signature_tag(ArgCount, Tag0, Var, Env3);
                      _   ->
                        {?NO_TAG, Env3}
                    end,

  InvokeExpr0 = #{ op   => invoke
                 , env  => Env
                 , form => Form
                 , f    => FExpr#{arity => ArgCount}
                 , args => ArgsExpr
                 , tag  => TagExpr
                 },

  {InvokeExpr1, Env5} = add_type_tag(Form, InvokeExpr0, Env4),

  clj_env:push_expr(InvokeExpr1, Env5).

-spec signature_tag( integer()
                   , expr() | ?NO_TAG
                   , 'clojerl.Var':type()
                   , clj_env:env()
                   ) ->
  {expr() | ?NO_TAG, clj_env:env()}.
signature_tag(ArgCount, Default, Var, Env) ->
  ArgLists = clj_rt:to_list(clj_rt:get(clj_rt:meta(Var), arglists)),
  Fun = fun(ArgList) ->
            ArgsOnly = lists:filter( fun is_not_ampersand/1
                                   , clj_rt:to_list(ArgList)
                                   ),
            clj_rt:count(ArgsOnly) =:= ArgCount
        end,
  case lists:filter(Fun, ArgLists) of
    [ArgList] -> fetch_type_tag(ArgList, Env);
    %% This also handles the case where there are multiple clauses for the
    %% same arity.
    _         -> {Default, Env}
  end.

%%------------------------------------------------------------------------------
%% Analyze symbol
%%------------------------------------------------------------------------------

-spec analyze_symbol('clojerl.Symbol':type(), clj_env:env()) -> clj_env:env().
analyze_symbol(Symbol, Env0) ->
  InPattern     = clj_env:get(in_pattern, false, Env0),
  {Expr0, Env1} = do_analyze_symbol(InPattern, Symbol, Env0),
  {Expr1, Env2} = add_type_tag(Symbol, Expr0, Env1),
  clj_env:push_expr(Expr1, Env2).

-spec do_analyze_symbol(boolean(), 'clojerl.Symbol':t(), clj_env:env()) ->
  {expr(), clj_env:env()}.
do_analyze_symbol(true = _InPattern, Symbol, Env0) ->
  ?ERROR_WHEN( not is_valid_bind_symbol(Symbol)
             , [<<"Not a valid binding symbol, had: ">>, Symbol]
             , clj_env:location(Env0)
             ),

  Expr0 = #{ op         => local
           , env        => Env0
           , form       => Symbol
           , tag        => ?NO_TAG
           , name       => Symbol
           , shadow     => clj_env:get_local(Symbol, Env0)
           , underscore => is_underscore(Symbol)
           , id         => erlang:unique_integer()
           },
  %% We need to do this here so that the registered local has a type tag.
  {Expr1, Env1} = add_type_tag(Symbol, Expr0, Env0),
  {Expr1, add_pattern_local(Expr1, Env1)};
do_analyze_symbol(false = _InPattern, Symbol, Env0) ->
  case resolve(Symbol, Env0) of
    {?NIL, _} ->
      ?ERROR([ <<"Unable to resolve symbol: ">>, Symbol
             , <<" in this context">>
             , maybe_def_context_message(Env0)
             ]
            , clj_env:location(Env0)
            );
    {{local, Local}, Env1} ->
      {Local#{env => Env0}, Env1};
    {{erl_fun, FunExpr}, Env1} ->
      {FunExpr, Env1};
    {{var, Var}, Env1} ->
      var_expr(Var, Symbol, Env1);
    {{type, Type}, Env1} ->
      TypeExpr = type_expr(Type, Symbol, Env1),
      {TypeExpr, Env1}
  end.

-spec maybe_def_context_message(clj_env:env()) -> binary().
maybe_def_context_message(Env) ->
  case clj_env:get(in_def, Env) of
    true -> <<" (check usage of symbols bound outside def)">>;
    _ -> <<"">>
  end.

-spec is_underscore('clojerl.Symbol':type()) -> boolean().
is_underscore(Symbol) ->
  'clojerl.Symbol':name(Symbol) =:= <<"_">>.

-spec erl_fun_expr( 'clojerl.Symbol':type()
                  , module()
                  , atom()
                  , arity()
                  , clj_env:env()
                  ) -> expr().
erl_fun_expr(Symbol, Module, Function, Arity, Env) ->
  #{ op       => erl_fun
   , env      => Env
   , form     => Symbol
   , tag      => ?NO_TAG
   , module   => Module
   , function => Function
   , arity    => Arity
   }.

-spec var_expr('clojerl.Var':type(), 'clojerl.Symbol':type(), clj_env:env()) ->
  {expr(), clj_env:env()}.
var_expr(Var, Symbol, Env0) ->
  {TagExpr, Env1} = fetch_type_tag(Var, Env0),
  VarExpr         = #{ op         => var
                     , env        => Env0
                     , form       => Symbol
                     , name       => Symbol
                     , var        => Var
                     , is_dynamic => 'clojerl.Var':is_dynamic(Var)
                     , tag        => TagExpr
                     },
  {VarExpr, Env1}.

-spec type_expr( 'clojerl.Symbol':type()
               , 'clojerl.Symbol':type()
               , clj_env:env()
               ) ->
  expr().
type_expr(Type, Symbol, Env) ->
 #{ op   => type
  , env  => Env
  , form => Symbol
  , tag  => 'erlang.Type':?CONSTRUCTOR('erlang.Type')
  , type => Type
  }.

-spec type_expr(any(), clj_env:env()) -> expr().
type_expr(Value, Env) ->
  Type       = clj_rt:type(Value),
  TypeModule = 'erlang.Type':module(Type),
  TypeSym    = clj_rt:symbol(atom_to_binary(TypeModule, utf8)),
  type_expr(Type, TypeSym, Env).

-spec resolve('clojerl.Symbol':env(), clj_env:env()) ->
  { {var, 'clojerl.Var':type()}
    | {erl_fun, erl_fun_expr()}
    | {local, expr()}
    | {type, 'erlang.Type':type()}
    | ?NIL
  , clj_env:env()
  }.
resolve(Symbol, Env) ->
  resolve(Symbol, true, Env).

-spec resolve('clojerl.Symbol':env(), boolean(), clj_env:env()) ->
  { {var , 'clojerl.Var':type()}
    | {erl_fun, erl_fun_expr()}
    | {local, expr()}
    | {type, 'erlang.Type':type()}
    | ?NIL
  , clj_env:env()
  }.
resolve(Symbol, CheckPrivate, Env) ->
  CurrentNs = 'clojerl.Namespace':current(),
  Local     = clj_env:get_local(Symbol, Env),
  NsStr     = 'clojerl.Symbol':namespace(Symbol),
  MappedVal = 'clojerl.Namespace':find_mapping(CurrentNs, Symbol),

  if
    Local =/= ?NIL ->
      {{local, Local}, Env};
    MappedVal =/= ?NIL ->
      case clj_rt:'var?'(MappedVal) of
        true ->
          CurrentNsSym  = 'clojerl.Namespace':name(CurrentNs),
          CurrentNsName = 'clojerl.Symbol':name(CurrentNsSym),
          ?ERROR_WHEN( CheckPrivate
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
      Expr = erl_fun(Symbol, Env),
      {{erl_fun, Expr}, Env};
    true ->
      case is_maybe_type(Symbol) of
        true  ->
          TypeModule = clj_rt:keyword(Symbol),
          Type = 'erlang.Type':?CONSTRUCTOR(TypeModule),
          {{type, Type}, Env};
        false -> {?NIL, Env}
      end
  end.

-spec erl_fun('clojerl.Symbol':type(), clj_env:env()) -> erl_fun_expr().
erl_fun(Symbol, Env0) ->
  {Ns0, Name, Arity} = clj_utils:parse_erl_fun(Symbol),
  NameAtom           = binary_to_atom(Name, utf8),
  NsSym              = clj_rt:symbol(Ns0),
  {NsAtom, Env}     = case resolve(NsSym, Env0) of
                         {{type, Type}, EnvTmp} ->
                           {'erlang.Type':module(Type), EnvTmp};
                         {_, EnvTmp} ->
                           { binary_to_atom('clojerl.Symbol':name(NsSym), utf8)
                           , EnvTmp
                           }
                       end,

  erl_fun_expr(Symbol, NsAtom, NameAtom, Arity, Env).

-spec is_maybe_type('clojerl.Symbol':type()) -> boolean().
is_maybe_type(Symbol) ->
  ?NIL = 'clojerl.Symbol':namespace(Symbol),
  Name = 'clojerl.Symbol':name(Symbol),
  Re   = <<"([a-z]\\w*\\.)+[A-Za-z]\\w*">>,
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

-spec wrapping_meta(expr(), clj_env:env()) -> clj_env:env().
wrapping_meta(#{form := Form, tag := TagExpr} = Expr, Env) ->
  Meta = case clj_rt:'meta?'(Form) of
           true  -> clj_reader:remove_location(clj_rt:meta(Form));
           false -> ?NIL
         end,
  case Meta of
    Meta when Meta =/= ?NIL andalso Meta =/= #{} ->
      {MetaExpr, Env1} = clj_env:pop_expr(analyze_form(Meta, Env)),
      WithMetaExpr = #{ op   => with_meta
                      , env  => Env
                      , form => Form
                      , tag  => TagExpr
                      , meta => MetaExpr
                      , expr => Expr
                      },
      clj_env:push_expr(WithMetaExpr, Env1);
    Meta ->
      clj_env:push_expr(Expr, Env)
  end.

%%------------------------------------------------------------------------------
%% Analyze vector
%%------------------------------------------------------------------------------

-spec analyze_vector('clojerl.Vector':type(), clj_env:env()) -> clj_env:env().
analyze_vector(Vector, Env0) ->
  Count   = clj_rt:count(Vector),
  Env1    = clj_env:push(#{context => expr}, clear_def_var(Env0)),
  Items   = clj_rt:to_list(Vector),
  Env2    = analyze_forms(Items, Env1),
  {ItemsExpr, Env3} = clj_env:last_exprs(Count, Env2),

  VectorExpr = #{ op    => vector
                , env   => Env0
                , form  => Vector
                , tag   => type_expr(Vector, Env0)
                , items => ItemsExpr
                },

  wrapping_meta(VectorExpr, clj_env:pop(Env3)).

%%------------------------------------------------------------------------------
%% Analyze map
%%------------------------------------------------------------------------------

-spec analyze_map('clojerl.Map':type(), clj_env:env()) -> clj_env:env().
analyze_map(Map, Env0) ->
  Keys  = clj_rt:to_list('clojerl.Map':keys(Map)),
  Vals  = clj_rt:to_list('clojerl.Map':vals(Map)),

  Count = clj_rt:count(Map),
  Env1  = clj_env:push(#{context => expr}, clear_def_var(Env0)),

  Env2  = analyze_forms(Keys, Env1),
  {KeysExpr, Env3} = clj_env:last_exprs(Count, Env2),
  Env4  = analyze_forms(Vals, Env3),
  {ValsExpr, Env5} = clj_env:last_exprs(Count, Env4),

  MapExpr = #{ op   => map
             , env  => Env0
             , form => Map
             , tag  => type_expr(Map, Env0)
             , keys => KeysExpr
             , vals => ValsExpr
             },

  wrapping_meta(MapExpr, clj_env:pop(Env5)).

%%------------------------------------------------------------------------------
%% Analyze Erlang map
%%------------------------------------------------------------------------------

-spec analyze_erl_map(map(), clj_env:env()) -> clj_env:env().
analyze_erl_map(Map, Env0) ->
  Keys  = maps:keys(Map),
  Vals  = maps:values(Map),

  Count = maps:size(Map),
  Env1  = clj_env:push(#{context => expr}, clear_def_var(Env0)),

  Env2 = analyze_forms(Keys, Env1),
  {KeysExpr, Env3} = clj_env:last_exprs(Count, Env2),
  Env4 = analyze_forms(Vals, Env3),
  {ValsExpr, Env5} = clj_env:last_exprs(Count, Env4),

  MapExpr = #{ op      => erl_map
             , env     => Env0
             , form    => Map
             , tag     => type_expr(Map, Env0)
             , keys    => KeysExpr
             , vals    => ValsExpr
             , pattern => false
             },

  wrapping_meta(MapExpr, clj_env:pop(Env5)).

%%------------------------------------------------------------------------------
%% Analyze set
%%------------------------------------------------------------------------------

-spec analyze_set('clojerl.Set':type(), clj_env:env()) -> clj_env:env().
analyze_set(Set, Env0) ->
  Env1    = clj_env:push(#{context => expr}, clear_def_var(Env0)),
  Items   = clj_rt:to_list(Set),
  Env2    = analyze_forms(Items, Env1),

  Count   = clj_rt:count(Set),
  {ItemsExpr, Env3} = clj_env:last_exprs(Count, Env2),

  SetExpr = #{ op    => set
             , env   => Env0
             , form  => Set
             , items => ItemsExpr
             , tag   => type_expr(Set, Env0)
             },

  wrapping_meta(SetExpr, clj_env:pop(Env3)).

%%------------------------------------------------------------------------------
%% Analyze tuple
%%------------------------------------------------------------------------------

-spec analyze_tuple('erlang.Tuple':type(), clj_env:env()) ->
  clj_env:env().
analyze_tuple(Tuple, Env0) ->
  Env1    = clj_env:push(#{context => expr}, clear_def_var(Env0)),
  Items   = erlang:tuple_to_list(Tuple),
  Env2    = analyze_forms(Items, Env1),

  Count   = erlang:tuple_size(Tuple),
  {ItemsExpr, Env3} = clj_env:last_exprs(Count, Env2),

  TupleExpr = #{ op    => tuple
               , env   => Env0
               , form  => Tuple
               , tag   => type_expr(Tuple, Env0)
               , items => ItemsExpr
               },

  clj_env:push_expr(TupleExpr, clj_env:pop(Env3)).

%%------------------------------------------------------------------------------
%% receive
%%------------------------------------------------------------------------------

-spec parse_receive(any(), clj_env:env()) ->
  clj_env:env().
parse_receive(List, Env) ->
  [ _ %% receive*
  | ClausesAndAfter
  ] = clj_rt:to_list(List),

  AfterSymbol = clj_rt:symbol(<<"after">>),
  IsNotAfter  = fun(X) ->
                    not (clj_rt:'seq?'(X)
                         andalso clj_rt:equiv(clj_rt:first(X), AfterSymbol))
                end,

  {Clauses, Afters} = lists:splitwith(IsNotAfter, ClausesAndAfter),

  ?ERROR_WHEN( length(Afters) > 1
             , <<"Only one after clause allowed in "
                 "receive expression">>
             , clj_env:location(Env)
             ),

  {ClausesExprs, DefaultExpr, Env1} = parse_patterns_bodies(Clauses, Env),

  ?ERROR_WHEN( DefaultExpr =/= ?NIL
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

  BodyExprs   = [B || {_, B} <- ClausesExprs],
  ReceiveExpr = #{ op      => 'receive'
                 , env     => Env
                 , form    => List
                 , tag     => resolve_exprs_type_tag(BodyExprs)
                 , clauses => ClausesExprs
                 , 'after' => AfterExpr
                 },

  clj_env:push_expr(ReceiveExpr, Env2).

-spec parse_after(any(), clj_env:env()) -> clj_env:env().
parse_after(List, Env) ->
  [ _ %% after
  , Timeout
  | Body
  ] = clj_rt:to_list(List),

  {TimeoutExpr, Env1} = clj_env:pop_expr(analyze_form(Timeout, Env)),
  {BodyExpr, Env2}    = clj_env:pop_expr(analyze_body(Body, Env1)),

  AfterExpr = #{ op      => 'after'
               , env     => Env
               , form    => List
               , tag     => type_tag(BodyExpr)
               , timeout => TimeoutExpr
               , body    => BodyExpr
               },

  clj_env:push_expr(AfterExpr, Env2).

%%------------------------------------------------------------------------------
%% Erlang fun
%%------------------------------------------------------------------------------

-spec parse_erlang_fun(any(), clj_env:env()) ->
  clj_env:env().
parse_erlang_fun(List, Env0) ->
  [ _ %% erl-fun*
  | Args
  ] = clj_rt:to_list(List),

  ?ERROR_WHEN( 1 > length(Args) orelse length(Args) > 3
             , [ <<"Expected 1, 2 or 3 arguments for erl-fun*, got: ">>
               , length(Args)
               ]
             , clj_env:location(Env0)
             ),

  {Module, Function, Arity} = case Args of
                                [F] ->
                                  {?NIL, F, ?NIL};
                                [F, A] when is_integer(A) ->
                                  {?NIL, F, A};
                                [M, F] ->
                                  {M, F, ?NIL};
                                [M, F, A] ->
                                  {M, F, A}
                              end,

  ?ERROR_WHEN( Module =/= ?NIL andalso not is_atom(Module)
             , [ <<"Module must be a keyword, got: ">>
               , clj_rt:type(Module)
               ]
             , clj_env:location(Env0)
                      ),

  ?ERROR_WHEN( not is_atom(Function)
             , [ <<"Function must be a keyword, got: ">>
               , clj_rt:type(Function)
               ]
             , clj_env:location(Env0)
             ),

  ?ERROR_WHEN( Arity =/= ?NIL andalso not is_integer(Arity)
             , [ <<"Arity must be an integer or nil, got: ">>
               , clj_rt:type(Arity)
               ]
             , clj_env:location(Env0)
             ),

  ErlFunExpr = erl_fun_expr(List, Module, Function, Arity, Env0),

  clj_env:push_expr(ErlFunExpr, Env0).

%%------------------------------------------------------------------------------
%% Erlang binary
%%------------------------------------------------------------------------------

-spec parse_erlang_binary(any(), clj_env:env()) ->
  clj_env:env().
parse_erlang_binary(List, Env0) ->
  [ _ %% erl-binary*
  | Segments
  ] = clj_rt:to_list(List),

  Env1 = lists:foldl(fun parse_segment/2, Env0, Segments),
  {SegmentsExprs, Env2} = clj_env:last_exprs(length(Segments), Env1),

  BinaryExpr = #{ op       => erl_binary
                , env      => Env0
                , form     => List
                , tag      => type_expr(<<>>, Env0)
                , segments => SegmentsExprs
                },

  clj_env:push_expr(BinaryExpr, Env2).

-spec parse_segment(any(), clj_env:env()) ->
  clj_env:env().
parse_segment(Segment0, Env0) ->
  Segment = segment_to_list(Segment0),
  ?ERROR_WHEN( not is_list(Segment)
               orelse Segment =:= []
             , [<<"Invalid segment: ">>, Segment]
             , clj_env:location(Env0)
             ),

  [Value | Rest] = Segment,
  Config0 = 'clojerl.Map':?CONSTRUCTOR(Rest),
  Config  = clj_rt:'->erl'(Config0, true),
  Type    = maps:get(type, Config, integer),

  {ValueExpr, Env1} = parse_segment_value(Value, Env0),
  {TypeExpr, Env2}  = parse_segment_type(Config, Env1),
  {SizeExpr, Env3}  = parse_segment_size(Config, Type, Env2),
  {UnitExpr, Env4}  = parse_segment_unit(Config, Type, Env3),
  {FlagsExpr, Env5} = parse_segment_flags(Config, Env4),

  SegmentExpr = #{ op    => binary_segment
                 , env   => Env0
                 , form  => Segment
                 , tag   => ?NO_TAG
                 , value => ValueExpr
                 , size  => SizeExpr
                 , unit  => UnitExpr
                 , type  => TypeExpr
                 , flags => FlagsExpr
                 },

  clj_env:push_expr(SegmentExpr, Env5).

-spec segment_to_list(any()) -> [any()] | invalid.
segment_to_list(Binary) when is_binary(Binary) ->
  [Binary, type,  binary];
segment_to_list(Integer) when is_number(Integer) ->
  [Integer, type, integer];
segment_to_list(#{?TYPE := _} = X) ->
  IsVector = clj_rt:'vector?'(X),
  IsSymbol = clj_rt:'symbol?'(X),
  if
    IsVector -> clj_rt:to_list(X);
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
  ?ERROR_WHEN( not lists:member(Type, valid_segment_types())
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
  Flags1 = clj_rt:to_list(Flags0),
  ?ERROR_WHEN( not lists:all(fun is_valid_segment_flag/1, Flags1)
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
  ] = clj_rt:to_list(List),

  {Items, Tail} = lists:splitwith(fun is_not_ampersand/1, AllItems),

  ?ERROR_WHEN( length(Tail) > 0 andalso length(Tail) =/= 2
             , [<<"There has to be one expression after &, got ">>
               , length(Tail)
               ]
             , clj_env:location(Env0)
             ),

  {ItemsExprs, Env1} = clj_env:last_exprs( length(Items)
                                         , analyze_forms(Items, Env0)
                                         ),
  {TailExpr, Env2}   = case Tail of
                         [_, TailItem] ->
                           clj_env:pop_expr(analyze_form(TailItem, Env1));
                         [] ->
                           {?NIL, Env1}
                       end,

  ListExpr = #{ op      => erl_list
              , env     => Env0
              , form    => List
              , tag     => type_expr([], Env0)
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
  ] = clj_rt:to_list(List),

  ?ERROR_WHEN( not clj_env:get(in_pattern, false, Env0)
             , [<<"Alias not in pattern.">>]
             , clj_env:location(Env0)
             ),

  ?ERROR_WHEN( length(Args) =/= 2
             , [ <<"Expected only 2 arguments for erl-alias*, got: ">>
               , length(Args)
               ]
             , clj_env:location(Env0)
             ),

  [Variable, Pattern]  = Args,

  ?ERROR_WHEN( not is_valid_bind_symbol(Variable)
             , [ <<"First argument to erl-alias* "
                   "should be a valid binding symbol, got: ">>
               , Variable
               ]
             , clj_env:location(Env0)
             ),

  {VariableExpr, Env1} = clj_env:pop_expr(parse_pattern(Variable, Env0)),
  {PatternExpr,  Env2} = clj_env:pop_expr(parse_pattern(Pattern, Env1)),

  MatchExpr   = #{ op       => erl_alias
                 , env      => Env0
                 , form     => List
                 , tag      => type_tag(VariableExpr)
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
  Body            = clj_rt:rest(List),
  {BodyExpr, Env} = clj_env:pop_expr(analyze_body(Body, Env0)),

  Expr = #{ op    => on_load
          , env   => Env0
          , form  => List
          , body  => BodyExpr
          , tag   => ?NO_TAG
          },

  clj_env:push_expr(Expr, Env).
