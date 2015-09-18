-module(clj_analyzer).

-export([
         analyze/2,
         macroexpand/2,
         macroexpand_1/2,
         is_special/1
        ]).

-import(clj_core,
        ['symbol?'/1,
         first/1, second/1, third/1, fourth/1, rest/1,
         count/1,
         deref/1,
         name/1, namespace/1,
         meta/1,
         keyword/1, symbol/1, symbol/2,
         boolean/1,
         get/2,
         type/1]).

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
  Op = first(Form),
  {MacroVar, Env} = lookup_var(Op, false, Env),
  case
    is_special(Op)
    orelse (not 'symbol?'(Op))
    orelse (MacroVar == undefined)
    orelse (not 'clojerl.Var':is_macro(MacroVar))
  of
    true -> Form;
    false ->
      {MacroVar, Env} = lookup_var(Op, false, Env),
      Fun = deref(MacroVar),
      Args = [Form, Env, rest(Form)],
      erlang:apply(Fun, Args)
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
  #{
     symbol(<<"ns">>) => fun parse_ns/2,
     symbol(<<"def">>) => fun parse_def/2,
     symbol(<<"quote">>) => fun parse_quote/2,
     symbol(<<"loop*">>) => undefined,
     symbol(<<"recur">>) => undefined,
     symbol(<<"if">>) => undefined,
     symbol(<<"case*">>) => undefined,
     symbol(<<"let*">>) => undefined,
     symbol(<<"letfn*">>) => undefined,
     symbol(<<"do">>) => undefined,
     symbol(<<"fn*">>) => undefined,
     symbol(<<"var">>) => undefined,
     symbol(<<"import*">>) => undefined,
     symbol(<<"deftype*">>) => undefined,
     symbol(<<"reify*">>) => undefined,
     symbol(<<"try">>) => undefined,
     %% symbol(<<"monitor-enter">>),
     %% symbol(<<"monitor-exit">>),
     %% symbol(<<"new">>),
     %% symbol(<<"&">>),
     symbol(<<"throw">>) => undefined,
     symbol(<<"catch">>) => undefined,
     symbol(<<"finally">>) => undefined
   }.

-spec analyze_form(clj_env:env(), any()) -> clj_env:env().
analyze_form(Env, Form) ->
  case type(Form) of
    'clojerl.Symbol' ->
      analyze_symbol(Env, Form);
    'clojerl.List' ->
      Op = first(Form),
      analyze_seq(Env, Op, Form);
    'clojerl.Vector' ->
      analyze_vector(Env, Form);
    'clojerl.Map' ->
      analyze_map(Env, Form);
    'clojerl.Set' ->
      analyze_set(Env, Form);
    _ ->
      analyze_const(Env, Form)
  end.

-spec analyze_const(clj_env:env(), any()) -> clj_env:env().
analyze_const(Env, Constant) ->
  Expr = #{op => constant,
           env => ?DEBUG(Env),
           tag => clj_core:type(Constant),
           form => Constant},
  clj_env:push_expr(Env, Expr).

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

-spec parse_ns(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_ns(Env, List) ->
  Second = clj_core:second(List),
  case 'symbol?'(Second) of
    true ->
      {_, NewEnv} = clj_env:find_or_create_ns(Env, Second),
      NewEnv;
    false ->
      throw(<<"First argument to ns must a symbol">>)
  end.

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
  Expr = #{op => quote,
           env => ?DEBUG(Env),
           expr => ConstExpr,
           form => List},
  clj_env:push_expr(NewEnv, Expr).

-spec parse_def(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
parse_def(Env, List) ->
  Docstring = validate_def_args(List),
  VarSymbol = second(List),
  case lookup_var(VarSymbol, Env) of
    {undefined, _} ->
      throw(<<"Can't refer to qualified var that doesn't exist">>);
    {Var, Env1} ->
      VarNsSym = 'clojerl.Var':namespace(Var),
      case {clj_env:current_ns(Env1), namespace(VarSymbol)} of
        {VarNsSym, _} -> ok;
        {_ , undefined} -> ok;
        _ -> throw(<<"Can't create defs outside of current ns">>)
      end,

      Meta = meta(VarSymbol),
      DynamicKeyword = keyword(<<"dynamic">>),
      IsDynamic = boolean(get(Meta, DynamicKeyword)),
      Var1 = 'clojerl.Var':dynamic(Var, IsDynamic),

      %% TODO: show warning when not dynamic but name suggests otherwise.
      %% TODO: Read metadata from symbol and add it to Var.

      Env2 = clj_env:update_var(Env1, Var1),
      Init = case Docstring of
               undefined -> third(List);
               _ -> fourth(List)
             end,

      ExprEnv2 = clj_env:context(Env2, expr),
      {InitExpr, Env3} = clj_env:pop_expr(analyze_form(ExprEnv2, Init)),
      VarExpr = #{op => def,
                  env => ?DEBUG(Env3),
                  form => List,
                  name => VarSymbol,
                  var => Var1,
                  doc => Docstring,
                  init => InitExpr,
                  dynamic => IsDynamic},

      clj_env:push_expr(Env3, VarExpr)
  end.

-spec validate_def_args('clojerl.List':type()) -> undefined | binary().
validate_def_args(List) ->
  Docstring =
    case {count(List), third(List)} of
      {4, Str} when is_binary(Str) -> Str;
      _ -> undefined
    end,
  case count(List) of
    C when C == 2;
           C == 3, Docstring == undefined;
           C == 4, Docstring =/= undefined  ->
      case type(second(List)) of
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
  NsSym = case namespace(VarSymbol) of
            undefined -> undefined;
            NsStr -> symbol(NsStr)
          end,
  case clj_env:current_ns(Env) of
    CurrentNs when CurrentNs == NsSym; NsSym == undefined ->
      NameSym = symbol(name(VarSymbol)),
      Fun = fun(Ns) -> clj_namespace:intern(Ns, NameSym) end,
      NewEnv = clj_env:update_ns(Env, CurrentNs, Fun),
      lookup_var(VarSymbol, false, NewEnv);
    _ ->
      lookup_var(VarSymbol, false, Env)
  end;
lookup_var(VarSymbol, false, Env) ->
  NsStr = namespace(VarSymbol),
  NameStr = name(VarSymbol),

  case {NsStr, NameStr} of
    {undefined, Name} when Name == <<"ns">>;
                           Name == <<"in-ns">> ->
      ClojureCoreSym = symbol(<<"clojure.core">>, Name),
      Var = clj_env:find_var(Env, ClojureCoreSym),
      {Var, Env};
    {undefined, _} ->
      CurrentNsSym = clj_env:current_ns(Env),
      Symbol = symbol(name(CurrentNsSym), NameStr),
      Var = clj_env:find_var(Env, Symbol),
      {Var, Env};
    {NsStr, NameStr} ->
      Symbol = symbol(NsStr, NameStr),
      Var = clj_env:find_var(Env, Symbol),
      {Var, Env}
  end.

-spec analyze_invoke(clj_env:env(), 'clojerl.List':type()) -> clj_env:env().
analyze_invoke(Env, Form) ->
  Env1 = analyze_form(Env, first(Form)),

  Args = rest(Form),
  FoldFun = fun(F, E) -> analyze_form(E, F) end,
  Env2 = lists:foldl(FoldFun, Env1, 'clojerl.List':to_list(Args)),

  ArgCount = clj_core:count(Args),
  {ArgsExpr, Env3} = clj_env:last_exprs(Env2, ArgCount),
  {FExpr, Env4} = clj_env:pop_expr(Env3),

  InvokeExpr = #{op => invoke,
                 env => ?DEBUG(Env4),
                 form => Form,
                 f => FExpr,
                 args => ArgsExpr},
  clj_env:push_expr(Env4, InvokeExpr).

-spec analyze_symbol(clj_env:env(), 'clojerl.Symbol':type()) -> clj_env:env().
analyze_symbol(Env, Symbol) ->
  Expr = #{op => var,
           env => ?DEBUG(Env),
           form => Symbol},
  case {namespace(Symbol), clj_env:get_local(Env, Symbol)} of
    {undefined, Local} when Local =/= undefined ->
      clj_env:push_expr(Env, Expr#{info => Local});
    _ ->
      case resolve(Env, Symbol) of
        undefined ->
          Str = clj_core:str([Symbol]),
          throw(<<"Unable to resolve var: ", Str/binary, " in this context">>);
        Var ->
          clj_env:push_expr(Env, Expr#{info => Var})
      end
  end.

-spec resolve(clj_env:env(), 'clojerl.Symbol':env()) -> any() | undefined.
resolve(Env, Symbol) ->
  CurrentNs = clj_env:find_ns(Env, clj_env:current_ns(Env)),
  Local = clj_env:get_local(Env, Symbol),
  NsStr = namespace(Symbol),
  UsedVar = clj_namespace:use(CurrentNs, Symbol),
  CurNsVar = clj_namespace:def(CurrentNs, Symbol),
  case {Local, NsStr, UsedVar, CurNsVar} of
    {Local, _, _, _} when Local =/= undefined ->
      Local;
    {_, NsStr, _, _} when NsStr =/= undefined ->
      clj_env:find_var(Env, Symbol);
    {_, _, UsedVar, _} when UsedVar =/= undefined ->
      UsedVar;
    {_, _, _, CurNsVar} when CurNsVar =/= undefined ->
      CurNsVar;
    _ ->
      undefined
  end.

-spec analyze_vector(clj_env:env(), 'clojerl.Vector':type()) -> clj_env:env().
analyze_vector(Env, Vector) ->
  Items = 'clojerl.Vector':to_list(Vector),
  FoldFun = fun(F, E) -> analyze_form(E, F) end,

  Count = clj_core:count(Vector),
  ExprEnv = clj_env:context(Env, expr),

  Env1 = lists:foldl(FoldFun, ExprEnv, Items),
  {ItemsExpr, Env2} = clj_env:last_exprs(Env1, Count),

  VectorExpr = #{op => vector,
                 env => ?DEBUG(Env2),
                 form => Vector,
                 items => ItemsExpr},
  clj_env:push_expr(Env2, VectorExpr).

-spec analyze_map(clj_env:env(), 'clojerl.Map':type()) -> clj_env:env().
analyze_map(Env, Map) ->
  FoldFun = fun(F, E) -> analyze_form(E, F) end,

  Keys = 'clojerl.Map':keys(Map),
  Vals = 'clojerl.Map':vals(Map),

  Count = clj_core:count(Map),
  ExprEnv = clj_env:context(Env, expr),

  Env1 = lists:foldl(FoldFun, ExprEnv, Keys),
  {KeysExpr, Env2} = clj_env:last_exprs(Env1, Count),
  Env3 = lists:foldl(FoldFun, Env2, Vals),
  {ValsExpr, Env4} = clj_env:last_exprs(Env3, Count),

  MapExpr = #{op => map,
              env => ?DEBUG(Env4),
              form => Map,
              keys => KeysExpr,
              vals => ValsExpr},
  clj_env:push_expr(Env4, MapExpr).

-spec analyze_set(clj_env:env(), 'clojerl.Set':type()) -> clj_env:env().
analyze_set(Env, Set) ->
  Items = 'clojerl.Set':to_list(Set),
  FoldFun = fun(F, E) -> analyze_form(E, F) end,
  ExprEnv = clj_env:context(Env, expr),
  Env1 = lists:foldl(FoldFun, ExprEnv, Items),

  Count = clj_core:count(Set),
  {ItemsExpr, Env2} = clj_env:last_exprs(Env1, Count),

  VectorExpr = #{op => set,
                 env => ?DEBUG(Env2),
                 form => Set,
                 items => ItemsExpr},
  clj_env:push_expr(Env2, VectorExpr).
