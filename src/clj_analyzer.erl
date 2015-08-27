-module(clj_analyzer).

-export([
         analyze/1,
         macroexpand/2,
         macroexpand_1/2,
         is_special/1
        ]).

-spec is_special(any()) -> boolean().
is_special(S) ->
  lists:member(S, special_forms()).

-spec analyze(any()) -> clj_env:env().
analyze(Forms) ->
  lists:foldl(fun analyze/2, clj_env:default(), Forms).

-spec macroexpand_1('clojerl.List':type(), clj_env:env()) -> any().
macroexpand_1(Form, Env) ->
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
      Fun = clj_core:deref(MacroVar),
      Args = [Form, Env, clj_core:rest(Form)],
      erlang:apply(Fun, Args)
  end.

-spec macroexpand('clojerl.List':type(), clj_env:env()) -> any().
macroexpand(Form, Env) ->
  case macroexpand_1(Form, Env) of
    Form -> {Form, Env};
    ExpandedForm -> macroexpand_1(ExpandedForm, Env)
  end.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec analyze(any(), clj_env:env()) -> clj_env:env().
analyze(nil, Env) ->
  Expr = erl_syntax:abstract(undefined),
  clj_env:add_expr(Env, Expr);
analyze(Boolean, Env) when is_boolean(Boolean) ->
  Expr = erl_syntax:abstract(Boolean),
  clj_env:add_expr(Env, Expr);
analyze(String, Env) when is_binary(String) ->
  Expr = erl_syntax:abstract(String),
  clj_env:add_expr(Env, Expr);
analyze(Number, Env) when is_number(Number) ->
  Expr = erl_syntax:abstract(Number),
  clj_env:add_expr(Env, Expr);
analyze(Form, Env) ->
  case clj_core:type(Form) of
    'clojerl.Symbol' ->
      analyze_symbol(Form, Env);
    'clojerl.Keyword' ->
      Expr = erl_syntax:abstract(Form),
      clj_env:add_expr(Env, Expr);
    'clojerl.List' ->
      Op = clj_core:first(Form),
      analyze_seq(Op, Form, Env);
    'clojerl.Vector' ->
      Expr = erl_syntax:abstract(Form),
      clj_env:add_expr(Env, Expr);
    'clojerl.Map' ->
      Expr = erl_syntax:abstract(Form),
      clj_env:add_expr(Env, Expr);
    'clojerl.Set' ->
      Expr = erl_syntax:abstract(Form),
      clj_env:add_expr(Env, Expr);
    _ ->
      throw({invalid_form, Form, Env})
  end.

-spec analyze_seq(any(), 'clojerl.List':type(), clj_env:env()) -> clj_env:env().
analyze_seq(undefined, _List, _Env) ->
  throw(<<"Can't call nil">>);
analyze_seq(Op, List, Env) ->
  case macroexpand_1(List, Env) of
    List ->
      case lists:member(Op, special_forms()) of
        true ->
          Name = clj_core:name(Op),
          parse_special_form(Name, List, Env);
        false ->
          analyze_invoke(List, Env)
      end;
    ExpandedList ->
      analyze(ExpandedList, Env)
  end.

-spec parse_special_form(any(), 'clojerl.List':type(), clj_env:env()) -> clj_env:env().
parse_special_form(<<"def">>, List, Env) ->
  Docstring = validate_def_args(List),
  VarSymbol = clj_core:second(List),
  case lookup_var(VarSymbol, Env) of
    {undefined, _} ->
      throw(<<"Can't refer to qualified var that doesn't exist">>);
    {Var, Env1} ->
      VarNsSym = 'clojerl.Var':namespace(Var),
      case {clj_env:current_ns(Env1), clj_core:namespace(VarSymbol)} of
        {VarNsSym, _} -> ok;
        {_ , undefined} ->  throw(<<"Can't create defs outside of current ns">>);
        _ -> ok
      end,

      Meta = clj_core:meta(VarSymbol),
      DynamicKeyword = clj_core:keyword(<<"dynamic">>),
      IsDynamic = clj_core:boolean(clj_core:get(Meta, DynamicKeyword)),
      Var1 = 'clojerl.Var':dynamic(Var, IsDynamic),

      %% TODO: show warning when not dynamic but name suggests otherwise.
      %% TODO: Read metadata from symbol and add it to Var.

      Env2 = clj_env:update_var(Var1, Env1),
      Init = case Docstring of
               undefined -> clj_core:third(List);
               _ -> clj_core:fourth(List)
             end,
      {InitExpr, Env3} = clj_env:pop_expr(analyze(Init, Env2)),
      Expr = #{type => def,
               var => Var1,
               init => InitExpr},
      clj_env:add_expr(Env3, Expr)
  end.

-spec validate_def_args('clojerl.List':type()) -> undefined | binary().
validate_def_args(List) ->
  Docstring =
    case {clj_core:count(List), clj_core:third(List)} of
      {4, Str} when is_binary(Str) -> Str;
      _ -> undefined
    end,
  case clj_core:type(clj_core:second(List)) of
    'clojerl.Symbol' -> ok;
    _ -> throw(<<"First argument to def must be a Symbol">>)
  end,
  case clj_core:count(List) of
    C when C == 2;
           C == 3, Docstring == undefined;
           C == 4, Docstring =/= undefined  ->
      Docstring;
    1 ->
      throw(<<"Too few arguments to def">>);
    _ ->
      throw(<<"Too many arguments to def">>)
  end.

-spec lookup_var('clojerl.Symbol':type(), clj_env:env()) -> ok.
lookup_var(VarSymbol, Env) ->
  lookup_var(VarSymbol, true, Env).

-spec lookup_var('clojerl.Symbol':type(), CreateNew :: boolean(), clj_env:env()) ->
  {'clojerl.Var':type(), clj_env:env()}.
lookup_var(VarSymbol, true, Env) ->
  NsSym = case clj_core:namespace(VarSymbol) of
            undefined -> undefined;
            NsStr -> clj_core:symbol(NsStr)
          end,

  case clj_env:current_ns(Env) of
    CurrentNs when CurrentNs == NsSym; NsSym == undefined ->
      NameSym = clj_core:symbol(clj_core:name(VarSymbol)),
      Fun = fun(Ns) -> clj_namespace:intern(Ns, NameSym) end,
      NewEnv = clj_env:update_ns(CurrentNs, Fun, Env),
      lookup_var(VarSymbol, false, NewEnv);
    _ ->
      {undefined, Env}
  end;
lookup_var(VarSymbol, false, Env) ->
  NsStr = clj_core:namespace(VarSymbol),
  NameStr = clj_core:name(VarSymbol),

  case {NsStr, NameStr} of
    {undefined, Name} when Name == <<"ns">>;
                           Name == <<"in-ns">> ->
      ClojureCoreSym = clj_core:symbol(<<"clojure.core">>),
      Ns = clj_env:get_ns(ClojureCoreSym, Env),
      Var = clj_namespace:lookup(Ns, VarSymbol),
      {Var, Env};
    {undefined, _} ->
      Ns = clj_env:get_ns(clj_env:current_ns(Env), Env),
      Var = clj_namespace:lookup(Ns, VarSymbol),
      {Var, Env};
    {NsStr, NameStr} ->
      Ns = clj_env:get_ns(clj_core:symbol(NsStr), Env),
      Var = clj_namespace:lookup(Ns, clj_core:symbol(NameStr)),
      {Var, Env}
  end.

special_forms() ->
  SymbolFun = fun clj_core:symbol/1,
  [SymbolFun(<<"def">>),
   SymbolFun(<<"loop*">>),
   SymbolFun(<<"recur">>),
   SymbolFun(<<"if">>),
   SymbolFun(<<"case*">>),
   SymbolFun(<<"let*">>),
   SymbolFun(<<"letfn*">>),
   SymbolFun(<<"do">>),
   SymbolFun(<<"fn*">>),
   SymbolFun(<<"quote">>),
   SymbolFun(<<"var">>),
   SymbolFun(<<"import*">>),
   SymbolFun(<<"deftype*">>),
   SymbolFun(<<"reify*">>),
   SymbolFun(<<"try">>),
   %% SymbolFun('monitor-enter') => fun analyze_def/2,
   %% SymbolFun('monitor-exit') => fun analyze_def/2,
   %% SymbolFun('new') => fun analyze_def/2,
   %% SymbolFun('&') => fun analyze_def/2
   SymbolFun(<<"throw">>),
   SymbolFun(<<"catch">>),
   SymbolFun(<<"finally">>)
  ].

-spec analyze_invoke('clojerl.List':type(), clj_env:env()) -> clj_env:env().
analyze_invoke(_List, Env) ->
  Env.

-spec analyze_symbol('clojerl.Symbol':type(), clj_env:env()) -> clj_env:env().
analyze_symbol(_, Env) ->
  Env.
