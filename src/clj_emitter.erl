-module(clj_emitter).

-export([emit/1]).

-spec emit(clj_env:env()) ->
  {erl_parse:abstract_form(), erl_parse:abstract_expr(), clj_env:env()}.
emit(Env0) ->
  case clj_env:pop_expr(Env0) of
    {undefined, _} ->
      {[], [], Env0};
    {Expr, Env} ->
      %% io:format("~p~n~s~n", [clj_core:str(Expr), lists:duplicate(80, $=)]),
      SyntaxTrees   = ast(Expr),
      RevertedForms = erl_syntax:revert_forms(SyntaxTrees),
      {Forms, Expressions} =
        lists:partition(fun erl_syntax:is_form/1, RevertedForms),
      {Forms, Expressions, Env}
  end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec ast(map()) -> [erl_syntax:syntaxTree()].
ast(#{op := constant, form := Form}) ->
  [erl_syntax:abstract(Form)];
ast(#{op := quote, expr := Expr}) ->
  ast(Expr);
ast(#{op := var, var := Var} = _Expr) ->
  Module = var_module(Var),

  [application_mfa(Module, val, [])];
ast(#{op := binding} = Expr) ->
  NameSym = maps:get(name, Expr),
  NameAtom = 'clojerl.Symbol':to_atom(NameSym),

  [erl_syntax:variable(NameAtom)];
ast(#{op := local} = Expr) ->
  NameSym = maps:get(name, Expr),
  NameAtom = 'clojerl.Symbol':to_atom(NameSym),

  [erl_syntax:variable(NameAtom)];
ast(#{op := do} = Expr) ->
  #{ statements := StatementsExprs
   , ret        := ReturnExpr
   } = Expr,

  Stms = lists:flatmap(fun ast/1, StatementsExprs),
  Ret = ast(ReturnExpr),

  Stms ++ Ret;
ast(#{op := def, var := Var, init := InitExpr} = _Expr) ->
  %% Create a module that provides a single function with the var's
  %% value and add module attributes with the var's info.
  ModuleAst = module_attribute(var_module(Var)),

  ExportAst = export_attribute([{val, 0}]),

  %% Add the var's information as a module attribute
  VarAtom = erl_syntax:atom(var),
  VarAst = erl_syntax:abstract(Var),
  VarAttributeAst = erl_syntax:attribute(VarAtom, [VarAst]),

  InitAst = ast(InitExpr),
  ClauseAst =  erl_syntax:clause([], InitAst),
  FunctionAst = function_form(val, [ClauseAst]),

  [ModuleAst, ExportAst, VarAttributeAst, FunctionAst];
ast(#{op := fn} = Expr) ->
  #{methods := Methods} = Expr,

  Clauses = lists:map(fun method_to_clause/1, Methods),

  case maps:get(local, Expr, undefined) of
    undefined ->
      [erl_syntax:fun_expr(Clauses)];
    NameExpr ->
      #{name := NameSym} = NameExpr,

      NameAtom = 'clojerl.Symbol':to_atom(NameSym),
      Name = erl_syntax:variable(NameAtom),

      [erl_syntax:named_fun_expr(Name, Clauses)]
  end;
ast(#{op := erl_fun} = Expr) ->
  #{ module := Module
   , function := Function
   } = Expr,

  ModuleTree = erl_syntax:atom(Module),
  FunctionTree = erl_syntax:atom(Function),

  [erl_syntax:module_qualifier(ModuleTree, FunctionTree)];
ast(#{op := invoke} = Expr) ->
  #{ args := ArgsExpr
   , f    := FExpr
   } = Expr,

  Args = lists:flatmap(fun ast/1, ArgsExpr),
  [Fun] = ast(FExpr),

  [erl_syntax:application(Fun, Args)];
ast(#{op := vector} = Expr) ->
  #{items := ItemsExprs} = Expr,

  Items = lists:flatmap(fun ast/1, ItemsExprs),
  ListItems = erl_syntax:list(Items),

  [application_mfa('clojerl.Vector', new, [ListItems])].

%%------------------------------------------------------------------------------
%% AST Helper Functions
%%------------------------------------------------------------------------------

%% @doc Erlang module's name will be clj.{{namespace}}.{{name}}__var
-spec var_module('clojerl.Var':type()) -> module().
var_module(Var) ->
  NamespaceStr = clj_core:str('clojerl.Var':namespace(Var)),
  NameStr = clj_core:str('clojerl.Var':name(Var)),
  ModuleStr = <<"clj.", NamespaceStr/binary, ".", NameStr/binary, "__var">>,
  binary_to_atom(ModuleStr, utf8).

-spec module_attribute(atom()) -> erl_syntax:syntaxTree().
module_attribute(Name) when is_atom(Name) ->
  ModuleAtom = erl_syntax:atom(module),
  NameAtom = erl_syntax:atom(Name),
  erl_syntax:attribute(ModuleAtom, [NameAtom]).

-spec function_form(atom(), [erl_syntax:syntaxTree()]) ->
  erl_syntax:syntaxTree().
function_form(Name, Clauses) when is_atom(Name) ->
  NameAst = erl_syntax:atom(Name),
  erl_syntax:function(NameAst, Clauses).

-spec export_attribute({atom(), integer()}) ->
  erl_syntax:syntaxTree().
export_attribute(FAs) when is_list(FAs) ->
  ExportAtom = erl_syntax:atom(export),
  Fun  = fun({Function, Arity}) -> arity_qualifier(Function, Arity) end,
  Asts = lists:map(Fun, FAs),
  ListAst = erl_syntax:list(Asts),
  erl_syntax:attribute(ExportAtom, [ListAst]).

-spec arity_qualifier(atom(), integer()) -> erl_syntax:syntaxTree().
arity_qualifier(Function, Arity) when is_atom(Function),
                                      is_integer(Arity) ->
  FunctionAtomAst = erl_syntax:atom(Function),
  ArityIntegerAst = erl_syntax:integer(Arity),
  erl_syntax:arity_qualifier(FunctionAtomAst, ArityIntegerAst).

-spec method_to_clause(clj_analyzer:expr()) -> erl_syntax:syntaxTree().
method_to_clause(MethodExpr) ->
  #{ params := ParamsExprs
   , body   := BodyExpr
   } = MethodExpr,

  Args = lists:flatmap(fun ast/1, ParamsExprs),
  Guards = [],
  Body = ast(BodyExpr),

  erl_syntax:clause(Args, Guards, Body).

-spec application_mfa(module(), atom(), list()) -> erl_syntax:syntaxTree().
application_mfa(Module, Function, Args) ->
  ModuleTree = erl_syntax:atom(Module),
  FunctionTree = erl_syntax:atom(Function),

  ValQualifier = erl_syntax:module_qualifier(ModuleTree, FunctionTree),

  erl_syntax:application(ValQualifier, Args).
