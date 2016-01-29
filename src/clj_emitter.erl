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
%%------------------------------------------------------------------------------
%% var, binding & local
%%------------------------------------------------------------------------------
ast(#{op := var, var := Var} = _Expr) ->
  Module = 'clojerl.Var':module(Var),
  Name   = 'clojerl.Var':val_function(Var),

  [application_mfa(Module, Name, [])];
ast(#{op := binding} = Expr) ->
  NameSym = maps:get(name, Expr),
  NameAtom = 'clojerl.Symbol':to_atom(NameSym),

  [erl_syntax:variable(NameAtom)];
ast(#{op := local} = Expr) ->
  NameSym = maps:get(name, Expr),
  NameAtom = 'clojerl.Symbol':to_atom(NameSym),

  [erl_syntax:variable(NameAtom)];
%%------------------------------------------------------------------------------
%% do
%%------------------------------------------------------------------------------
ast(#{op := do} = Expr) ->
  #{ statements := StatementsExprs
   , ret        := ReturnExpr
   } = Expr,

  Stms = lists:flatmap(fun ast/1, StatementsExprs),
  Ret = ast(ReturnExpr),

  Stms ++ Ret;
%%------------------------------------------------------------------------------
%% def
%%------------------------------------------------------------------------------
ast(#{op := def, var := Var, init := InitExpr} = _Expr) ->
  Module  = 'clojerl.Var':module(Var),
  Name    = 'clojerl.Var':function(Var),
  ValName = 'clojerl.Var':val_function(Var),

  {ok, ModuleDef} = case code:ensure_loaded(Module) of
                      {module, Module} ->
                        clj_module:from_binary(Module);
                      {error, _} ->
                        {ok, clj_module:new([attribute_module(Module)])}
                    end,

  %% Add the var's information as a module attribute
  VarAtom = erl_syntax:atom(var),
  VarAst = erl_syntax:abstract(Var),
  VarAttrAst = erl_syntax:attribute(VarAtom, [VarAst]),

  {{FunctionsAst, ExportsAst}, ValClause} =
    case InitExpr of
      #{op := fn} ->
        { ast_for_fn(Name, InitExpr)
        , erl_syntax:clause([], [erl_syntax:abstract(Var)])
        };
      _ ->
        InitAst = ast(InitExpr),
        {{[], []}, erl_syntax:clause([], InitAst)}
    end,

  ValFunAst = function_form(ValName, [ValClause]),
  ValFunExportAst = export_attribute([{ValName, 0}]),

  AttrsAsts = [ValFunExportAst | ExportsAst] ++ [VarAttrAst],
  ModuleDef1 = clj_module:add_attributes(ModuleDef, AttrsAsts),
  ModuleDef2 = clj_module:add_functions(ModuleDef1, [ValFunAst | FunctionsAst]),

  clj_module:to_forms(ModuleDef2);
%%------------------------------------------------------------------------------
%% fn, invoke, erl_fun
%%------------------------------------------------------------------------------
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
ast(#{op := erl_fun, invoke := true} = Expr) ->
  #{ module   := Module
   , function := Function
   } = Expr,

  ModuleTree = erl_syntax:atom(Module),
  FunctionTree = erl_syntax:atom(Function),

  [erl_syntax:module_qualifier(ModuleTree, FunctionTree)];
ast(#{op := erl_fun} = Expr) ->
  #{ module   := Module
   , function := Function
   , arity    := Arity
   } = Expr,

  clj_utils:throw_when( Arity == undefined
                      , [ <<"Can't use erlang function as value without ">>
                        , <<"specifying its arity">>
                        ]
                      ),

  ModuleTree   = erl_syntax:atom(Module),
  FunctionTree = erl_syntax:atom(Function),
  ArityTree    = erl_syntax:abstract(Arity),

  ArityQualifier  = erl_syntax:arity_qualifier(FunctionTree, ArityTree),
  ModuleQualifier = erl_syntax:module_qualifier(ModuleTree, ArityQualifier),

  [erl_syntax:implicit_fun(ModuleQualifier)];
ast(#{op := invoke} = Expr) ->
  #{ args := ArgsExpr
   , f    := FExpr
   } = Expr,

  Args = lists:flatmap(fun ast/1, ArgsExpr),

  case FExpr of
    #{op := var, var := Var} ->
      Module   = 'clojerl.Var':module(Var),
      Function = 'clojerl.Var':function(Var),
      Args1 = var_process_args(Var, Args),
      [application_mfa(Module, Function, Args1)];
    #{op := erl_fun} ->
      [FunAst] = ast(FExpr#{invoke => true}),
      [erl_syntax:application(FunAst, Args)];
    #{op := fn} ->
      [FunAst] = ast(FExpr),
      [erl_syntax:application(FunAst, Args)];
    _ ->
      [FunAst] = ast(FExpr),
      [application_mfa(clj_core, invoke, [FunAst, erl_syntax:list(Args)])]
  end;
%%------------------------------------------------------------------------------
%% Literal data structures
%%------------------------------------------------------------------------------
ast(#{op := vector} = Expr) ->
  #{items := ItemsExprs} = Expr,

  Items = lists:flatmap(fun ast/1, ItemsExprs),
  ListItems = erl_syntax:list(Items),

  [application_mfa('clojerl.Vector', new, [ListItems])];
ast(#{op := map} = Expr) ->
  #{ keys := KeysExprs
   , vals := ValsExprs
   } = Expr,

  Keys = lists:flatmap(fun ast/1, KeysExprs),
  Vals = lists:flatmap(fun ast/1, ValsExprs),
  PairUp = fun
             PairUp([], [], Pairs) ->
               Pairs;
             PairUp([H1 | Tail1], [H2 | Tail2], Pairs) ->
               PairUp(Tail1, Tail2, [H1, H2 | Pairs])
           end,

  Items = PairUp(Keys, Vals, []),
  ListItems = erl_syntax:list(Items),

  [application_mfa('clojerl.Map', new, [ListItems])];
ast(#{op := set} = Expr) ->
  #{items := ItemsExprs} = Expr,

  Items = lists:flatmap(fun ast/1, ItemsExprs),
  ListItems = erl_syntax:list(Items),

  [application_mfa('clojerl.Set', new, [ListItems])];
%%------------------------------------------------------------------------------
%% if
%%------------------------------------------------------------------------------
ast(#{op := 'if'} = Expr) ->
  #{ test := TestExpr
   , then := ThenExpr
   , else := ElseExpr
   } = Expr,

  [Test] = ast(TestExpr),

  True          = erl_syntax:variable('True'),
  FalseAtom     = erl_syntax:atom(false),
  UndefinedAtom = erl_syntax:atom(undefined),
  TrueGuards    = [ application_mfa(erlang, '=/=', [True, FalseAtom])
                  , application_mfa(erlang, '=/=', [True, UndefinedAtom])
                  ],
  TrueClause    = erl_syntax:clause([True], TrueGuards, ast(ThenExpr)),

  Whatever = erl_syntax:variable('_'),
  FalseClause = erl_syntax:clause([Whatever], [], ast(ElseExpr)),

  [erl_syntax:case_expr(Test, [TrueClause, FalseClause])];
%%------------------------------------------------------------------------------
%% throw
%%------------------------------------------------------------------------------
ast(#{op := throw} = Expr) ->
  #{exception := ExceptionExpr} = Expr,

  [Exception] = ast(ExceptionExpr),

  [application_mfa(erlang, throw, [Exception])].

%%------------------------------------------------------------------------------
%% AST Helper Functions
%%------------------------------------------------------------------------------

-spec var_process_args(map(), [any()]) -> [any()].
var_process_args(Var, Args) ->
  Meta = clj_core:meta(Var),
  #{ 'variadic?'     := IsVariadic
   , max_fixed_arity := MaxFixedArity
   , variadic_arity  := VariadicArity
   } = Meta,

  ArgCount = length(Args),
  case IsVariadic of
    true when ArgCount =< MaxFixedArity, MaxFixedArity =/= undefined ->
      Args;
    true when ArgCount >= VariadicArity; MaxFixedArity == undefined ->
      {Args1, Rest} = lists:split(VariadicArity, Args),
      Args1 ++ [erl_syntax:list(Rest)];
    _ -> Args
  end.

-spec attribute_module(atom()) -> erl_syntax:syntaxTree().
attribute_module(Name) when is_atom(Name) ->
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

-spec ast_for_fn(atom(), map()) -> {[erl_syntax:syntaxTree()], [erl_syntax:syntaxTree()]}.
ast_for_fn(Name, #{op := fn, methods := Methods}) ->
  ParamCountFun = fun(#{params := Params}) -> length(Params) end,
  GroupedMethods = clj_utils:group_by(ParamCountFun, Methods),

  FunctionFun = fun(MethodsList) ->
                    ClausesAst = lists:map(fun method_to_clause/1, MethodsList),
                    function_form(Name, ClausesAst)
                end,
  ExportFun = fun(Arity) -> export_attribute([{Name, Arity}]) end,

  { lists:map(FunctionFun, maps:values(GroupedMethods))
  , lists:map(ExportFun, maps:keys(GroupedMethods))
  }.
