-module(clj_emitter).

-export([emit/1]).

-spec emit(clj_env:env()) -> clj_env:env().
emit(Env0) ->
  case clj_env:pop_expr(Env0) of
    {undefined, _} ->
      Env0;
    {Expr, Env} ->
      io:format("~p~n~s~n", [clj_core:str(Expr), lists:duplicate(80, $=)]),
      Forms = lists:map(fun erl_syntax:revert/1, ast(Expr)),
      compile_forms(Forms),
      Env
  end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

compile_forms([]) ->
  ok;
compile_forms(Forms) ->
  {ok, Name, Binary} = compile:forms(Forms),
  code:load_binary(Name, "", Binary).

-spec ast(map()) -> [erl_syntax:syntaxTree()].
ast(#{op := constant, form := Form} = Expr) ->
  %% A constant as a top level form doesn't produce code.
  case is_top_level(Expr) of
    true -> [];
    false -> [erl_syntax:abstract(Form)]
  end;
ast(#{op := quote, expr := Expr}) ->
  ast(Expr);
ast(#{op := var} = _Expr) ->
  io:format("{{{ Resolve var's value and emit it. }}}~n"),
  [];
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
  NamespaceStr = clj_core:str('clojerl.Var':namespace(Var)),
  NameStr = clj_core:str('clojerl.Var':name(Var)),
  %% Erlang module's name will be clj.{{namespace}}.{{name}}__var
  ModuleStr = <<"clj.", NamespaceStr/binary, ".", NameStr/binary, "__var">>,
  ModuleAst = module_attribute(binary_to_atom(ModuleStr, utf8)),

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
ast(#{op := invoke} = Expr) ->
  #{ args := ArgsExpr
   , f    := FExpr
   } = Expr,
  Args = lists:flatmap(fun ast/1, ArgsExpr),
  [Fun] = ast(FExpr),
  [erl_syntax:application(Fun, Args)].

%%------------------------------------------------------------------------------
%% AST Helper Functions
%%------------------------------------------------------------------------------

-spec is_top_level(clj_analyzer:expr()) -> boolean().
is_top_level(Expr) ->
  maps:get(top_level, Expr, false).

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
