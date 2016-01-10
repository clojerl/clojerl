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
ast(#{op := def, var := Var, init := InitExpr} = _Form) ->
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
ast(#{op := fn} = Form) ->
  io:format("======= FN* ======~n~p~n=================~n", [clj_core:str(Form)]),
  Name = erl_syntax:variable('F'),
  Clause = erl_syntax:clause([], [Name]),

  [erl_syntax:named_fun_expr(Name, [Clause])];
ast(#{op := constant, form := Form} = Expr) ->
  %% A constant as a top level form doesn't produce code.
  case maps:get(top_level, Expr, false) of
    true -> [];
    false -> [erl_syntax:abstract(Form)]
  end;
ast(#{op := quote, expr := Expr} = _Form) ->
  ast(Expr);
ast(#{op := var} = _Form) ->
  io:format("{{{ Resolve var's value and emit it. }}}~n"),
  [].

%%------------------------------------------------------------------------------
%% AST Helper Functions
%%------------------------------------------------------------------------------

module_attribute(Name) when is_atom(Name) ->
  ModuleAtom = erl_syntax:atom(module),
  NameAtom = erl_syntax:atom(Name),
  erl_syntax:attribute(ModuleAtom, [NameAtom]).

function_form(Name, Clauses) when is_atom(Name) ->
  NameAst = erl_syntax:atom(Name),
  erl_syntax:function(NameAst, Clauses).

export_attribute(FAs) when is_list(FAs) ->
  ExportAtom = erl_syntax:atom(export),
  Fun  = fun({Function, Arity}) -> arity_qualifier(Function, Arity) end,
  Asts = lists:map(Fun, FAs),
  ListAst = erl_syntax:list(Asts),
  erl_syntax:attribute(ExportAtom, [ListAst]).

arity_qualifier(Function, Arity) when is_atom(Function),
                                      is_integer(Arity) ->
  FunctionAtomAst = erl_syntax:atom(Function),
  ArityIntegerAst = erl_syntax:integer(Arity),
  erl_syntax:arity_qualifier(FunctionAtomAst, ArityIntegerAst).
