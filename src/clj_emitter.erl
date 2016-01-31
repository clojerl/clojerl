-module(clj_emitter).

-export([emit/1]).

-type ast() :: erl_syntax:syntaxTree().

-type state() :: #{ modules => #{atom() => clj_module:module()}
                  , asts    => [ast()]
                  }.

-spec emit(clj_env:env()) ->
  { [[erl_parse:abstract_form()]]
  , [erl_parse:abstract_expr()]
  , clj_env:env()
  }.
emit(Env0) ->
  case clj_env:pop_expr(Env0) of
    {undefined, _} ->
      {[], [], Env0};
    {Expr, Env} ->
      InitState = #{modules => #{}, asts => []},
      #{ modules := Modules
       , asts    := ReversedExpressions
       } = ast(Expr, InitState),

      ModulesForms  = lists:map( fun clj_module:to_forms/1
                               , maps:values(Modules)
                               ),
      ModulesForms1 = lists:map( fun erl_syntax:revert_forms/1
                               , ModulesForms
                               ),

      Expressions   = lists:map( fun erl_syntax:revert/1
                               , lists:reverse(ReversedExpressions)
                               ),

      {ModulesForms1, Expressions, Env}
  end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec ast(map(), state()) -> {[ast()], state()}.
ast(#{op := constant, form := Form}, State) ->
  Ast = erl_syntax:abstract(Form),
  push_ast(Ast, State);
ast(#{op := quote, expr := Expr}, State) ->
  ast(Expr, State);
%%------------------------------------------------------------------------------
%% var, binding & local
%%------------------------------------------------------------------------------
ast(#{op := var, var := Var} = _Expr, State) ->
  Module = 'clojerl.Var':module(Var),
  Name   = 'clojerl.Var':val_function(Var),
  Ast    = application_mfa(Module, Name, []),

  push_ast(Ast, State);
ast(#{op := binding} = Expr, State) ->
  NameSym  = maps:get(name, Expr),
  NameAtom = 'clojerl.Symbol':to_atom(NameSym),
  Ast      = erl_syntax:variable(NameAtom),

  push_ast(Ast, State);
ast(#{op := local} = Expr, State) ->
  NameSym  = maps:get(name, Expr),
  NameAtom = 'clojerl.Symbol':to_atom(NameSym),
  Ast      = erl_syntax:variable(NameAtom),

  push_ast(Ast, State);
%%------------------------------------------------------------------------------
%% do
%%------------------------------------------------------------------------------
ast(#{op := do} = Expr, State) ->
  #{ statements := StatementsExprs
   , ret        := ReturnExpr
   } = Expr,

  StmsCount = length(StatementsExprs),
  {Stms, State1} = pop_ast( lists:foldl(fun ast/2, State, StatementsExprs)
                          , StmsCount
                          ),
  {Ret, State2} = pop_ast(ast(ReturnExpr, State1)),
  Ast = erl_syntax:block_expr(Stms ++ [Ret]),

  push_ast(Ast, State2);
%%------------------------------------------------------------------------------
%% def
%%------------------------------------------------------------------------------
ast(#{op := def, var := Var, init := InitExpr} = _Expr, State) ->
  Module  = 'clojerl.Var':module(Var),
  Name    = 'clojerl.Var':function(Var),
  ValName = 'clojerl.Var':val_function(Var),

  State1 = ensure_module(Module, State),
  VarAst = erl_syntax:abstract(Var),
  {ValAst, State2} =
    case InitExpr of
      #{op := fn} = FnExpr ->
        { VarAst
        , add_functions(Module, Name, FnExpr, State1)
        };
      _ ->
        pop_ast(ast(InitExpr, State1))
    end,

  %% Add the var's information as a module attribute
  VarAtom    = erl_syntax:atom(var),
  VarAttrAst = erl_syntax:attribute(VarAtom, [VarAst]),

  ValFunExportAst = export_attribute([{ValName, 0}]),

  ValClause = erl_syntax:clause([], [ValAst]),
  ValFunAst = function_form(ValName, [ValClause]),

  AttrsAsts = [ValFunExportAst, VarAttrAst],

  State3 = add_functions_attributes(Module, [ValFunAst], AttrsAsts, State2),

  push_ast(VarAst, State3);
%%------------------------------------------------------------------------------
%% fn, invoke, erl_fun
%%------------------------------------------------------------------------------
ast(#{op := fn} = Expr, State) ->
  LocalExpr = maps:get(local, Expr, undefined),
  ModuleSym = maps:get(namespace, LocalExpr),
  NameSym   = maps:get(name, LocalExpr),

  RemoveKeys = [op, env, methods, form, once],
  Meta       = maps:without(RemoveKeys, Expr),
  VarNew     = 'clojerl.Var':new(ModuleSym, NameSym),
  Var        = clj_core:with_meta(VarNew, Meta),
  VarAst     = erl_syntax:abstract(Var),

  Name   = 'clojerl.Symbol':to_atom(NameSym),
  Module = 'clojerl.Symbol':to_atom(ModuleSym),

  State1 = ensure_module(Module, State),
  State2 = add_functions(Module, Name, Expr, State1),

  push_ast(VarAst, State2);
ast(#{op := erl_fun, invoke := true} = Expr, State) ->
  #{ module   := Module
   , function := Function
   } = Expr,

  ModuleTree = erl_syntax:atom(Module),
  FunctionTree = erl_syntax:atom(Function),
  Ast = erl_syntax:module_qualifier(ModuleTree, FunctionTree),

  push_ast(Ast, State);
ast(#{op := erl_fun} = Expr, State) ->
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
  Ast = erl_syntax:implicit_fun(ModuleQualifier),

  push_ast(Ast, State);
ast(#{op := invoke} = Expr, State) ->
  #{ args := ArgsExpr
   , f    := FExpr
   } = Expr,

  {Args, State1} = pop_ast( lists:foldl(fun ast/2, State, ArgsExpr)
                          , length(ArgsExpr)
                          ),

  case FExpr of
    #{op := var, var := Var} ->
      Module   = 'clojerl.Var':module(Var),
      Function = 'clojerl.Var':function(Var),
      Args1    = 'clojerl.Var':process_args(Var, Args, fun erl_syntax:list/1),
      Ast      = application_mfa(Module, Function, Args1),

      push_ast(Ast, State1);
    #{op := erl_fun} ->
      {FunAst, State2} = pop_ast(ast(FExpr, State1)),
      Ast = erl_syntax:application(FunAst, Args),

      push_ast(Ast, State2);
    #{op := fn} ->
      {VarAst, State2} = pop_ast(ast(FExpr, State1)),
      Var      = erl_syntax:concrete(VarAst),
      Module   = 'clojerl.Var':module(Var),
      Function = 'clojerl.Var':function(Var),
      Args1    = 'clojerl.Var':process_args(Var, Args, fun erl_syntax:list/1),
      Ast      = application_mfa(Module, Function, Args1),

      push_ast(Ast, State2);
    _ ->
      {FunAst, State2} = pop_ast(ast(FExpr, State1)),
      ArgsAst = erl_syntax:list(Args),
      Ast     = application_mfa(clj_core, invoke, [FunAst, ArgsAst]),

      push_ast(Ast, State2)
  end;
%%------------------------------------------------------------------------------
%% Literal data structures
%%------------------------------------------------------------------------------
ast(#{op := vector} = Expr, State) ->
  #{items := ItemsExprs} = Expr,

  {Items, State1} = pop_ast( lists:foldl(fun ast/2, State, ItemsExprs)
                           , length(ItemsExprs)
                           ),
  ListItems = erl_syntax:list(Items),

  Ast = application_mfa('clojerl.Vector', new, [ListItems]),
  push_ast(Ast, State1);
ast(#{op := map} = Expr, State) ->
  #{ keys := KeysExprs
   , vals := ValsExprs
   } = Expr,

  {Keys, State1} = pop_ast( lists:foldl(fun ast/2, State, KeysExprs)
                          , length(KeysExprs)
                          ),
  {Vals, State2} = pop_ast( lists:foldl(fun ast/2, State1, ValsExprs)
                          , length(ValsExprs)
                          ),
  PairUp = fun
             PairUp([], [], Pairs) ->
               Pairs;
             PairUp([H1 | Tail1], [H2 | Tail2], Pairs) ->
               PairUp(Tail1, Tail2, [H1, H2 | Pairs])
           end,

  Items = PairUp(Keys, Vals, []),
  ListItems = erl_syntax:list(Items),

  Ast = application_mfa('clojerl.Map', new, [ListItems]),
  push_ast(Ast, State2);
ast(#{op := set} = Expr, State) ->
  #{items := ItemsExprs} = Expr,

  {Items, State1} = pop_ast( lists:foldl(fun ast/2, State, ItemsExprs)
                           , length(ItemsExprs)
                           ),
  ListItems = erl_syntax:list(Items),

  Ast = application_mfa('clojerl.Set', new, [ListItems]),
  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% if
%%------------------------------------------------------------------------------
ast(#{op := 'if'} = Expr, State) ->
  #{ test := TestExpr
   , then := ThenExpr
   , else := ElseExpr
   } = Expr,

  {Test, State1} = pop_ast(ast(TestExpr, State)),

  True          = erl_syntax:variable('True'),
  FalseAtom     = erl_syntax:atom(false),
  UndefinedAtom = erl_syntax:atom(undefined),
  TrueGuards    = [ application_mfa(erlang, '=/=', [True, FalseAtom])
                  , application_mfa(erlang, '=/=', [True, UndefinedAtom])
                  ],
  {ThenAst, State2} = pop_ast(ast(ThenExpr, State1)),
  TrueClause    = erl_syntax:clause([True], TrueGuards, [ThenAst]),

  Whatever = erl_syntax:variable('_'),
  {ElseAst, State3} = pop_ast(ast(ElseExpr, State2)),
  FalseClause = erl_syntax:clause([Whatever], [], [ElseAst]),

  Ast = erl_syntax:case_expr(Test, [TrueClause, FalseClause]),
  push_ast(Ast, State3);
%%------------------------------------------------------------------------------
%% throw
%%------------------------------------------------------------------------------
ast(#{op := throw} = Expr, State) ->
  #{exception := ExceptionExpr} = Expr,

  {Exception, State1} = pop_ast(ast(ExceptionExpr, State)),

  Ast = application_mfa(erlang, throw, [Exception]),
  push_ast(Ast, State1).

%%------------------------------------------------------------------------------
%% AST Helper Functions
%%------------------------------------------------------------------------------

-spec function_form(atom(), [ast()]) ->
  ast().
function_form(Name, Clauses) when is_atom(Name) ->
  NameAst = erl_syntax:atom(Name),
  erl_syntax:function(NameAst, Clauses).

-spec export_attribute({atom(), integer()}) ->
  ast().
export_attribute(FAs) when is_list(FAs) ->
  ExportAtom = erl_syntax:atom(export),
  Fun  = fun({Function, Arity}) -> arity_qualifier(Function, Arity) end,
  Asts = lists:map(Fun, FAs),
  ListAst = erl_syntax:list(Asts),
  erl_syntax:attribute(ExportAtom, [ListAst]).

-spec arity_qualifier(atom(), integer()) -> ast().
arity_qualifier(Function, Arity) when is_atom(Function),
                                      is_integer(Arity) ->
  FunctionAtomAst = erl_syntax:atom(Function),
  ArityIntegerAst = erl_syntax:integer(Arity),
  erl_syntax:arity_qualifier(FunctionAtomAst, ArityIntegerAst).

-spec method_to_clause(clj_analyzer:expr(), state()) -> ast().
method_to_clause(MethodExpr, State) ->
  #{ params := ParamsExprs
   , body   := BodyExpr
   } = MethodExpr,

  {Args, State1} = pop_ast( lists:foldl(fun ast/2, State, ParamsExprs)
                          , length(ParamsExprs)
                          ),
  Guards         = [],
  {Body, State2} = pop_ast(ast(BodyExpr, State1)),

  Clause = erl_syntax:clause(Args, Guards, [Body]),
  push_ast(Clause, State2).

-spec application_mfa(module(), atom(), list()) -> ast().
application_mfa(Module, Function, Args) ->
  ModuleTree = erl_syntax:atom(Module),
  FunctionTree = erl_syntax:atom(Function),

  ValQualifier = erl_syntax:module_qualifier(ModuleTree, FunctionTree),

  erl_syntax:application(ValQualifier, Args).

-spec add_functions(module(), atom(), map(), state()) -> state().
add_functions(Module, Name, #{op := fn, methods := Methods}, State) ->
  ParamCountFun = fun(#{params := Params}) -> length(Params) end,
  GroupedMethods = clj_utils:group_by(ParamCountFun, Methods),

  FunctionFun =
    fun(MethodsList, StateAcc) ->
        StateAcc1 = lists:foldl(fun method_to_clause/2, StateAcc, MethodsList),
        {ClausesAst, StateAcc2} = pop_ast(StateAcc1, length(MethodsList)),
        Fun = function_form(Name, ClausesAst),
        add_functions_attributes(Module, [Fun], [], StateAcc2)
    end,

  State1 = lists:foldl(FunctionFun, State, maps:values(GroupedMethods)),

  ExportFun = fun(Arity, StateAcc) ->
                  Attr = export_attribute([{Name, Arity}]),
                  add_functions_attributes(Module, [], [Attr], StateAcc)
              end,

  lists:foldl(ExportFun, State1, maps:keys(GroupedMethods)).

-spec ensure_module(atom(), state()) -> state().
ensure_module(Name, State = #{modules := Modules}) ->
  case maps:is_key(Name, Modules) of
    true  -> State;
    false ->
      {ok, ModuleDef} = clj_module:load(Name),
      State#{modules => Modules#{Name => ModuleDef}}
  end.

-spec add_functions_attributes(atom(), [ast()], [ast()], state()) ->
   state().
add_functions_attributes(Name, Funs, Attrs, State = #{modules := Modules}) ->
  Module = maps:get(Name, Modules),

  Module1 = clj_module:add_functions(Module, Funs),
  Module2 = clj_module:add_attributes(Module1, Attrs),

  State#{modules => Modules#{Name => Module2}}.

-spec push_ast(ast(), state()) -> state().
push_ast(Ast, State = #{asts := Asts}) ->
  State#{asts => [Ast | Asts]}.

-spec pop_ast(state()) -> {ast(), state()}.
pop_ast(State = #{asts := [Ast | Asts]}) ->
  {Ast, State#{asts => Asts}}.

-spec pop_ast(state(), non_neg_integer()) -> {[ast()], state()}.
pop_ast(State = #{asts := Asts}, N) ->
  {ReturnAsts, RestAsts} = lists:split(N, Asts),
  {lists:reverse(ReturnAsts), State#{asts => RestAsts}}.
