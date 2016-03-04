-module(clj_emitter).

-export([ emit/1
        , remove_state/1
        , without_state/3
        , is_macro/1
        ]).

-type ast() :: erl_syntax:syntaxTree().

-type state() :: #{ asts            => [ast()]
                  , lexical_renames => clj_scope:scope()
                  , is_macro        => boolean()
                  }.

-define(TIME(L, X), clj_utils:time(L, fun() -> X end)).

-spec emit(clj_env:env()) -> clj_env:env().
emit(Env0) ->
  case clj_env:pop_expr(Env0) of
    {undefined, _} -> Env0;
    {Expr, Env} ->
      State = clj_env:get(Env, emitter, initial_state()),
      clj_env:put(Env, emitter, ast(Expr, State#{is_macro => false}))
  end.

%% @doc Applies Fun to the Env and Args but it first removes the emitter
%%      state that could be present in the Env.
-spec without_state(clj_env:env(), function(), [any()]) ->
  clj_env:env().
without_state(Env, Fun, Args) ->
  State = clj_env:get(Env, emitter, initial_state()),
  Env1 = apply(Fun, [clj_env:remove(Env, emitter) | Args]),
  clj_env:put(Env1, emitter, State).

-spec remove_state(clj_env:env()) ->
  { [[erl_parse:abstract_form()]]
  , [erl_parse:abstract_expr()]
  , clj_env:env()
  }.
remove_state(Env) ->
  ModulesForms = lists:map(fun clj_module:to_forms/1, clj_module:all()),

  State = clj_env:get(Env, emitter, initial_state()),
  Exprs = lists:reverse(maps:get(asts, State)),

  { ModulesForms
  , Exprs
  , clj_env:remove(Env, emitter)
  }.

-spec is_macro(clj_env:env()) -> clj_env:env().
is_macro(Env) ->
  #{is_macro := IsMacro} = clj_env:get(Env, emitter, initial_state()),
  IsMacro.

-spec initial_state() -> state().
initial_state() ->
  #{ asts            => []
   , lexical_renames => clj_scope:new()
   , is_macro        => false
   }.

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
  NameSym = maps:get(name, Expr),
  NameBin = case get_lexical_rename(Expr, State) of
              undefined ->
                clj_core:str(NameSym);
              LexicalNameSym ->
                clj_core:str(LexicalNameSym)
              end,
  Ast     = erl_syntax:variable(binary_to_list(NameBin)),

  push_ast(Ast, State);
ast(#{op := local} = Expr, State) ->
  NameSym = maps:get(name, Expr),
  NameBin = case get_lexical_rename(Expr, State) of
              undefined ->
                clj_core:str(NameSym);
              LexicalNameSym ->
                clj_core:str(LexicalNameSym)
              end,
  Ast     = erl_syntax:variable(binary_to_list(NameBin)),

  push_ast(Ast, State);
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
  IsMacro = 'clojerl.Var':is_macro(Var),

  ok = ?TIME("ensure_module", ensure_module(Module)),
  VarAst = erl_syntax:abstract(Var),
  {ValAst, State1} =
    case InitExpr of
      #{op := fn} = FnExpr ->
        { VarAst
        , ?TIME("add_functions", add_functions(Module, Name, FnExpr, State))
        };
      _ ->
        pop_ast(ast(InitExpr, State))
    end,

  ValClause = erl_syntax:clause([], [ValAst]),
  ValFunAst = function_form(ValName, [ValClause]),

  Vars    = [Var],
  Funs    = [ValFunAst],
  Exports = [{ValName, 0}],

  ?TIME(
     "update_module",
     begin
       ok = clj_module:add_vars(Module, Vars),
       ok = clj_module:add_functions(Module, Funs),
       ok = clj_module:add_exports(Module, Exports)
     end
    ),
  push_ast(VarAst, State1#{is_macro => IsMacro});
%%------------------------------------------------------------------------------
%% fn, invoke, erl_fun
%%------------------------------------------------------------------------------
ast(#{op := fn} = Expr, State) ->
  #{methods := Methods} = Expr,

  State1 = lists:foldl(fun method_to_case_clause/2, State, Methods),
  {ClausesAsts, State2} = pop_ast(State1, length(Methods)),

  ListArgSym  = clj_core:gensym(<<"list_arg">>),
  ListArgName = clj_core:name(ListArgSym),
  ListArgAst  = erl_syntax:variable(binary_to_list(ListArgName)),
  CaseAst     = erl_syntax:case_expr(ListArgAst, ClausesAsts),

  #{name := NameSym} = maps:get(local, Expr, undefined),
  Name         = clj_core:name(NameSym),
  NameAst      = erl_syntax:variable(binary_to_list(Name)),
  FunClauseAst = erl_syntax:clause([ListArgAst], none, [CaseAst]),
  FunAst       = erl_syntax:named_fun_expr(NameAst, [FunClauseAst]),

  push_ast(FunAst, State2);
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
   , form     := Symbol
   } = Expr,

  clj_utils:throw_when( Arity == undefined
                      , [ <<"Can't use an erlang function as a value without ">>
                        , <<"specifying its arity: ">>
                        , atom_to_binary(Module, utf8)
                        , <<"/">>
                        , atom_to_binary(Function, utf8)
                        ]
                      , clj_reader:location_meta(Symbol)
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

  TrueSymbol    = clj_core:gensym(<<"true_">>),
  TrueSymbolStr = binary_to_list(clj_core:str(TrueSymbol)),
  True          = erl_syntax:variable(TrueSymbolStr),
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
%% let
%%------------------------------------------------------------------------------
ast(#{op := Op} = Expr, State0) when Op =:= 'let'; Op =:= loop ->
  #{ body     := BodyExpr
   , bindings := BindingsExprs
   } = Expr,

  State00 = add_lexical_renames_scope(State0),
  State = lists:foldl(fun put_lexical_rename/2, State00, BindingsExprs),

  MatchAstFun = fun(BindingExpr = #{init := InitExpr}, StateAcc) ->
                    {Binding, StateAcc1} = pop_ast(ast(BindingExpr, StateAcc)),
                    {Init, StateAcc2}    = pop_ast(ast(InitExpr, StateAcc1)),
                    MatchAst = erl_syntax:match_expr(Binding, Init),
                    push_ast(MatchAst, StateAcc2)
                end,

  State1            = lists:foldl(MatchAstFun, State, BindingsExprs),
  {Matches, State2} = pop_ast(State1, length(BindingsExprs)),
  {Body,    State3} = pop_ast(ast(BodyExpr, State2)),

  Ast = case Op of
          'let' ->
            Clause = erl_syntax:clause([], [], Matches ++ [Body]),
            FunAst = erl_syntax:fun_expr([Clause]),
            erl_syntax:application(FunAst, []);
          loop  ->
            %% Emit two nested funs for 'loop' expressions.
            %% An outer unnamed fun that initializes the bindings
            %% and an inner named fun that receives the initialized
            %% values as arguments and on every recur.
            StateTmp = lists:foldl(fun ast/2, State3, BindingsExprs),
            {ArgsAsts, _} = pop_ast(StateTmp, length(BindingsExprs)),

            LoopClause = erl_syntax:clause(ArgsAsts, [], [Body]),

            LoopId     = maps:get(loop_id, Expr),
            LoopIdStr  = binary_to_list(clj_core:str(LoopId)),
            NameAst    = erl_syntax:variable(LoopIdStr),
            LoopFunAst = erl_syntax:named_fun_expr(NameAst, [LoopClause]),
            LoopAppAst = erl_syntax:application(LoopFunAst, ArgsAsts),

            Clause = erl_syntax:clause([], [], Matches ++ [LoopAppAst]),
            FunAst = erl_syntax:fun_expr([Clause]),
            erl_syntax:application(FunAst, [])
        end,

  State4 = remove_lexical_renames_scope(State3),

  push_ast(Ast, State4);
%%------------------------------------------------------------------------------
%% recur
%%------------------------------------------------------------------------------
ast(#{op := recur} = Expr, State) ->
  #{ loop_id   := LoopId
   , loop_type := LoopType
   , exprs     := ArgsExprs
   } = Expr,

  {Args, State1} = pop_ast( lists:foldl(fun ast/2, State, ArgsExprs)
                          , length(ArgsExprs)
                          ),

  LoopIdStr = binary_to_list(clj_core:str(LoopId)),

  %% We need to use invoke so that recur also works inside functions
  %% (i.e not funs)
  Ast = case LoopType of
          fn ->
            NameAst = erl_syntax:variable(LoopIdStr),
            ArgsAst = erl_syntax:list(Args),
            application_mfa(clj_core, invoke, [NameAst, ArgsAst]);
          loop ->
            NameAst = erl_syntax:variable(LoopIdStr),
            erl_syntax:application(NameAst, Args);
          var ->
            NameAst = erl_syntax:atom(LoopIdStr),
            erl_syntax:application(NameAst, Args)
        end,
  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% throw
%%------------------------------------------------------------------------------
ast(#{op := throw} = Expr, State) ->
  #{ exception := ExceptionExpr
   , form      := Form
   } = Expr,

  {Exception, State1} = pop_ast(ast(ExceptionExpr, State)),
  Location    = clj_reader:location_meta(Form),
  LocationAst = erl_syntax:abstract(Location),

  Ast = application_mfa(clj_utils, throw, [Exception, LocationAst]),
  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% try
%%------------------------------------------------------------------------------
ast(#{op := 'try'} = Expr, State) ->
  #{ body    := BodyExpr
   , catches := CatchesExprs
   , finally := FinallyExpr
   } = Expr,

  {Body, State1} = pop_ast(ast(BodyExpr, State)),
  {Catches, State1} = pop_ast( lists:foldl(fun ast/2, State, CatchesExprs)
                             , length(CatchesExprs)
                             ),

  {Finally, State2} = case FinallyExpr of
                        undefined -> {undefined, State1};
                        _         -> pop_ast(ast(FinallyExpr, State))
                      end,

  After = case Finally of
            undefined -> [];
            _         -> [Finally]
          end,

  TryAst    = erl_syntax:try_expr([Body], [], Catches, After),

  %% We need to wrap everything in a fun to create a new variable scope.
  ClauseAst = erl_syntax:clause([], [TryAst]),
  FunAst    = erl_syntax:fun_expr([ClauseAst]),
  ApplyAst  = erl_syntax:application(FunAst, []),

  push_ast(ApplyAst, State2);
%%------------------------------------------------------------------------------
%% catch
%%------------------------------------------------------------------------------
ast(#{op := 'catch'} = Expr, State) ->
  #{ class := ErrType
   , local := Local
   , body  := BodyExpr
   } = Expr,

  ClassAst          = erl_syntax:atom(ErrType),
  {NameAst, State1} = pop_ast(ast(Local, State)),
  ClassNameAst      = erl_syntax:class_qualifier(ClassAst, NameAst),

  {Body, State2}    = pop_ast(ast(BodyExpr, State1)),

  Ast = erl_syntax:clause([ClassNameAst], [], [Body]),

  push_ast(Ast, State2).

%%------------------------------------------------------------------------------
%% AST Helper Functions
%%------------------------------------------------------------------------------

-spec function_form(atom(), [ast()]) ->
  ast().
function_form(Name, Clauses) when is_atom(Name) ->
  NameAst = erl_syntax:atom(Name),
  erl_syntax:function(NameAst, Clauses).

-spec method_to_function_clause(clj_analyzer:expr(), state()) -> ast().
method_to_function_clause(MethodExpr, State) ->
  method_to_clause(MethodExpr, State, function).

-spec method_to_case_clause(clj_analyzer:expr(), state()) -> ast().
method_to_case_clause(MethodExpr, State) ->
  method_to_clause(MethodExpr, State, 'case').

-spec method_to_clause(clj_analyzer:expr(), state(), function | 'case') ->
  ast().
method_to_clause(MethodExpr, State, ClauseFor) ->
  #{ params      := ParamsExprs
   , body        := BodyExpr
   , 'variadic?' := IsVariadic
   } = MethodExpr,

  {Args, State1} = pop_ast( lists:foldl(fun ast/2, State, ParamsExprs)
                          , length(ParamsExprs)
                          ),
  Guards         = [],
  {Body, State2} = pop_ast(ast(BodyExpr, State1)),

  ParamCount = length(ParamsExprs),
  Args1 = case ClauseFor of
            function ->
              Args;
            'case' when IsVariadic, ParamCount == 1 ->
              Args;
            'case' when IsVariadic ->
              [erl_syntax:list(lists:droplast(Args), lists:last(Args))];
            'case' ->
              [erl_syntax:list(Args)]
          end,

  Clause = erl_syntax:clause(Args1, Guards, [Body]),
  push_ast(Clause, State2).

-spec application_mfa(module(), atom(), list()) -> ast().
application_mfa(Module, Function, Args) ->
  ModuleTree = erl_syntax:atom(Module),
  FunctionTree = erl_syntax:atom(Function),

  ValQualifier = erl_syntax:module_qualifier(ModuleTree, FunctionTree),

  erl_syntax:application(ValQualifier, Args).

-spec group_methods([map()]) -> #{integer() => [map()]}.
group_methods(Methods) ->
  ParamCountFun = fun(#{params := Params}) -> length(Params) end,
  clj_utils:group_by(ParamCountFun, Methods).

-spec add_functions(module(), atom(), map(), state()) -> state().
add_functions(Module, Name, #{op := fn, methods := Methods}, State) ->
  GroupedMethods = group_methods(Methods),

  ExportFun = fun(Arity) ->
                  ok = clj_module:add_exports(Module, [{Name, Arity}])
              end,

  lists:foreach(ExportFun, maps:keys(GroupedMethods)),

  FunctionFun =
    fun(MethodsList, StateAcc) ->
        StateAcc1 = lists:foldl( fun method_to_function_clause/2
                               , StateAcc
                               , MethodsList
                               ),
        {ClausesAst, StateAcc2} = pop_ast(StateAcc1, length(MethodsList)),

        FunAst = function_form(Name, ClausesAst),
        ok = clj_module:add_functions(Module, [FunAst]),

        StateAcc2
    end,

  lists:foldl(FunctionFun, State, maps:values(GroupedMethods)).

-spec ensure_module(atom()) -> ok.
ensure_module(Name) ->
  case clj_module:is_loaded(Name) of
    true  -> ok;
    false -> ok = clj_module:load(Name)
  end.

%% Push & pop asts

-spec push_ast(ast(), state()) -> state().
push_ast(Ast, State = #{asts := Asts}) ->
  Reverted = erl_syntax:revert(Ast),
  State#{asts => [Reverted | Asts]}.

-spec pop_ast(state()) -> {ast(), state()}.
pop_ast(State = #{asts := [Ast | Asts]}) ->
  {Ast, State#{asts => Asts}}.

-spec pop_ast(state(), non_neg_integer()) -> {[ast()], state()}.
pop_ast(State = #{asts := Asts}, N) ->
  {ReturnAsts, RestAsts} = lists:split(N, Asts),
  {lists:reverse(ReturnAsts), State#{asts => RestAsts}}.

%% Lexical renames

-spec add_lexical_renames_scope(state()) -> state().
add_lexical_renames_scope(State = #{lexical_renames := Renames}) ->
  State#{lexical_renames => clj_scope:new(Renames)}.

-spec remove_lexical_renames_scope(state()) -> state().
remove_lexical_renames_scope(State = #{lexical_renames := Renames}) ->
  State#{lexical_renames => clj_scope:parent(Renames)}.

-spec get_lexical_rename(map(), state()) -> 'clojerl.Symbol':type() | undefined.
get_lexical_rename(BindingExpr, State) ->
  #{lexical_renames := Renames} = State,
  Code = hash_scope(BindingExpr),
  clj_scope:get(Renames, Code).

-spec put_lexical_rename(map(), state()) -> state().
put_lexical_rename(BindingExpr, State) ->
  #{lexical_renames := Renames} = State,
  #{name := Name} = BindingExpr,

  Code = hash_scope(BindingExpr),
  NameBin = clj_core:name(Name),
  ShadowName = <<NameBin/binary, "__shadow__">>,

  NewRenames = clj_scope:put(Renames, Code, clj_core:gensym(ShadowName)),

  State#{lexical_renames => NewRenames}.

-spec hash_scope(map()) -> binary().
hash_scope(BindingExpr) ->
  Depth = shadow_depth(BindingExpr),
  #{name := Name} = BindingExpr,
  NameBin = clj_core:name(Name),
  term_to_binary({NameBin, Depth}).

-spec shadow_depth(map()) -> non_neg_integer().
shadow_depth(BindingExpr = #{shadow := _}) ->
  do_shadow_depth(BindingExpr, 0);
shadow_depth(_) ->
  0.

-spec do_shadow_depth(map(), non_neg_integer()) -> non_neg_integer().
do_shadow_depth(#{shadow := Shadowed}, Depth) when Shadowed =/= undefined ->
  do_shadow_depth(Shadowed, Depth + 1);
do_shadow_depth(_, Depth) ->
  Depth.
