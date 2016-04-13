-module(clj_emitter).

-export([ emit/1
        , remove_state/1
        , without_state/3
        ]).

-type ast() :: erl_parse:abstract_form().

-type state() :: #{ asts            => [ast()]
                  , lexical_renames => clj_scope:scope()
                  }.

-spec emit(clj_env:env()) -> clj_env:env().
emit(Env0) ->
  case clj_env:pop_expr(Env0) of
    {undefined, _} -> Env0;
    {Expr, Env} ->
      State = clj_env:get(Env, emitter, initial_state()),
      clj_env:put(Env, emitter, ast(Expr, State))
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
  { [erl_parse:abstract_expr()]
  , clj_env:env()
  }.
remove_state(Env) ->
  State = clj_env:get(Env, emitter, initial_state()),
  Exprs = lists:reverse(maps:get(asts, State)),

  { Exprs
  , clj_env:remove(Env, emitter)
  }.

-spec initial_state() -> state().
initial_state() ->
  #{ asts            => []
   , lexical_renames => clj_scope:new()
   }.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec ast(map(), state()) -> {[ast()], state()}.
ast(#{op := constant, form := Form}, State) ->
  Ast = erl_syntax:revert(erl_syntax:abstract(Form)),
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
  Ast     = {var, 0, binary_to_atom(NameBin, utf8)},

  push_ast(Ast, State);
ast(#{op := local} = Expr, State) ->
  NameSym = maps:get(name, Expr),
  NameBin = case get_lexical_rename(Expr, State) of
              undefined ->
                clj_core:str(NameSym);
              LexicalNameSym ->
                clj_core:str(LexicalNameSym)
              end,
  Ast     = {var, 0, binary_to_atom(NameBin, utf8)},

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
  Ast = {block, 0, Stms ++ [Ret]},

  push_ast(Ast, State2);
%%------------------------------------------------------------------------------
%% def
%%------------------------------------------------------------------------------
ast(#{op := def, var := Var, init := InitExpr} = _Expr, State) ->
  Module  = 'clojerl.Var':module(Var),
  Name    = 'clojerl.Var':function(Var),
  ValName = 'clojerl.Var':val_function(Var),

  ok     = ensure_module(Module),
  VarAst = erl_syntax:revert(erl_syntax:abstract(Var)),

  {ValAst, State1} =
    case InitExpr of
      #{op := fn} = FnExpr ->
        { VarAst
        , add_functions(Module, Name, FnExpr, State)
        };
      _ ->
        {V, S} = pop_ast(ast(InitExpr, State)),
        case 'clojerl.Var':is_dynamic(Var) of
          true  -> {var_val_function(V, VarAst), S};
          false -> {V, S}
        end
    end,

  ValClause = {clause, 0, [], [], [ValAst]},
  ValFunAst = function_form(ValName, [ValClause]),

  clj_module:add_vars(Module, [Var]),
  clj_module:add_functions(Module, [ValFunAst]),
  clj_module:add_exports(Module, [{ValName, 0}]),

  push_ast(VarAst, State1);
%%------------------------------------------------------------------------------
%% fn, invoke, erl_fun
%%------------------------------------------------------------------------------
ast(#{op := fn} = Expr, State) ->
  #{methods := Methods} = Expr,

  State1 = lists:foldl(fun method_to_case_clause/2, State, Methods),
  {ClausesAsts, State2} = pop_ast(State1, length(Methods)),

  ListArgSym  = clj_core:gensym(<<"list_arg">>),
  ListArgName = clj_core:name(ListArgSym),
  ListArgAst  = {var, 0, binary_to_atom(ListArgName, utf8)},
  CaseAst     = {'case', 0, ListArgAst, ClausesAsts},

  #{name := NameSym} = maps:get(local, Expr, undefined),
  Name         = clj_core:name(NameSym),
  NameAtom     = binary_to_atom(Name, utf8),
  FunClauseAst = {clause, 0, [ListArgAst], [], [CaseAst]},
  FunAst       = {named_fun, 0, NameAtom, [FunClauseAst]},

  push_ast(FunAst, State2);
ast(#{op := erl_fun, invoke := true} = Expr, State) ->
  #{ module   := Module
   , function := Function
   } = Expr,

  ModuleAst   = {atom, 0, Module},
  FunctionAst = {atom, 0, Function},
  Ast         = {remote, 0, ModuleAst, FunctionAst},

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

  Ast = {'fun', 0, { function
                   , {atom, 0, Module}
                   , {atom, 0, Function}
                   , {integer, 0, Arity}
                   }
        },

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
      VarMeta = clj_core:meta(Var),
      Module  = 'clojerl.Var':module(Var),

      Ast = case clj_core:get(VarMeta, 'fn?', false) of
              true ->
                Function = 'clojerl.Var':function(Var),
                Args1    = 'clojerl.Var':process_args(Var, Args, fun list_ast/1),
                application_mfa(Module, Function, Args1);
              false ->
                ValFunction = 'clojerl.Var':val_function(Var),
                FunAst      = application_mfa(Module, ValFunction, []),
                ArgsAst     = list_ast(Args),
                application_mfa(clj_core, invoke, [FunAst, ArgsAst])
            end,

      push_ast(Ast, State1);
    #{op := erl_fun} ->
      {FunAst, State2} = pop_ast(ast(FExpr, State1)),
      Ast = {call, 0, FunAst, Args},

      push_ast(Ast, State2);
    _ ->
      {FunAst, State2} = pop_ast(ast(FExpr, State1)),
      ArgsAst = list_ast(Args),
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
  ListItems = list_ast(Items),

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
  ListItems = list_ast(Items),

  Ast = application_mfa('clojerl.Map', new, [ListItems]),
  push_ast(Ast, State2);
ast(#{op := set} = Expr, State) ->
  #{items := ItemsExprs} = Expr,

  {Items, State1} = pop_ast( lists:foldl(fun ast/2, State, ItemsExprs)
                           , length(ItemsExprs)
                           ),
  ListItems = list_ast(Items),

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
  TrueSymbolAt  = binary_to_atom(clj_core:str(TrueSymbol), utf8),
  True          = {var, 0, TrueSymbolAt},
  FalseAtom     = {atom, 0, false},
  UndefinedAtom = {atom, 0, undefined},
  TrueGuards    = [ {op, 2, '=/=', True, FalseAtom}
                  , {op, 2, '=/=', True, UndefinedAtom}
                  ],
  {ThenAst, State2} = pop_ast(ast(ThenExpr, State1)),
  TrueClause    = {clause, 0, [True], [TrueGuards], [ThenAst]},

  Whatever = {var, 0, '_'},
  {ElseAst, State3} = pop_ast(ast(ElseExpr, State2)),
  FalseClause = {clause, 0, [Whatever], [], [ElseAst]},

  Ast = {'case', 0, Test, [TrueClause, FalseClause]},
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
                    MatchAst = {match, 0, Binding, Init},
                    push_ast(MatchAst, StateAcc2)
                end,

  State1            = lists:foldl(MatchAstFun, State, BindingsExprs),
  {Matches, State2} = pop_ast(State1, length(BindingsExprs)),
  {Body,    State3} = pop_ast(ast(BodyExpr, State2)),

  Ast = case Op of
          'let' ->
            Clause = {clause, 0, [], [], Matches ++ [Body]},
            FunAst = {'fun', 0, {clauses, [Clause]}},
            {call, 0, FunAst, []};
          loop  ->
            %% Emit two nested funs for 'loop' expressions.
            %% An outer unnamed fun that initializes the bindings
            %% and an inner named fun that receives the initialized
            %% values as arguments and on every recur.
            StateTmp = lists:foldl(fun ast/2, State3, BindingsExprs),
            {ArgsAsts, _} = pop_ast(StateTmp, length(BindingsExprs)),

            LoopClause = {clause, 0, ArgsAsts, [], [Body]},

            LoopId     = maps:get(loop_id, Expr),
            LoopIdAtom = binary_to_atom(clj_core:str(LoopId), utf8),
            LoopFunAst = {named_fun, 0, LoopIdAtom, [LoopClause]},
            LoopAppAst = {call, 0, LoopFunAst, ArgsAsts},

            Clause = {clause, 0, [], [], Matches ++ [LoopAppAst]},
            FunAst = {'fun', 0, {clauses, [Clause]}},
            {call, 0, FunAst, []}
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

  LoopIdAtom = binary_to_atom(clj_core:str(LoopId), utf8),

  %% We need to use invoke so that recur also works inside functions
  %% (i.e not funs)
  Ast = case LoopType of
          fn ->
            NameAst = {var, 0, LoopIdAtom},
            ArgsAst = list_ast(Args),
            application_mfa(clj_core, invoke, [NameAst, ArgsAst]);
          loop ->
            NameAst = {var, 0, LoopIdAtom},
            {call, 0, NameAst, Args};
          var ->
            NameAst = {atom, 0, LoopIdAtom},
            {call, 0, NameAst, Args}
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
  LocationAst = erl_syntax:revert(erl_syntax:abstract(Location)),

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

  TryAst    = {'try', 0, [Body], [], Catches, After},

  %% We need to wrap everything in a fun to create a new variable scope.
  ClauseAst = {clause, 0, [], [], [TryAst]},
  FunAst    = {'fun', 0, {clauses, [ClauseAst]}},
  ApplyAst  = {call, 0, FunAst, []},

  push_ast(ApplyAst, State2);
%%------------------------------------------------------------------------------
%% catch
%%------------------------------------------------------------------------------
ast(#{op := 'catch'} = Expr, State) ->
  #{ class := ErrType
   , local := Local
   , body  := BodyExpr
   } = Expr,

  ClassAst          = {atom, 0, ErrType},
  {NameAst, State1} = pop_ast(ast(Local, State)),
  ClassNameAst      = {tuple, 0, [ClassAst, NameAst, {var, 0, '_'}]},

  {Body, State2}    = pop_ast(ast(BodyExpr, State1)),

  Ast = {clause, 0, [ClassNameAst], [], [Body]},

  push_ast(Ast, State2).

%%------------------------------------------------------------------------------
%% AST Helper Functions
%%------------------------------------------------------------------------------

-spec list_ast(list()) -> ast().
list_ast([]) ->
  {nil, 0};
list_ast(List) when is_list(List) ->
  list_ast(List, {nil, 0}).

-spec list_ast(list(), any()) -> ast().
list_ast(Heads, Tail) when is_list(Heads) ->
  do_list_ast(Heads, Tail).

-spec do_list_ast(list(), ast()) -> ast().
do_list_ast([], Tail) ->
  Tail;
do_list_ast([H | Hs], Tail) ->
  {cons, 0, H, do_list_ast(Hs, Tail)}.

-spec function_form(atom(), [ast()]) -> ast().
function_form(Name, [Clause | _] = Clauses) when is_atom(Name) ->
  {clause, _, Args, _, _} = Clause,
  {function, 0, Name, length(Args), Clauses}.

-spec method_to_function_clause(clj_analyzer:expr(), state()) -> ast().
method_to_function_clause(MethodExpr, State) ->
  method_to_clause(MethodExpr, State, function).

-spec method_to_case_clause(clj_analyzer:expr(), state()) -> ast().
method_to_case_clause(MethodExpr, State) ->
  method_to_clause(MethodExpr, State, 'case').

-spec method_to_clause(clj_analyzer:expr(), state(), function | 'case') ->
  ast().
method_to_clause(MethodExpr, State0, ClauseFor) ->
  #{ params      := ParamsExprs
   , body        := BodyExpr
   , 'variadic?' := IsVariadic
   } = MethodExpr,

  State00 = add_lexical_renames_scope(State0),
  State = lists:foldl(fun put_lexical_rename/2, State00, ParamsExprs),

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
              [list_ast(lists:droplast(Args), lists:last(Args))];
            'case' ->
              [list_ast(Args)]
          end,

  Clause = {clause, 0, Args1, Guards, [Body]},

  State3 = remove_lexical_renames_scope(State2),

  push_ast(Clause, State3).

-spec application_mfa(module(), atom(), list()) -> ast().
application_mfa(Module, Function, Args) ->
  {call, 0, {remote, 0, {atom, 0, Module}, {atom, 0, Function}}, Args}.

-spec group_methods([map()]) -> #{integer() => [map()]}.
group_methods(Methods) ->
  ParamCountFun = fun(#{params := Params}) -> length(Params) end,
  clj_utils:group_by(ParamCountFun, Methods).

-spec add_functions(module(), atom(), map(), state()) -> state().
add_functions(Module, Name, #{op := fn, methods := Methods}, State) ->
  GroupedMethods = group_methods(Methods),

  ExportFun = fun(Arity) ->
                  clj_module:add_exports(Module, [{Name, Arity}])
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

        clj_module:add_functions(Module, [FunAst]),

        StateAcc2
    end,

  lists:foldl(FunctionFun, State, maps:values(GroupedMethods)).

-spec ensure_module(atom()) -> ok.
ensure_module(Name) ->
  case clj_module:is_loaded(Name) of
    true  -> ok;
    false -> clj_module:load(Name), ok
  end.

%% Push & pop asts

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
  case shadow_depth(BindingExpr) of
    0 -> maps:get(name, BindingExpr);
    _ ->
      Code = hash_scope(BindingExpr),
      clj_scope:get(Renames, Code)
  end.

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

-spec var_val_function(ast(), ast()) -> ast().
var_val_function(Val, VarAst) ->
  TestAst            = application_mfa('clojerl.Var', dynamic_binding, [VarAst]),
  UndefinedAtom      = {atom, 0, undefined},
  UndefinedClauseAst = {clause, 0, [UndefinedAtom], [], [Val]},
  XAtom              = {var, 0, x},
  ValueClauseAst     = {clause, 0, [XAtom], [], [XAtom]},

  {'case', 0, TestAst, [UndefinedClauseAst, ValueClauseAst]}.
