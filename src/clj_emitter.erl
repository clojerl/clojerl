-module(clj_emitter).

-include("clojerl.hrl").

-export([ emit/1
        , remove_state/1
        ]).

-type ast()   :: erl_parse:abstract_form().

-type state() :: #{ asts            => [ast()]
                  , lexical_renames => clj_scope:scope()
                  }.

-spec emit(clj_env:env()) -> clj_env:env().
emit(Env0) ->
  {Expr, Env} = clj_env:pop_expr(Env0),
  State       = clj_env:get(Env, emitter, initial_state()),
  clj_env:put(Env, emitter, ast(Expr, State)).

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
  Ast = erl_parse:abstract(Form, line_from(Form)),
  push_ast(Ast, State);
ast(#{op := quote, expr := Expr}, State) ->
  ast(Expr, State);
%%------------------------------------------------------------------------------
%% var, binding & local
%%------------------------------------------------------------------------------
ast(#{op := var} = Expr, State) ->
  #{ var  := Var
   , form := Form
   } = Expr,
  Module = 'clojerl.Var':module(Var),
  Name   = 'clojerl.Var':val_function(Var),
  Ast    = application_mfa(Module, Name, [], anno_from(Form)),

  push_ast(Ast, State);
ast(#{op := binding} = Expr, State) ->
  #{form := Form} = Expr,
  NameBin = get_lexical_rename(Expr, State),
  Ast     = {var, anno_from(Form), binary_to_atom(NameBin, utf8)},

  push_ast(Ast, State);
ast(#{op := local} = Expr, State) ->
  #{name := NameSym} = Expr,
  NameBin = get_lexical_rename(Expr, State),
  Ast     = {var, anno_from(NameSym), binary_to_atom(NameBin, utf8)},

  push_ast(Ast, State);
%% do
%%------------------------------------------------------------------------------
ast(#{op := do} = Expr, State) ->
  #{ statements := StatementsExprs
   , ret        := ReturnExpr
   , form       := Form
   } = Expr,

  StmsCount = length(StatementsExprs),
  {Stms, State1} = pop_ast( lists:foldl(fun ast/2, State, StatementsExprs)
                          , StmsCount
                          ),
  {Ret, State2} = pop_ast(ast(ReturnExpr, State1)),
  Ast = {block, anno_from(Form), Stms ++ [Ret]},

  push_ast(Ast, State2);
%%------------------------------------------------------------------------------
%% def
%%------------------------------------------------------------------------------
ast(#{op := def} = Expr, State) ->
  #{ var  := Var
   , init := InitExpr
   , meta := _MetaExpr
   } = Expr,
  Module  = 'clojerl.Var':module(Var),
  Name    = 'clojerl.Var':function(Var),
  ValName = 'clojerl.Var':val_function(Var),

  ok      = clj_module:ensure_loaded(Module, file_from(Var)),
  VarAst  = erl_parse:abstract(Var),
  VarAnno = anno_from(Var),

  {ValAst, State1} =
    case InitExpr of
      #{op := fn} = FnExpr ->
        { VarAst
        , add_functions(Module, Name, VarAnno, FnExpr, State)
        };
      _ ->
        {V, S} = pop_ast(ast(InitExpr, State)),
        %% If the var is dynamic then the body of the val function needs
        %% to take this into account.
        case 'clojerl.Var':is_dynamic(Var) of
          true  -> {var_val_function(V, VarAst), S};
          false -> {V, S}
        end
    end,

  ValClause = {clause, VarAnno, [], [], [ValAst]},
  ValFunAst = function_form(ValName, VarAnno, [ValClause]),

  clj_module:add_vars(Module, [Var]),
  clj_module:add_functions(Module, [ValFunAst]),
  clj_module:add_exports(Module, [{ValName, 0}]),

  push_ast(VarAst, State1);
%%------------------------------------------------------------------------------
%% import
%%------------------------------------------------------------------------------
ast(#{op := import} = Expr, State) ->
  #{ typename := Typename
   , form     := Form
   } = Expr,

  Type = binary_to_atom(Typename, utf8),

  clj_utils:throw_when( {module, Type} =/= code:ensure_loaded(Type)
                      , [<<"Type '">>, Type, <<"' could not be loaded.">>]
                      , clj_reader:location_meta(Form)
                      ),

  TypenameAst = erl_parse:abstract(Typename),
  ImportAst   = application_mfa(clj_namespace, import_type, [TypenameAst]),

  push_ast(ImportAst, State);
%%------------------------------------------------------------------------------
%% type
%%------------------------------------------------------------------------------
ast(#{op := type} = Expr, State) ->
  #{type := TypeSym} = Expr,

  TypeModule = sym_to_kw(TypeSym),
  Ast        = {atom, anno_from(TypeSym), TypeModule},

  push_ast(Ast, State);
%%------------------------------------------------------------------------------
%% new
%%------------------------------------------------------------------------------
ast(#{op := new} = Expr, State) ->
  #{ type := #{op := type, type := TypeSym}
   , args := ArgsExprs
   } = Expr,

  {ArgsAsts, State1} = pop_ast( lists:foldl(fun ast/2, State, ArgsExprs)
                              , length(ArgsExprs)
                              ),

  TypeModule = sym_to_kw(TypeSym),
  Ast = application_mfa(TypeModule, ?CONSTRUCTOR, ArgsAsts),
  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% deftype
%%------------------------------------------------------------------------------
ast(#{op := deftype} = Expr, State0) ->
  #{ type      := TypeSym
   , name      := Name
   , fields    := FieldsExprs
   , methods   := MethodsExprs
   , protocols := ProtocolsExprs
   } = Expr,

  Module = sym_to_kw(TypeSym),
  Anno   = anno_from(Name),
  ok     = clj_module:ensure_loaded(Module, file_from(Name)),

  %% Attributes
  Attributes     = [ {attribute, 0, behavior, sym_to_kw(ProtocolName)}
                     || #{type := ProtocolName} <- ProtocolsExprs
                   ],

  %% Functions
  {AllFieldsAsts, State} = pop_ast( lists:foldl(fun ast/2, State0, FieldsExprs)
                                   , length(FieldsExprs)
                                   ),
  {MethodsAsts, State1}  = pop_ast( lists:foldl(fun ast/2, State, MethodsExprs)
                                  , length(MethodsExprs)
                                  ),

  %% Expand the first argument to pattern match on all fields so that they are
  %% available in the functions scope.
  TypeTupleAst = type_tuple(Module, AllFieldsAsts, [], match),
  MethodsAsts1 = lists:map( fun(F) ->
                                expand_first_argument(F, TypeTupleAst)
                            end
                          , MethodsAsts
                          ),

  %% Predicate function to check if a field is one of the hidden record fields.
  IsRecordFld = fun({var, _, FieldName}) ->
                    not (FieldName =:= '__meta' orelse FieldName =:= '__extmap')
                  end,
  {FieldsAsts, HiddenFieldsAsts} = lists:partition(IsRecordFld, AllFieldsAsts),

  %% Generate accessor functions for all fields.
  AccessorsAsts = lists:map( fun(F) -> accessor_function(Module, F) end
                           , AllFieldsAsts
                           ),

  %% Create a constructor that doesn't include the hidded fields.
  CtorAst   = constructor_function( Module
                                  , Anno
                                  , AllFieldsAsts
                                  , HiddenFieldsAsts
                                  ),
  %% When there are hidden fields we assume  it is a record, so we also want
  %% to create:
  %%   - a constructor function that includes the hidden fields.
  %%   - a create/1 function that takes a map.
  CtorsAsts = case HiddenFieldsAsts of
                [] -> [CtorAst];
                _  -> [ CtorAst
                      , constructor_function(Module, Anno, AllFieldsAsts, [])
                      , create_function( Module
                                       , Anno
                                       , AllFieldsAsts
                                       , HiddenFieldsAsts
                                       )
                      ]
              end,

  %% Exports
  MethodsExports = lists:map(fun function_signature/1, MethodsAsts1),
  CtorExport     = {?CONSTRUCTOR, length(FieldsAsts)},
  CtorsExports   = case HiddenFieldsAsts of
                     [] -> [CtorExport];
                     _  -> [ CtorExport
                           , {?CONSTRUCTOR, length(AllFieldsAsts)}
                           , {create, 1}
                           ]
                   end,
  AccessorsExports = lists:map( fun({var, _, FName}) ->
                                    {field_fun_name(FName), 1}
                                end
                              , AllFieldsAsts
                              ),

  Exports   = MethodsExports ++ CtorsExports ++ AccessorsExports,
  Functions = CtorsAsts ++ AccessorsAsts ++ MethodsAsts1,

  clj_module:add_attributes(Module, Attributes),
  clj_module:add_exports(Module, Exports),
  clj_module:add_functions(Module, Functions),

  Opts   = #{erl_flags => [binary, debug_info], output_dir => "ebin"},
  Module = clj_compiler:compile_forms(clj_module:get_forms(Module), Opts),

  Ast = erl_parse:abstract(Name, line_from(Name)),

  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% methods
%%------------------------------------------------------------------------------
ast(#{op := method} = Expr, State0) ->
  #{name := Name} = Expr,

  {ClauseAst, State1} = pop_ast(method_to_function_clause(Expr, State0)),
  FunAst = function_form(sym_to_kw(Name), anno_from(Name), [ClauseAst]),

  push_ast(FunAst, State1);
%%------------------------------------------------------------------------------
%% defprotocol
%%------------------------------------------------------------------------------
ast(#{op := defprotocol} = Expr, State) ->
  #{ name         := NameSym
   , methods_sigs := MethodsSigs
   } = Expr,

  Module = sym_to_kw(NameSym),
  ok     = clj_module:ensure_loaded(Module, file_from(NameSym)),

  TermType = {type, 0, term, []},
  CallbackAttrFun = fun(Sig) ->
                        MethodNameSym = clj_core:first(Sig),
                        Arity         = clj_core:second(Sig),
                        ArgsTypes     = lists:duplicate(Arity, TermType),
                        { attribute
                        , 0
                        , callback
                        , { {sym_to_kw(MethodNameSym), Arity}
                          , [{ type
                             , 0
                             , 'fun'
                             , [ {type, 0, product, ArgsTypes}
                               , TermType
                               ]
                             }
                            ]
                          }
                        }
                    end,

  ProtocolAttr = {attribute, 0, protocol, true},
  Attributes   = lists:map(CallbackAttrFun, MethodsSigs),
  clj_module:add_attributes(Module, [ProtocolAttr | Attributes]),

  Ast = erl_parse:abstract(NameSym, line_from(NameSym)),
  push_ast(Ast, State);
%%------------------------------------------------------------------------------
%% defprotocol
%%------------------------------------------------------------------------------
ast(#{op := extend_type} = Expr, State) ->
  #{ op    := extend_type
   , type  := #{type := TypeSym}
   , impls := Impls
   } = Expr,

  EmitProtocolFun =
    fun(#{type := ProtoSym} = Proto, StateAcc) ->
        ProtoBin  = clj_core:str(ProtoSym),
        TypeBin   = clj_core:str(TypeSym),
        ModuleBin = <<ProtoBin/binary, "__", TypeBin/binary>>,
        Module    = erlang:binary_to_atom(ModuleBin, utf8),

        clj_module:ensure_loaded(Module, file_from(TypeSym)),

        MethodsExprs = maps:get(Proto, Impls),

        %% Functions
        StateAcc1 = lists:foldl(fun ast/2, StateAcc, MethodsExprs),
        {FunctionsAsts, StateAcc2} = pop_ast(StateAcc1, length(MethodsExprs)),

        %% Exports
        Exports = lists:map(fun function_signature/1, FunctionsAsts),

        clj_module:add_exports(Module, Exports),
        clj_module:add_functions(Module, FunctionsAsts),

        Opts   = #{erl_flags => [binary, debug_info], output_dir => "ebin"},
        Module = clj_compiler:compile_forms(clj_module:get_forms(Module), Opts),

        StateAcc2
    end,

  State1 = lists:foldl(EmitProtocolFun, State, maps:keys(Impls)),

  Ast = {atom, 0, undefined},
  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% fn, invoke, erl_fun
%%------------------------------------------------------------------------------
ast(#{op := fn} = Expr, State) ->
  #{ methods := Methods
   , form    := Form
   , local   := #{name := NameSym}
   } = Expr,

  State1 = lists:foldl(fun method_to_case_clause/2, State, Methods),
  {ClausesAsts, State2} = pop_ast(State1, length(Methods)),

  Anno        = anno_from(Form),
  ListArgSym  = clj_core:gensym(<<"list_arg">>),
  ListArgName = clj_core:name(ListArgSym),
  ListArgAst  = {var, Anno, binary_to_atom(ListArgName, utf8)},
  CaseAst     = {'case', Anno, ListArgAst, ClausesAsts},

  Name         = clj_core:name(NameSym),
  NameAtom     = binary_to_atom(Name, utf8),
  FunClauseAst = {clause, Anno, [ListArgAst], [], [CaseAst]},
  FunAst       = {named_fun, Anno, NameAtom, [FunClauseAst]},

  push_ast(FunAst, State2);
ast(#{op := erl_fun, invoke := true} = Expr, State) ->
  #{ module   := Module
   , function := Function
   , form     := Form
   } = Expr,

  Anno        = anno_from(Form),
  ModuleAst   = {atom, Anno, Module},
  FunctionAst = {atom, Anno, Function},
  Ast         = {remote, Anno, ModuleAst, FunctionAst},

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

  Anno = anno_from(Symbol),
  Ast  = {'fun', Anno, { function
                       , {atom, Anno, Module}
                       , {atom, Anno, Function}
                       , {integer, Anno, Arity}
                       }
         },

  push_ast(Ast, State);
ast(#{op := invoke} = Expr, State) ->
  #{ args := ArgsExpr
   , f    := FExpr
   , form := Form
   } = Expr,

  Anno = anno_from(Form),

  {Args, State1} = pop_ast( lists:foldl(fun ast/2, State, ArgsExpr)
                          , length(ArgsExpr)
                          ),

  case FExpr of
    #{op := var, var := Var, form := Symbol} ->
      VarMeta = clj_core:meta(Var),
      Module  = 'clojerl.Var':module(Var),

      Ast = case clj_core:get(VarMeta, 'fn?', false) of
              true ->
                Function = 'clojerl.Var':function(Var),
                Args1    = 'clojerl.Var':process_args(Var, Args, fun list_ast/1),
                CurrentNs = clj_namespace:current(),
                NsName    = clj_core:name(clj_namespace:name(CurrentNs)),
                VarNsName = clj_core:namespace(Var),

                %% When the var's symbol is not namespace qualified and the var's
                %% namespace is the current namespace, emit a local function
                %% call, otherwise emit a remote call.
                case clj_core:namespace(Symbol) of
                  undefined when NsName =:= VarNsName ->
                    application_fa(Function, Args1, Anno);
                  _ ->
                    application_mfa(Module, Function, Args1, Anno)
                end;
              false ->
                ValFunction = 'clojerl.Var':val_function(Var),
                FunAst      = application_mfa(Module, ValFunction, [], Anno),
                ArgsAst     = list_ast(Args),
                application_mfa(clj_core, invoke, [FunAst, ArgsAst], Anno)
            end,

      push_ast(Ast, State1);
    #{op := erl_fun} ->
      {FunAst, State2} = pop_ast(ast(FExpr, State1)),
      Ast = {call, Anno, FunAst, Args},

      push_ast(Ast, State2);
    _ ->
      {FunAst, State2} = pop_ast(ast(FExpr, State1)),
      ArgsAst = list_ast(Args),
      Ast     = application_mfa(clj_core, invoke, [FunAst, ArgsAst], Anno),

      push_ast(Ast, State2)
  end;
%%------------------------------------------------------------------------------
%% with-meta
%%------------------------------------------------------------------------------
ast(#{op := with_meta} = WithMetaExpr, State) ->
  #{ meta := Meta
   , expr := Expr
   } = WithMetaExpr,

  {MetaAst, State1} = pop_ast(ast(Meta, State)),
  {ExprAst, State2} = pop_ast(ast(Expr, State1)),

  Ast = application_mfa(clj_core, with_meta, [ExprAst, MetaAst]),

  push_ast(Ast, State2);
%%------------------------------------------------------------------------------
%% Literal data structures
%%------------------------------------------------------------------------------
ast(#{op := vector} = Expr, State) ->
  #{ items := ItemsExprs
   , form  := Form
   } = Expr,

  {Items, State1} = pop_ast( lists:foldl(fun ast/2, State, ItemsExprs)
                           , length(ItemsExprs)
                           ),
  ListItems = list_ast(Items),

  Ast = application_mfa( 'clojerl.Vector'
                       , ?CONSTRUCTOR
                       , [ListItems]
                       , anno_from(Form)
                       ),
  push_ast(Ast, State1);
ast(#{op := map} = Expr, State) ->
  #{ keys := KeysExprs
   , vals := ValsExprs
   , form := Form
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

  Ast = application_mfa( 'clojerl.Map'
                       , ?CONSTRUCTOR
                       , [ListItems]
                       , anno_from(Form)
                       ),
  push_ast(Ast, State2);
ast(#{op := set} = Expr, State) ->
  #{ items := ItemsExprs
   , form  := Form
   } = Expr,

  {Items, State1} = pop_ast( lists:foldl(fun ast/2, State, ItemsExprs)
                           , length(ItemsExprs)
                           ),
  ListItems = list_ast(Items),

  Ast = application_mfa( 'clojerl.Set'
                       , ?CONSTRUCTOR
                       , [ListItems]
                       , anno_from(Form)
                       ),
  push_ast(Ast, State1);
ast(#{op := tuple} = Expr, State) ->
  #{ items := ItemsExprs
   , form  := Form
   } = Expr,

  {Items, State1} = pop_ast( lists:foldl(fun ast/2, State, ItemsExprs)
                           , length(ItemsExprs)
                           ),

  Ast = {tuple, anno_from(Form), Items},
  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% if
%%------------------------------------------------------------------------------
ast(#{op := 'if'} = Expr, State) ->
  #{ test := TestExpr
   , then := ThenExpr
   , else := ElseExpr
   , form := Form
   } = Expr,

  Anno = anno_from(Form),

  {Test, State1} = pop_ast(ast(TestExpr, State)),

  TrueSymbol    = clj_core:gensym(<<"true_">>),
  TrueSymbolAt  = binary_to_atom(clj_core:str(TrueSymbol), utf8),
  True          = {var, Anno, TrueSymbolAt},
  FalseAtom     = {atom, Anno, false},
  UndefinedAtom = {atom, Anno, undefined},
  TrueGuards    = [ {op, 2, '=/=', True, FalseAtom}
                  , {op, 2, '=/=', True, UndefinedAtom}
                  ],
  {ThenAst, State2} = pop_ast(ast(ThenExpr, State1)),
  TrueClause    = {clause, Anno, [True], [TrueGuards], [ThenAst]},

  Whatever = {var, Anno, '_'},
  {ElseAst, State3} = pop_ast(ast(ElseExpr, State2)),
  FalseClause = {clause, Anno, [Whatever], [], [ElseAst]},

  Ast = {'case', Anno, Test, [TrueClause, FalseClause]},
  push_ast(Ast, State3);
%%------------------------------------------------------------------------------
%% case
%%------------------------------------------------------------------------------
ast(#{op := 'case'} = Expr, State) ->
  #{ form    := Form
   , test    := TestExpr
   , clauses := ClausesExprs
   , default := DefaultExpr
   } = Expr,

  {TestAst, State1} = pop_ast(ast(TestExpr, State)),

  ClauseFun = fun({PatternExpr, BodyExpr}, StateAcc) ->
                  {Pattern, StateAcc1} = pop_ast(ast(PatternExpr, StateAcc)),
                  {Body, StateAcc2}    = pop_ast(ast(BodyExpr, StateAcc1)),
                  ClauseAst = {clause, 0, [Pattern], [], [Body]},
                  push_ast(ClauseAst, StateAcc2)
              end,

  State2 = lists:foldl(ClauseFun, State1, ClausesExprs),
  {ClausesAsts0, State3} = pop_ast(State2, length(ClausesExprs)),

  {ClausesAsts, State4} =
    case DefaultExpr of
      undefined -> {ClausesAsts0, State3};
      _ ->
        {DefaultAst, State3_1} = pop_ast(ast(DefaultExpr, State3)),
        DefaultClause = {clause, 0, [{var, 0, '_'}], [], [DefaultAst]},
        {ClausesAsts0 ++ [DefaultClause], State3_1}
    end,

  CaseAst = {'case', anno_from(Form), TestAst, ClausesAsts},

  push_ast(CaseAst, State4);
%%------------------------------------------------------------------------------
%% let
%%------------------------------------------------------------------------------
ast(#{op := Op} = Expr, State0) when Op =:= 'let'; Op =:= loop ->
  #{ body     := BodyExpr
   , bindings := BindingsExprs
   , form := Form
   } = Expr,

  Anno = anno_from(Form),

  State00 = add_lexical_renames_scope(State0),
  State = lists:foldl(fun put_lexical_rename/2, State00, BindingsExprs),

  MatchAstFun = fun(BindingExpr = #{init := InitExpr}, StateAcc) ->
                    #{form := InitForm}  = BindingExpr,
                    {Binding, StateAcc1} = pop_ast(ast(BindingExpr, StateAcc)),
                    {Init, StateAcc2}    = pop_ast(ast(InitExpr, StateAcc1)),
                    MatchAst = {match, anno_from(InitForm), Binding, Init},
                    push_ast(MatchAst, StateAcc2)
                end,

  State1            = lists:foldl(MatchAstFun, State, BindingsExprs),
  {Matches, State2} = pop_ast(State1, length(BindingsExprs)),
  {Body,    State3} = pop_ast(ast(BodyExpr, State2)),

  Ast = case Op of
          'let' ->
            Clause = {clause, Anno, [], [], Matches ++ [Body]},
            FunAst = {'fun', Anno, {clauses, [Clause]}},
            {call, Anno, FunAst, []};
          loop  ->
            %% Emit two nested funs for 'loop' expressions.
            %% An outer unnamed fun that initializes the bindings
            %% and an inner named fun that receives the initialized
            %% values as arguments and on every recur.
            StateTmp = lists:foldl(fun ast/2, State3, BindingsExprs),
            {ArgsAsts, _} = pop_ast(StateTmp, length(BindingsExprs)),

            LoopClause = {clause, Anno, ArgsAsts, [], [Body]},

            LoopId     = maps:get(loop_id, Expr),
            LoopIdAtom = binary_to_atom(clj_core:str(LoopId), utf8),
            LoopFunAst = {named_fun, Anno, LoopIdAtom, [LoopClause]},
            LoopAppAst = {call, Anno, LoopFunAst, ArgsAsts},

            Clause = {clause, Anno, [], [], Matches ++ [LoopAppAst]},
            FunAst = {'fun', Anno, {clauses, [Clause]}},
            {call, Anno, FunAst, []}
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
   , form      := Form
   } = Expr,

  {Args, State1} = pop_ast( lists:foldl(fun ast/2, State, ArgsExprs)
                          , length(ArgsExprs)
                          ),

  LoopIdAtom = binary_to_atom(clj_core:str(LoopId), utf8),

  Anno = anno_from(Form),

  %% We need to use invoke so that recur also works inside functions
  %% (i.e not funs)
  Ast = case LoopType of
          fn ->
            NameAst = {var, Anno, LoopIdAtom},
            ArgsAst = list_ast(Args),
            application_mfa(clj_core, invoke, [NameAst, ArgsAst], Anno);
          loop ->
            NameAst = {var, Anno, LoopIdAtom},
            {call, Anno, NameAst, Args};
          LoopType when LoopType =:= var orelse LoopType =:= function ->
            NameAst = {atom, Anno, LoopIdAtom},
            {call, Anno, NameAst, Args}
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

  Anno = anno_from(Form),
  Ast  = application_mfa(erlang, throw, [Exception], Anno),
  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% try
%%------------------------------------------------------------------------------
ast(#{op := 'try'} = Expr, State) ->
  #{ body    := BodyExpr
   , catches := CatchesExprs
   , finally := FinallyExpr
   , form    := Form
   } = Expr,

  Anno = anno_from(Form),

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

  TryAst    = {'try', Anno, [Body], [], Catches, After},

  %% We need to wrap everything in a fun to create a new variable scope.
  ClauseAst = {clause, Anno, [], [], [TryAst]},
  FunAst    = {'fun', Anno, {clauses, [ClauseAst]}},
  ApplyAst  = {call, Anno, FunAst, []},

  push_ast(ApplyAst, State2);
%%------------------------------------------------------------------------------
%% catch
%%------------------------------------------------------------------------------
ast(#{op := 'catch'} = Expr, State) ->
  #{ class := ErrType
   , local := Local
   , body  := BodyExpr
   , form  := Form
   } = Expr,

  Anno              = anno_from(Form),
  ClassAst          = case ErrType of
                        ErrType when is_atom(ErrType) ->
                          {atom, Anno, ErrType};
                        ErrType -> % If it's not an atom it's a symbol
                          ErrTypeBin = clj_core:name(ErrType),
                          {var, Anno, binary_to_atom(ErrTypeBin, utf8)}
                      end,
  {NameAst, State1} = pop_ast(ast(Local, State)),
  ClassNameAst      = {tuple, Anno, [ClassAst, NameAst, {var, Anno, '_'}]},

  {Body, State2}    = pop_ast(ast(BodyExpr, State1)),

  Ast = {clause, Anno, [ClassNameAst], [], [Body]},

  push_ast(Ast, State2);
%%------------------------------------------------------------------------------
%% on_load
%%------------------------------------------------------------------------------
ast(#{op := on_load} = Expr, State) ->
  #{body := BodyExpr} = Expr,

  {Ast, State1} = pop_ast(ast(BodyExpr, State)),

  CurrentNs  = clj_namespace:current(),
  NameSym    = clj_namespace:name(CurrentNs),
  ModuleName = binary_to_atom(clj_core:name(NameSym), utf8),
  clj_module:add_on_load(ModuleName, Ast),

  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% Unknown op
%%------------------------------------------------------------------------------
ast(#{op := Unknown}, _State) ->
  error({unknown_op, Unknown}).

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

%% @doc Replaces the first argument of a function with a pattern match.
%%
%% This function is used for deftype methods so that the fields are available
%% in the scope of the body of each method.
-spec expand_first_argument(ast(), ast()) -> ast().
expand_first_argument( {function, _, _, _, [ClauseAst]} = Function
                     , TypeTupleAst
                     ) ->
  {clause, _, [FirstAst | RestAsts], _, _} = ClauseAst,

  NewFirstAst  = {match, 0, FirstAst, TypeTupleAst},
  NewClauseAst = erlang:setelement(3, ClauseAst, [NewFirstAst | RestAsts]),

  erlang:setelement(5, Function, [NewClauseAst]).

%% @doc Builds a constructor function for the specified type.
%%
%% The constructor will take AllFields -- HiddenFields as arguments.
%% Hidden fields will be assigned the value `undefined'.
-spec constructor_function(atom(), erl_anno:anno(), [ast()], [ast()]) -> ast().
constructor_function(Typename, Anno, AllFieldsAsts, HiddenFieldsAsts) ->
  FieldsAsts  = AllFieldsAsts -- HiddenFieldsAsts,
  TupleAst    = type_tuple(Typename, AllFieldsAsts, HiddenFieldsAsts, create),
  BodyAst     = [TupleAst],
  ClausesAsts = [{clause, 0, FieldsAsts, [], BodyAst}],

  function_form(?CONSTRUCTOR, Anno, ClausesAsts).

%% @doc Builds an accessor function for the specified type and field.
%%
%% The accessor function generated has a '-' as a prefix.
-spec accessor_function(atom(), ast()) -> ast().
accessor_function(Typename, {var, Anno, FieldName} = FieldAst) ->
  TupleAst     = type_tuple(Typename, [FieldAst], [], match),
  BodyAst      = [FieldAst],
  ClausesAsts  = [{clause, Anno, [TupleAst], [], BodyAst}],
  AccessorName = field_fun_name(FieldName),
  function_form(AccessorName, Anno, ClausesAsts).

-spec create_function(atom(), erl_anno:anno(), [ast()], [ast()]) -> ast().
create_function(Typename, Anno, AllFieldsAsts, HiddenFieldsAsts) ->
  MapVarAst    = {var, Anno, map},
  GetAstFun    = fun(FName) ->
                     ArgsAst = [ MapVarAst
                               , {atom, Anno, FName}
                               ],
                     application_mfa(clj_core, get, ArgsAst)
                 end,

  DissocFoldFun = fun({var, _, FName} = FieldAst, MapAst) ->
                      case lists:member(FieldAst, HiddenFieldsAsts) of
                        true  -> MapAst;
                        false ->
                          FAtom = {atom, Anno, FName},
                          application_mfa(clj_core, dissoc, [MapAst, FAtom])
                      end
                  end,

  %% Coerce argument into a clojerl.Map
  EmptyMapAst   = erl_parse:abstract('clojerl.Map':?CONSTRUCTOR([])),
  ArgsListAst   = list_ast([EmptyMapAst, MapVarAst]),
  MergeCallAst  = application_mfa(clj_core, merge, [ArgsListAst]),

  ExtMapAst     = lists:foldl(DissocFoldFun, MergeCallAst, AllFieldsAsts),

  FieldsMapAst = { map
                 , Anno
                 , [ { map_field_assoc
                     , Anno
                     , {atom, Anno, FieldName}
                     , case lists:member(FieldAst, HiddenFieldsAsts) of
                         true when FieldName =:= '__extmap' ->
                           ExtMapAst;
                         true ->
                           {atom, Anno, undefined};
                         false ->
                           GetAstFun(FieldName)
                       end
                     }
                     || {var, _, FieldName} = FieldAst <- AllFieldsAsts
                   ]
                 },
  InfoAst      = {atom, Anno, undefined},
  TupleAst     = type_tuple_ast(Typename, FieldsMapAst, InfoAst),

  BodyAst      = [TupleAst],

  ClausesAsts  = [{clause, Anno, [MapVarAst], [], BodyAst}],

  function_form(create, Anno, ClausesAsts).

-spec field_fun_name(atom()) -> atom().
field_fun_name(FieldName) when is_atom(FieldName) ->
  list_to_atom("-" ++ atom_to_list(FieldName)).

%% @doc Builds a tuple abstract form tagged with atom ?TYPE which is used
%%      to build Clojerl data types.
-spec type_tuple(atom(), [ast()], [ast()], create | match) -> ast().
type_tuple(Typename, AllFieldsAsts, HiddenFieldsAsts, TupleType) ->
  {MapFieldType, InfoAst} =
    case TupleType of
      create ->
        {map_field_assoc, {atom, 0, undefined}};
      match  ->
        {map_field_exact, {var, 0, '_'}}
    end,

  MapAssocsAsts = [ { MapFieldType
                    , 0
                    , {atom, 0, FieldName}
                    , case lists:member(FieldAst, HiddenFieldsAsts) of
                        true when TupleType =:= create ->
                          {atom, 0, undefined};
                        _ ->
                          FieldAst
                      end
                    }
                    || {var, _, FieldName} = FieldAst <- AllFieldsAsts
                  ],
  MapAst        = {map, 0, MapAssocsAsts},
  type_tuple_ast(Typename, MapAst, InfoAst).

-spec type_tuple_ast(atom(), ast(), ast()) -> ast().
type_tuple_ast(Typename, DataAst, InfoAst) ->
  { tuple
  , 0
  , [ {atom,  0, ?TYPE}
    , {atom,  0, Typename}
    , DataAst
    , InfoAst
    ]
  }.

-spec function_form(atom(), erl_anno:anno(), [ast()]) -> ast().
function_form(Name, Anno, [Clause | _] = Clauses) when is_atom(Name) ->
  {clause, _, Args, _, _} = Clause,
  {function, Anno, Name, length(Args), Clauses}.

-spec function_signature(ast()) -> {atom(), arity()}.
function_signature({function, _, Name, Arity, _}) ->
  {Name, Arity}.

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
   } = MethodExpr,

  %% Get this value this way since it might not be there
  IsVariadic = maps:get('variadic?', MethodExpr, false),

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
  application_mfa(Module, Function, Args, 0).

-spec application_mfa(module(), atom(), list(), erl_anno:anno()) -> ast().
application_mfa(Module, Function, Args, Anno) ->
  { call
  , Anno
  , {remote, Anno, {atom, Anno, Module}, {atom, Anno, Function}}
  , Args
  }.

-spec application_fa(atom(), list(), erl_anno:anno()) -> ast().
application_fa(Function, Args, Anno) ->
  {call, Anno, {atom, Anno, Function}, Args}.

-spec group_methods([map()]) -> #{integer() => [map()]}.
group_methods(Methods) ->
  ParamCountFun = fun(#{params := Params}) -> length(Params) end,
  clj_utils:group_by(ParamCountFun, Methods).

-spec add_functions(module(), atom(), erl_anno:anno(), map(), state()) ->
  state().
add_functions(Module, Name, Anno, #{op := fn, methods := Methods}, State) ->
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

        FunAst = function_form(Name, Anno, ClausesAst),

        clj_module:add_functions(Module, [FunAst]),

        StateAcc2
    end,

  lists:foldl(FunctionFun, State, maps:values(GroupedMethods)).

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

%% @doc Finds and returns the name of the lexical rename.
%%
%% This function always returns something valid because the BindingExpr
%% is always registered in the lexixal scope, the analyzer makes sure
%% this happens.
%% @end
-spec get_lexical_rename(map(), state()) -> binary().
get_lexical_rename(BindingExpr, State) ->
  #{lexical_renames := Renames} = State,

  RenameSym = case shadow_depth(BindingExpr) of
                0 -> maps:get(name, BindingExpr);
                _ ->
                  Code = hash_scope(BindingExpr),
                  clj_scope:get(Renames, Code)
              end,

  clj_core:str(RenameSym).

-spec put_lexical_rename(map(), state()) -> state().
put_lexical_rename(#{shadow := undefined}, State) ->
  State;
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
  XVar               = {var, 0, x},
  TupleAst           = {tuple, 0, [{atom, 0, ok}, XVar]},
  ValueClauseAst     = {clause, 0, [TupleAst], [], [XVar]},

  {'case', 0, TestAst, [UndefinedClauseAst, ValueClauseAst]}.

-spec line_from(any()) -> erl_anno:line().
line_from(Form) ->
  case clj_core:'meta?'(Form) of
    false -> 0;
    true  ->
      case clj_core:meta(Form) of
        undefined -> 0;
        Map ->
          clj_core:get(Map, line, 0)
      end
  end.

-spec file_from(any()) -> binary().
file_from(Form) ->
  case clj_core:meta(Form) of
    undefined -> <<>>;
    Map ->
      clj_core:get(Map, file, <<>>)
  end.

-spec anno_from(any()) -> erl_anno:anno().
anno_from(Form) ->
  case clj_core:'meta?'(Form) of
    false -> 0;
    true  ->
      case clj_core:meta(Form) of
        undefined -> 0;
        Map ->
          Line   = clj_core:get(Map, line, 0),
          Column = clj_core:get(Map, column, 1),
          Anno   = [{location, {Line, Column}}],
          case clj_core:get(Map, file, undefined) of
            undefined -> Anno;
            File      -> [{file, binary_to_list(File)} | Anno]
          end
      end
  end.

-spec sym_to_kw('clojerl.Symbol':type()) -> atom().
sym_to_kw(Symbol) -> binary_to_atom(clj_core:str(Symbol), utf8).
