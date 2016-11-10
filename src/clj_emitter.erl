-module(clj_emitter).

-include("clojerl.hrl").

-export([ emit/1
        , remove_state/1
        , new_c_var/1
        ]).

-type ast()   :: cerl:cerl().

-type state() :: #{ asts                => [ast()]
                  , lexical_renames     => clj_scope:scope()
                  , force_remote_invoke => boolean()
                  }.

-spec emit(clj_env:env()) -> clj_env:env().
emit(Env0) ->
  {Expr, Env} = clj_env:pop_expr(Env0),
  State       = clj_env:get(Env, emitter, initial_state()),
  erlang:put(local_var_counter, 0),
  clj_env:put(Env, emitter, ast(Expr, State)).

-spec remove_state(clj_env:env()) ->
  { [cerl:cerl()]
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
  #{ asts                => []
   , lexical_renames     => clj_scope:new()
   , force_remote_invoke => false
   }.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec ast(map(), state()) -> {[ast()], state()}.
ast(#{op := constant, form := Form, env := Env}, State) ->
  Ast = cerl:ann_abstract(anno_from(Env), Form),
  push_ast(Ast, State);
ast(#{op := quote, expr := Expr}, State) ->
  ast(Expr, State);
%%------------------------------------------------------------------------------
%% var, binding & local
%%------------------------------------------------------------------------------
ast(#{op := var} = Expr, State) ->
  #{ var  := Var
   , env  := Env
   } = Expr,
  Module = 'clojerl.Var':module(Var),
  Name   = 'clojerl.Var':val_function(Var),
  Ast    = call_mfa(Module, Name, [], anno_from(Env)),

  push_ast(Ast, State);
ast(#{op := binding} = Expr, State) ->
  #{env := Env} = Expr,
  NameBin = get_lexical_rename(Expr, State),
  Ast     = cerl:ann_c_var(anno_from(Env), binary_to_atom(NameBin, utf8)),

  push_ast(Ast, State);
ast(#{op := local} = Expr, State) ->
  #{env := Env} = Expr,
  NameBin = get_lexical_rename(Expr, State),
  Ast     = cerl:ann_c_var(anno_from(Env), binary_to_atom(NameBin, utf8)),

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
  Ast = case Stms of
          [] -> Ret;
          [Head | Tail]  ->
            SeqFun = fun(X, Acc) -> cerl:c_seq(Acc, X) end,
            lists:foldl(SeqFun, Head, Tail ++ [Ret])
        end,
  push_ast(Ast, State2);
%%------------------------------------------------------------------------------
%% def
%%------------------------------------------------------------------------------
ast(#{op := def} = Expr, State) ->
  #{ var  := Var
   , init := InitExpr
   , meta := _MetaExpr
   , env  := Env
   } = Expr,
  Module  = 'clojerl.Var':module(Var),
  Name    = 'clojerl.Var':function(Var),
  ValName = 'clojerl.Var':val_function(Var),

  ok      = clj_module:ensure_loaded(Module, file_from(Env)),
  VarAst  = cerl:abstract(Var),
  VarAnno = anno_from(Env),

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
          true  -> {var_val_function(V, VarAst, VarAnno), S};
          false -> {V, S}
        end
    end,

  ValFunAst = function_form(ValName, VarAnno, [], ValAst),

  clj_module:add_mappings(Module, [Var]),
  clj_module:add_functions(Module, [ValFunAst]),
  clj_module:add_exports(Module, [{ValName, 0}]),

  push_ast(VarAst, State1);
%%------------------------------------------------------------------------------
%% import
%%------------------------------------------------------------------------------
ast(#{op := import} = Expr, State) ->
  #{ typename := Typename
   , env      := Env
   } = Expr,

  Parts   = binary:split(Typename, <<".">>, [global]),
  SymName = lists:last(Parts),
  NsName  = 'clojerl.String':join(lists:droplast(Parts), <<".">>),
  Module  = binary_to_atom(NsName, utf8),

  clj_module:ensure_loaded(Module, file_from(Env)),
  %% Add the mapping from type name symbol to fully qualified symbol
  clj_module:add_mappings(Module, [{SymName, clj_core:symbol(Typename)}]),

  TypenameAst = cerl:abstract(Typename),
  Anno        = anno_from(Env),
  ImportAst   = call_mfa(clj_namespace, import_type, [TypenameAst], Anno),

  push_ast(ImportAst, State);
%%------------------------------------------------------------------------------
%% type
%%------------------------------------------------------------------------------
ast(#{op := type} = Expr, State) ->
  #{ type := TypeSym
   , env  := Env
   } = Expr,

  TypeModule = sym_to_kw(TypeSym),
  Ast        = cerl:ann_c_atom(anno_from(Env), TypeModule),

  push_ast(Ast, State);
%%------------------------------------------------------------------------------
%% new
%%------------------------------------------------------------------------------
ast(#{op := new} = Expr, State) ->
  #{ type := TypeExpr
   , args := ArgsExprs
   , env  := Env
   } = Expr,

  {ArgsAsts, State1} = pop_ast( lists:foldl(fun ast/2, State, ArgsExprs)
                              , length(ArgsExprs)
                              ),

  {TypeAst, State2}  = pop_ast(ast(TypeExpr, State1)),
  TypeModule         = cerl:concrete(TypeAst),

  Ast = call_mfa(TypeModule, ?CONSTRUCTOR, ArgsAsts, anno_from(Env)),
  push_ast(Ast, State2);
%%------------------------------------------------------------------------------
%% deftype
%%------------------------------------------------------------------------------
ast(#{op := deftype} = Expr, State0) ->
  #{ type      := TypeSym
   , name      := Name
   , fields    := FieldsExprs
   , methods   := MethodsExprs
   , protocols := ProtocolsExprs
   , env       := Env
   } = Expr,

  Module = sym_to_kw(TypeSym),
  Anno   = anno_from(Env),
  ok     = clj_module:ensure_loaded(Module, file_from(Env)),

  %% Attributes
  Attributes     = [ { cerl:ann_c_atom(Anno, behavior)
                     , cerl:ann_abstract(Anno, [sym_to_kw(ProtocolName)])
                     }
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
  IsRecordFld = fun(Field) ->
                    FieldName = cerl:var_name(Field),
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
                      , creation_function( Module
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
  AccessorsExports = lists:map( fun(FieldAst) ->
                                    FName = cerl:var_name(FieldAst),
                                    {field_fun_name(FName), 1}
                                end
                              , AllFieldsAsts
                              ),

  Exports   = MethodsExports ++ CtorsExports ++ AccessorsExports,
  Functions = CtorsAsts ++ AccessorsAsts ++ MethodsAsts1,

  clj_module:add_attributes(Module, Attributes),
  clj_module:add_exports(Module, Exports),
  clj_module:add_functions(Module, Functions),

  Opts   = clj_env:get(Env, compiler_opts, default_compiler_options()),
  Module = clj_compiler:compile_forms(clj_module:get_forms(Module), Opts),
  ok     = clj_module:remove(Module),

  Ast = cerl:ann_abstract(anno_from(Env), Name),

  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% methods
%%------------------------------------------------------------------------------
ast(#{op := method} = Expr, State0) ->
  #{ name := Name
   , env  := Env
   } = Expr,

  {ClauseAst, State1} = pop_ast(method_to_function_clause(Expr, State0)),
  Args = cerl:clause_pats(ClauseAst),
  Body = cerl:clause_body(ClauseAst),
  FunAst = function_form(sym_to_kw(Name), anno_from(Env), Args, Body),

  push_ast(FunAst, State1);
%%------------------------------------------------------------------------------
%% defprotocol
%%------------------------------------------------------------------------------
ast(#{op := defprotocol} = Expr, State) ->
  #{ name         := NameSym
   , methods_sigs := MethodsSigs
   , env          := Env
   } = Expr,

  Module = sym_to_kw(NameSym),
  ok     = clj_module:ensure_loaded(Module, file_from(Env)),
  Anno   = anno_from(Env),

  TermType = cerl:ann_abstract(Anno, {type, Anno, term, []}),
  CallbackAttrFun = fun(Sig) ->
                        MethodNameSym = clj_core:first(Sig),
                        Arity         = clj_core:second(Sig),
                        ArgsTypes     = lists:duplicate(Arity, TermType),
                        Value = { {sym_to_kw(MethodNameSym), Arity}
                                , [{ type
                                   , Anno
                                   , 'fun'
                                   , [ {type, Anno, product, ArgsTypes}
                                     , TermType
                                     ]
                                   }
                                  ]
                                },
                        { cerl:ann_c_atom(Anno, callback)
                        , cerl:ann_abstract(Anno, Value)
                        }
                    end,

  ProtocolAttr = {cerl:ann_c_atom(Anno, protocol), cerl:c_atom(true)},
  Attributes   = lists:map(CallbackAttrFun, MethodsSigs),
  clj_module:add_attributes(Module, [ProtocolAttr | Attributes]),

  Opts   = clj_env:get(Env, compiler_opts, default_compiler_options()),
  Module = clj_compiler:compile_forms(clj_module:get_forms(Module), Opts),
  ok     = clj_module:remove(Module),

  Ast = cerl:ann_abstract(Anno, NameSym),
  push_ast(Ast, State);
%%------------------------------------------------------------------------------
%% extend_type
%%------------------------------------------------------------------------------
ast(#{op := extend_type} = Expr, State) ->
  #{ op    := extend_type
   , type  := #{type := TypeSym}
   , impls := Impls
   , env   := Env
   } = Expr,

  ForceRemote = maps:get(force_remote_invoke, State),

  EmitProtocolFun =
    fun(#{type := ProtoSym} = Proto, StateAcc) ->
        ProtoBin = clj_core:str(ProtoSym),
        TypeBin  = clj_core:str(TypeSym),
        Module   = 'clojerl.protocol':impl_module(ProtoBin, TypeBin),

        clj_module:ensure_loaded(Module, file_from(Env)),

        MethodsExprs = maps:get(Proto, Impls),

        %% Functions
        StateAcc1 = lists:foldl(fun ast/2, StateAcc, MethodsExprs),
        {FunctionsAsts, StateAcc2} = pop_ast(StateAcc1, length(MethodsExprs)),

        %% Exports
        Exports = lists:map(fun function_signature/1, FunctionsAsts),

        clj_module:add_exports(Module, Exports),
        clj_module:add_functions(Module, FunctionsAsts),

        Opts   = clj_env:get(Env, compiler_opts, default_compiler_options()),
        Module = clj_compiler:compile_forms(clj_module:get_forms(Module), Opts),
        ok     = clj_module:remove(Module),

        StateAcc2
    end,

  %% We force remote calls for all functions since the function will
  %% live in its own module, but is analyzed in the context of the
  %% surrounding code where the extend-type is used.
  State1 = lists:foldl( EmitProtocolFun
                      , State#{force_remote_invoke => true}
                      , maps:keys(Impls)
                      ),

  Ast = cerl:ann_c_atom(anno_from(Env), ?NIL),
  push_ast(Ast, State1#{force_remote_invoke => ForceRemote});
%%------------------------------------------------------------------------------
%% fn, invoke, erl_fun
%%------------------------------------------------------------------------------
ast(#{op := fn} = Expr, State) ->
  #{ local   := LocalExpr = #{name := NameSym}
   , env     := Env
   } = Expr,

  Anno = anno_from(Env),

  {DefsAsts, State1} = letrec_defs([LocalExpr], [Expr], State),

  NameAtom = sym_to_kw(NameSym),
  FName    = cerl:ann_c_fname(Anno, NameAtom, 1),
  Ast      = cerl:ann_c_letrec(Anno, DefsAsts, FName),

  push_ast(Ast, State1);
ast(#{op := erl_fun} = Expr, State) ->
  #{ module   := Module
   , function := Function
   , arity    := Arity
   , env      := Env
   } = Expr,

  clj_utils:error_when( Arity == ?NIL
                      , [ <<"Can't use an erlang function as a value without ">>
                        , <<"specifying its arity: ">>
                        , atom_to_binary(Module, utf8)
                        , <<"/">>
                        , atom_to_binary(Function, utf8)
                        ]
                      , clj_env:location(Env)
                      ),

  Anno = anno_from(Env),
  Ast  = call_mfa( erlang
                 , make_fun
                 , [ cerl:ann_c_atom(Anno, Module)
                   , cerl:ann_c_atom(Anno, Function)
                   , cerl:ann_c_int(Anno, Arity)
                   ]
                 , Anno
                 ),

  push_ast(Ast, State);
ast(#{op := invoke} = Expr, State) ->
  #{ args := ArgsExpr
   , f    := FExpr
   , env  := Env
   } = Expr,

  Anno = anno_from(Env),

  {Args, State1} = pop_ast( lists:foldl(fun ast/2, State, ArgsExpr)
                          , length(ArgsExpr)
                          ),

  case FExpr of
    #{op := var, var := Var, form := Symbol} ->
      VarMeta = clj_core:meta(Var),
      Module  = 'clojerl.Var':module(Var),

      Ast =
        case clj_core:get(VarMeta, 'fn?', false) of
          true ->
            Function    = 'clojerl.Var':function(Var),
            Args1       = 'clojerl.Var':process_args(Var, Args, fun list_ast/1),
            CurrentNs   = clj_namespace:current(),
            NsName      = clj_core:name(clj_namespace:name(CurrentNs)),
            VarNsName   = clj_core:namespace(Var),
            ForceRemote = maps:get(force_remote_invoke, State),
            %% When the var's symbol is not namespace qualified and the var's
            %% namespace is the current namespace, emit a local function
            %% call, otherwise emit a remote call.
            case clj_core:namespace(Symbol) of
              ?NIL when NsName =:= VarNsName, not ForceRemote ->
                call_fa(Function, Args1, Anno);
              _ ->
                call_mfa(Module, Function, Args1, Anno)
            end;
          false ->
            ValFunction = 'clojerl.Var':val_function(Var),
            FunAst      = call_mfa(Module, ValFunction, [], Anno),
            ArgsAst     = list_ast(Args),
            call_mfa('clojerl.IFn', apply, [FunAst, ArgsAst], Anno)
        end,

      push_ast(Ast, State1);
    #{ op       := erl_fun
     , module   := Module
     , function := Function
     , env      := Env
     } ->

      Anno = anno_from(Env),
      Ast  = call_mfa(Module, Function, Args, Anno),
      push_ast(Ast, State);
    _ ->
      {FunAst, State2} = pop_ast(ast(FExpr, State1)),
      ArgsAst = list_ast(Args),
      Ast     = call_mfa('clojerl.IFn', apply, [FunAst, ArgsAst], Anno),

      push_ast(Ast, State2)
  end;
%%------------------------------------------------------------------------------
%% letfn
%%------------------------------------------------------------------------------
ast(#{op := letfn} = Expr, State) ->
  #{ vars := VarsExprs
   , fns  := FnsExprs
   , body := BodyExpr
   , env  := Env
   } = Expr,

  Anno = anno_from(Env),

  {DefsAsts, State1} = letrec_defs(VarsExprs, FnsExprs, State),
  {BodyAst,  State2} = pop_ast(ast(BodyExpr, State1)),

  FNamesAsts = [FNameAst || {FNameAst, _} <- DefsAsts],
  VarsAsts   = [cerl:c_var(cerl:fname_id(FNameAst)) || FNameAst <- FNamesAsts],

  LetAst    = cerl:ann_c_let( Anno
                            , VarsAsts
                            , cerl:c_values(FNamesAsts)
                            , BodyAst
                            ),
  LetRecAst = cerl:ann_c_letrec(Anno, DefsAsts, LetAst),

  push_ast(LetRecAst, State2);
%%------------------------------------------------------------------------------
%% with-meta
%%------------------------------------------------------------------------------
ast(#{op := with_meta} = WithMetaExpr, State) ->
  #{ meta := Meta
   , expr := Expr
   , env  := Env
   } = WithMetaExpr,

  {MetaAst, State1} = pop_ast(ast(Meta, State)),
  {ExprAst, State2} = pop_ast(ast(Expr, State1)),

  Ast = call_mfa(clj_core, with_meta, [ExprAst, MetaAst], anno_from(Env)),

  push_ast(Ast, State2);
%%------------------------------------------------------------------------------
%% Literal data structures
%%------------------------------------------------------------------------------
ast(#{op := vector} = Expr, State) ->
  #{ items := ItemsExprs
   , env   := Env
   } = Expr,

  {Items, State1} = pop_ast( lists:foldl(fun ast/2, State, ItemsExprs)
                           , length(ItemsExprs)
                           ),
  ListItems = list_ast(Items),

  Ast = call_mfa('clojerl.Vector', ?CONSTRUCTOR, [ListItems], anno_from(Env)),
  push_ast(Ast, State1);
ast(#{op := map} = Expr, State) ->
  #{ keys := KeysExprs
   , vals := ValsExprs
   , env  := Env
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

  Ast = call_mfa('clojerl.Map', ?CONSTRUCTOR, [ListItems], anno_from(Env)),
  push_ast(Ast, State2);
ast(#{op := set} = Expr, State) ->
  #{ items := ItemsExprs
   , env   := Env
   } = Expr,

  {Items, State1} = pop_ast( lists:foldl(fun ast/2, State, ItemsExprs)
                           , length(ItemsExprs)
                           ),
  ListItems = list_ast(Items),

  Ast = call_mfa('clojerl.Set', ?CONSTRUCTOR, [ListItems], anno_from(Env)),
  push_ast(Ast, State1);
ast(#{op := tuple} = Expr, State) ->
  #{ items := ItemsExprs
   , env   := Env
   } = Expr,

  {Items, State1} = pop_ast( lists:foldl(fun ast/2, State, ItemsExprs)
                           , length(ItemsExprs)
                           ),

  Ast = cerl:ann_c_tuple(anno_from(Env), Items),
  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% if
%%------------------------------------------------------------------------------
ast(#{op := 'if'} = Expr, State) ->
  #{ test := TestExpr
   , then := ThenExpr
   , else := ElseExpr
   , env  := Env
   } = Expr,

  Anno = anno_from(Env),

  {Test, State1} = pop_ast(ast(TestExpr, State)),
  TrueAtom       = cerl:ann_c_atom(Anno, true),
  {ThenAst, State2} = pop_ast(ast(ThenExpr, State1)),
  TrueClause     = cerl:ann_c_clause(Anno, [TrueAtom], ThenAst),

  FalseAtom      = cerl:ann_c_atom(Anno, false),
  {ElseAst, State3} = pop_ast(ast(ElseExpr, State2)),
  FalseClause    = cerl:ann_c_clause(Anno, [FalseAtom], ElseAst),

  TestBoolean    = call_mfa(clj_core, boolean, [Test], Anno),
  Ast = cerl:ann_c_case(Anno, TestBoolean, [TrueClause, FalseClause]),
  push_ast(Ast, State3);
%%------------------------------------------------------------------------------
%% case
%%------------------------------------------------------------------------------
ast(#{op := 'case'} = Expr, State) ->
  #{ test    := TestExpr
   , clauses := ClausesExprs
   , default := DefaultExpr
   , env     := Env
   } = Expr,

  {TestAst, State1} = pop_ast(ast(TestExpr, State)),

  ClauseFun = fun({PatternExpr, BodyExpr}, StateAcc) ->
                  EnvPattern  = maps:get(env, PatternExpr),
                  AnnoPattern = anno_from(EnvPattern),
                  {Pattern, StateAcc1} = pop_ast(ast(PatternExpr, StateAcc)),
                  {Body, StateAcc2}    = pop_ast(ast(BodyExpr, StateAcc1)),
                  ClauseAst = cerl:ann_c_clause(AnnoPattern, [Pattern], Body),
                  push_ast(ClauseAst, StateAcc2)
              end,

  State2 = lists:foldl(ClauseFun, State1, ClausesExprs),
  {ClausesAsts0, State3} = pop_ast(State2, length(ClausesExprs)),

  {ClausesAsts, State4} =
    case DefaultExpr of
      ?NIL -> {ClausesAsts0, State3};
      _ ->
        EnvDefault    = maps:get(env, DefaultExpr),
        AnnoDefault   = anno_from(EnvDefault),
        DefaultVarAst = new_c_var(AnnoDefault),
        {DefaultAst, State3_1} = pop_ast(ast(DefaultExpr, State3)),
        DefaultClause = cerl:ann_c_clause( AnnoDefault
                                         , [DefaultVarAst]
                                         , DefaultAst
                                         ),
        {ClausesAsts0 ++ [DefaultClause], State3_1}
    end,

  CaseAst = cerl:ann_c_case(anno_from(Env), TestAst, ClausesAsts),

  push_ast(CaseAst, State4);
%%------------------------------------------------------------------------------
%% let
%%------------------------------------------------------------------------------
ast(#{op := Op} = Expr, State0) when Op =:= 'let'; Op =:= loop ->
  #{ body     := BodyExpr
   , bindings := BindingsExprs
   , env      := Env
   } = Expr,

  Anno = anno_from(Env),

  State00 = add_lexical_renames_scope(State0),
  State = lists:foldl(fun put_lexical_rename/2, State00, BindingsExprs),

  MatchAstFun =
    fun (BindingExpr = #{init := InitExpr}, {Bindings, StateAcc}) ->
        {Var,  StateAcc1} = pop_ast(ast(BindingExpr, StateAcc)),
        {Init, StateAcc2} = pop_ast(ast(InitExpr, StateAcc1)),
        {[{Var, Init} | Bindings], StateAcc2}
    end,

  {Bindings, State1} =
    lists:foldl(MatchAstFun, {[], State}, BindingsExprs),
  {Body, State2} = pop_ast(ast(BodyExpr, State1)),
  FoldFun        = fun ({Var, Init}, BodyAcc) ->
                       cerl:ann_c_let(Anno, [Var], Init, BodyAcc)
                   end,

  Ast = case {Op, Bindings} of
          {'let', []} ->
            Body;
          {'let',  _} ->
            [{Var, Init} | RestBindings] = Bindings,
            LetInit = cerl:ann_c_let(Anno, [Var], Init, Body),
            lists:foldl(FoldFun, LetInit, RestBindings);
          {loop, _} ->
            Vars       = [V || {V, _} <- lists:reverse(Bindings)],

            LoopId     = maps:get(loop_id, Expr),
            LoopIdAtom = binary_to_atom(clj_core:str(LoopId), utf8),

            FNameAst   = cerl:ann_c_fname(Anno, LoopIdAtom, length(Vars)),
            FunAst     = cerl:ann_c_fun(Anno, Vars, Body),
            Defs       = [{FNameAst, FunAst}],
            ApplyAst   = cerl:ann_c_apply([local | Anno], FNameAst, Vars),
            LetRecAst  = cerl:ann_c_letrec(Anno, Defs, ApplyAst),

            lists:foldl(FoldFun, LetRecAst, Bindings)
        end,

  State3 = remove_lexical_renames_scope(State2),

  push_ast(Ast, State3);
%%------------------------------------------------------------------------------
%% recur
%%------------------------------------------------------------------------------
ast(#{op := recur} = Expr, State) ->
  #{ loop_id   := LoopId
   , loop_type := LoopType
   , exprs     := ArgsExprs
   , env       := Env
   } = Expr,

  Anno           = anno_from(Env),
  {Args, State1} = pop_ast( lists:foldl(fun ast/2, State, ArgsExprs)
                          , length(ArgsExprs)
                          ),
  LoopIdAtom     = binary_to_atom(clj_core:str(LoopId), utf8),

  %% We need to use invoke so that recur also works inside functions
  %% (i.e not funs)
  Ast = case LoopType of
          fn ->
            NameAst = cerl:ann_c_var(Anno, LoopIdAtom),
            ArgsAst = list_ast(Args),
            call_mfa('clojerl.IFn', apply, [NameAst, ArgsAst], Anno);
          loop ->
            NameAst = cerl:ann_c_fname(Anno, LoopIdAtom, length(Args)),
            cerl:ann_c_apply([local | Anno], NameAst, Args);
          LoopType when LoopType =:= var orelse LoopType =:= function ->
            NameAst = cerl:ann_c_fname(Anno, LoopIdAtom, length(Args)),
            cerl:ann_c_apply([local | Anno], NameAst, Args)
        end,
  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% throw
%%------------------------------------------------------------------------------
ast(#{op := throw} = Expr, State) ->
  #{ exception := ExceptionExpr
   , env       := Env
   } = Expr,

  {Exception, State1} = pop_ast(ast(ExceptionExpr, State)),

  Anno = anno_from(Env),
  Ast  = call_mfa(erlang, throw, [Exception], Anno),
  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% try
%%------------------------------------------------------------------------------
ast(#{op := 'try'} = Expr, State) ->
  #{ body    := BodyExpr
   , catches := CatchesExprs
   , finally := FinallyExpr
   , env     := Env
   } = Expr,

  Anno = anno_from(Env),

  {BodyAst, State1} = pop_ast(ast(BodyExpr, State)),
  {Catches, State1} = pop_ast( lists:foldl(fun ast/2, State, CatchesExprs)
                             , length(CatchesExprs)
                             ),

  [_, Y, Z] = CatchVarsAsts = [ new_c_var(Anno)
                              , new_c_var(Anno)
                              , new_c_var(Anno)
                              ],
  RaiseAst          = cerl:c_primop(cerl:c_atom(raise), [Z, Y]),

  CatchAllClause    = cerl:ann_c_clause(Anno, CatchVarsAsts, RaiseAst),
  { ClausesVars
  , CaseAst
  } = case_from_clauses(Anno, Catches ++ [CatchAllClause]),

  {Finally, State2} = case FinallyExpr of
                        ?NIL -> {?NIL, State1};
                        _         -> pop_ast(ast(FinallyExpr, State))
                      end,

  VarAst = new_c_var(Anno),
  TryAst = cerl:ann_c_try( Anno
                         , BodyAst
                         , [VarAst]
                         , VarAst
                         , ClausesVars
                         , CaseAst
                         ),

  Ast    =
    case Finally of
      ?NIL -> TryAst;
      _         ->
        %% after function
        FinallyName     = cerl:var_name(new_c_var(Anno)),
        FinallyFName    = cerl:ann_c_fname(Anno, FinallyName, 0),
        FinallyFunAst   = cerl:ann_c_fun(Anno, [], Finally),
        ApplyFinallyAst = cerl:ann_c_apply([local | Anno], FinallyFName, []),

        Defs = [{FinallyFName, FinallyFunAst}],
        OuterVarAst  = new_c_var(Anno),
        OuterBodyAst = cerl:ann_c_seq(Anno, ApplyFinallyAst, OuterVarAst),
        HandlerAst   = cerl:ann_c_seq(Anno, ApplyFinallyAst, RaiseAst),
        OuterTryAst  = cerl:ann_c_try( Anno
                                     , TryAst
                                     , [OuterVarAst]
                                     , OuterBodyAst
                                     , CatchVarsAsts
                                     , HandlerAst
                                     ),
        cerl:ann_c_letrec(Anno, Defs, OuterTryAst)
    end,

  push_ast(Ast, State2);
%%------------------------------------------------------------------------------
%% catch
%%------------------------------------------------------------------------------
ast(#{op := 'catch'} = Expr, State) ->
  #{ class := ErrType
   , local := Local
   , body  := BodyExpr
   , env   := Env
   } = Expr,

  Anno              = anno_from(Env),
  ClassAst          = case ErrType of
                        ErrType when is_atom(ErrType) ->
                          cerl:ann_c_atom(Anno, ErrType);
                        ErrType -> % If it's not an atom it's a symbol
                          ErrTypeBin = clj_core:name(ErrType),
                          cerl:ann_c_var(Anno, binary_to_atom(ErrTypeBin, utf8))
                      end,
  {NameAst, State1} = pop_ast(ast(Local, State)),
  VarsAsts          = [ ClassAst
                      , NameAst
                      , new_c_var(Anno)
                      ],
  {Body, State2}    = pop_ast(ast(BodyExpr, State1)),

  Ast = cerl:ann_c_clause(Anno, VarsAsts, Body),

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
  cerl:c_nil();
list_ast(List) when is_list(List) ->
  list_ast(List, cerl:c_nil()).

-spec list_ast(list(), any()) -> ast().
list_ast(Heads, Tail) when is_list(Heads) ->
  do_list_ast(Heads, Tail).

-spec do_list_ast(list(), ast()) -> ast().
do_list_ast([], Tail) ->
  Tail;
do_list_ast([H | Hs], Tail) ->
  cerl:c_cons(H, do_list_ast(Hs, Tail)).

%% @doc Replaces the first argument of a function with a pattern match.
%%
%% This function is used for deftype methods so that the fields are available
%% in the scope of the body of each method.
-spec expand_first_argument(ast(), ast()) -> ast().
expand_first_argument( {FNameAst, FunAst}
                     , TypeTupleAst
                     ) ->
  Vars = cerl:fun_vars(FunAst),
  Body = cerl:fun_body(FunAst),
  Anno = cerl:get_ann(FunAst),
  [_ | VarsTail] = Vars,
  ClauseAst = cerl:c_clause([TypeTupleAst | VarsTail], Body),
  CaseAst   = cerl:ann_c_case(Anno, cerl:c_values(Vars), [ClauseAst]),

  { FNameAst
  , cerl:ann_c_fun(Anno, Vars, CaseAst)
  }.

%% @doc Builds a constructor function for the specified type.
%%
%% The constructor will take AllFields -- HiddenFields as arguments.
%% Hidden fields will be assigned the value `?NIL'.
-spec constructor_function(atom(), erl_anno:anno(), [ast()], [ast()]) -> ast().
constructor_function(Typename, Anno, AllFieldsAsts, HiddenFieldsAsts) ->
  FieldsAsts  = AllFieldsAsts -- HiddenFieldsAsts,
  TupleAst    = type_tuple(Typename, AllFieldsAsts, HiddenFieldsAsts, create),
  function_form(?CONSTRUCTOR, Anno, FieldsAsts, TupleAst).

%% @doc Builds an accessor function for the specified type and field.
%%
%% The accessor function generated has a '-' as a prefix.
-spec accessor_function(atom(), ast()) -> ast().
accessor_function(Typename, FieldAst) ->
  Anno         = cerl:get_ann(FieldAst),
  FieldName    = cerl:var_name(FieldAst),
  TupleAst     = type_tuple(Typename, [FieldAst], [], match),
  AccessorName = field_fun_name(FieldName),
  ClauseAst    = cerl:ann_c_clause(Anno, [TupleAst], FieldAst),
  {Vars, CaseAst} = case_from_clauses(Anno, [ClauseAst]),
  function_form(AccessorName, Anno, Vars, CaseAst).

-spec creation_function(atom(), erl_anno:anno(), [ast()], [ast()]) -> ast().
creation_function(Typename, Anno, AllFieldsAsts, HiddenFieldsAsts) ->
  MapVarAst    = new_c_var(Anno),
  GetAstFun    = fun(FName) ->
                     ArgsAst = [ MapVarAst
                               , cerl:ann_c_atom(Anno, FName)
                               ],
                     call_mfa(clj_core, get, ArgsAst, Anno)
                 end,

  DissocFoldFun = fun(FieldAst, MapAst) ->
                      FName = cerl:var_name(FieldAst),
                      case lists:member(FieldAst, HiddenFieldsAsts) of
                        true  -> MapAst;
                        false ->
                          FAtom = cerl:ann_c_atom(Anno, FName),
                          call_mfa(clj_core, dissoc, [MapAst, FAtom], Anno)
                      end
                  end,

  %% Coerce argument into a clojerl.Map
  EmptyMapAst   = cerl:abstract('clojerl.Map':?CONSTRUCTOR([])),
  ArgsListAst   = list_ast([EmptyMapAst, MapVarAst]),
  MergeCallAst  = call_mfa(clj_core, merge, [ArgsListAst], Anno),

  ExtMapAst     = lists:foldl(DissocFoldFun, MergeCallAst, AllFieldsAsts),

  AssocAtom     = cerl:c_atom(assoc),
  NilAtom       = cerl:c_atom(?NIL),
  MapPairsFun   = fun(FieldAst) ->
                      FieldName = cerl:var_name(FieldAst),
                      Value = case lists:member(FieldAst, HiddenFieldsAsts) of
                                true when FieldName =:= '__extmap' ->
                                  ExtMapAst;
                                true ->
                                  NilAtom;
                                false ->
                                  GetAstFun(FieldName)
                              end,
                      cerl:ann_c_map_pair( Anno
                                         , AssocAtom
                                         , cerl:ann_c_atom(Anno, FieldName)
                                         , Value
                                         )
                  end,
  MapPairs      = lists:map(MapPairsFun, AllFieldsAsts),
  FieldsMapAst  = cerl:ann_c_map(Anno, MapPairs),
  InfoAst       = NilAtom,
  TupleAst      = type_tuple_ast(Typename, FieldsMapAst, InfoAst),

  function_form(create, Anno, [MapVarAst], TupleAst).

-spec field_fun_name(atom()) -> atom().
field_fun_name(FieldName) when is_atom(FieldName) ->
  list_to_atom("-" ++ atom_to_list(FieldName)).

%% @doc Builds a tuple abstract form tagged with atom ?TYPE which is used
%%      to build Clojerl data types.
-spec type_tuple(atom(), [cerl:cerl()], [cerl:cerl()], create | match) ->
  cerl:cerl().
type_tuple(Typename, AllFieldsAsts, HiddenFieldsAsts, TupleType) ->
  {MapFieldType, InfoAst} = case TupleType of
                              create -> {assoc, cerl:c_atom(?NIL)};
                              match  -> {exact, new_c_var([])}
                            end,

  NilAtom       = cerl:c_atom(?NIL),
  MapPairFun    = fun(FieldAst) ->
                      FieldName = cerl:var_name(FieldAst),
                      Value = case lists:member(FieldAst, HiddenFieldsAsts) of
                                true when TupleType =:= create ->
                                  NilAtom;
                                _ ->
                                  FieldAst
                              end,
                      cerl:ann_c_map_pair( 0
                                         , cerl:c_atom(MapFieldType)
                                         , cerl:c_atom(FieldName)
                                         , Value
                                         )
                  end,
  MapPairsAsts  = lists:map(MapPairFun, AllFieldsAsts),
  MapAst        = case TupleType of
                    create -> cerl:c_map(MapPairsAsts);
                    match  -> cerl:c_map_pattern(MapPairsAsts)
                  end,
  type_tuple_ast(Typename, MapAst, InfoAst).

-spec type_tuple_ast(atom(), ast(), ast()) -> ast().
type_tuple_ast(Typename, DataAst, InfoAst) ->
  cerl:c_tuple([ cerl:c_atom(?TYPE)
               , cerl:c_atom(Typename)
               , DataAst
               , InfoAst
               ]).

-spec function_form(atom(), [term()], [cerl:cerl()], cerl:cerl()) -> cerl:cerl().
function_form(Name, Anno, Args, Body) when is_atom(Name) ->
  EvalName = cerl:c_fname(Name, length(Args)),
  EvalFun  = cerl:ann_c_fun(Anno, Args, Body),
  {EvalName, EvalFun}.

-spec function_signature(ast()) -> {atom(), arity()}.
function_signature({FName, _}) ->
  {cerl:fname_id(FName), cerl:fname_arity(FName)}.

-spec method_to_function_clause(clj_analyzer:expr(), state()) -> ast().
method_to_function_clause(MethodExpr, State) ->
  method_to_clause(MethodExpr, State, function).

-spec method_to_case_clause(clj_analyzer:expr(), state()) -> ast().
method_to_case_clause(MethodExpr, State) ->
  method_to_clause(MethodExpr, State, 'case').

-spec method_to_clause(clj_analyzer:expr(), state(), function | 'case') ->
  ast().
method_to_clause(MethodExpr, State0, ClauseFor) ->
  #{ params := ParamsExprs
   , body   := BodyExpr
   , env    := Env
   } = MethodExpr,

  %% Get this value this way since it might not be there
  IsVariadic = maps:get('variadic?', MethodExpr, false),

  State00 = add_lexical_renames_scope(State0),
  State = lists:foldl(fun put_lexical_rename/2, State00, ParamsExprs),

  {Args, State1} = pop_ast( lists:foldl(fun ast/2, State, ParamsExprs)
                          , length(ParamsExprs)
                          ),

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

  Clause = cerl:ann_c_clause(anno_from(Env), Args1, Body),

  State3 = remove_lexical_renames_scope(State2),

  push_ast(Clause, State3).

-spec call_mfa(module(), atom(), list(), erl_anno:anno()) -> ast().
call_mfa(Module, Function, Args, Anno) ->
  cerl:ann_c_call(Anno, cerl:c_atom(Module), cerl:c_atom(Function), Args).

-spec call_fa(atom(), list(), erl_anno:anno()) -> ast().
call_fa(Function, Args, Anno) ->
  FName = cerl:ann_c_fname(Anno, Function, length(Args)),
  cerl:ann_c_apply(Anno, FName, Args).

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

        {Vars, CaseAst} = body_from_clauses(Anno, ClausesAst),
        FunAst = function_form(Name, Anno, Vars, CaseAst),

        clj_module:add_functions(Module, [FunAst]),

        StateAcc2
    end,

  lists:foldl(FunctionFun, State, maps:values(GroupedMethods)).

-spec body_from_clauses(cerl:anno(), [cerl:c_clause()]) ->
  {[cerl:c_var()], cerl:cerl()}.
body_from_clauses(_Anno, [ClauseAst]) ->
  Patterns = cerl:clause_pats(ClauseAst),
  BodyAst  = cerl:clause_body(ClauseAst),
  {Patterns, BodyAst};
body_from_clauses(Anno, ClauseAsts) ->
  case_from_clauses(Anno, ClauseAsts).

-spec case_from_clauses(cerl:anno(), [cerl:c_clause()]) ->
  {[cerl:c_var()], cerl:cerl()}.
case_from_clauses(Anno, [ClauseAst | _] = ClausesAst) ->
  Patterns = cerl:clause_pats(ClauseAst),
  Vars = [new_c_var(Anno) || _ <- lists:seq(0, length(Patterns) - 1)],
  CaseAst = cerl:ann_c_case(Anno, cerl:c_values(Vars), ClausesAst),
  {Vars, CaseAst}.

-spec letrec_defs([ast()], [ast()], state()) ->
  {[{cerl:cerl(), cerl:cerl()}], state()}.
letrec_defs(VarsExprs, FnsExprs, State0) ->
  {VarsAsts, State1} = pop_ast( lists:foldl(fun ast/2, State0, VarsExprs)
                              , length(VarsExprs)
                              ),

  FNamesAsts = [ cerl:ann_c_fname(anno_from(VarEnv), sym_to_kw(VarName), 1)
                 || #{name := VarName, env := VarEnv} <- VarsExprs
               ],

  FoldFun =
    fun(#{op := fn} = FnExpr, StateAcc) ->
        #{ methods := Methods
         , env     := Env
         } = FnExpr,

        Anno = anno_from(Env),

        StateAcc1 = lists:foldl(fun method_to_case_clause/2, StateAcc, Methods),
        {ClausesAsts, StateAcc2} = pop_ast(StateAcc1, length(Methods)),

        ArgsVar  = cerl:ann_c_var(Anno, args),
        CaseAst  = cerl:ann_c_case(Anno, ArgsVar, ClausesAsts),
        LetAst   = cerl:ann_c_let( Anno
                                 , VarsAsts
                                 , cerl:c_values(FNamesAsts)
                                 , CaseAst
                                 ),
        FunAst   = cerl:ann_c_fun(Anno, [ArgsVar], LetAst),

        push_ast(FunAst, StateAcc2)
    end,

  {FnsAsts, State2} = pop_ast( lists:foldl(FoldFun, State1, FnsExprs)
                             , length(FnsExprs)
                             ),

  {lists:zip(FNamesAsts, FnsAsts), State2}.

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
put_lexical_rename(#{shadow := ?NIL}, State) ->
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
do_shadow_depth(#{shadow := Shadowed}, Depth) when Shadowed =/= ?NIL ->
  do_shadow_depth(Shadowed, Depth + 1);
do_shadow_depth(_, Depth) ->
  Depth.

-spec var_val_function(ast(), ast(), erl_anno:anno()) -> ast().
var_val_function(Val, VarAst, Anno) ->
  TestAst            = call_mfa('clojerl.Var', dynamic_binding, [VarAst], Anno),

  NilAtom        = cerl:ann_c_atom(Anno, ?NIL),
  NilClauseAst   = cerl:ann_c_clause(Anno, [NilAtom], Val),

  XVar           = new_c_var(Anno),
  TupleAst       = cerl:ann_c_tuple(Anno, [cerl:c_atom(ok), XVar]),
  ValueClauseAst = cerl:ann_c_clause(Anno, [TupleAst], XVar),

  cerl:ann_c_case(Anno, TestAst, [NilClauseAst, ValueClauseAst]).

-spec file_from(clj_env:env()) -> binary().
file_from(Env) ->
  maps:get(file, clj_env:location(Env), <<>>).

-spec anno_from(clj_env:env()) -> erl_anno:anno().
anno_from(Env) ->
  case clj_env:location(Env) of
    ?NIL -> [0];
    Location  ->
      [ maps:get(line, Location, 0)
      , {file, maps:get(file, Location, "")}
      ]
  end.

-spec sym_to_kw('clojerl.Symbol':type()) -> atom().
sym_to_kw(Symbol) ->
  binary_to_atom(clj_core:str(Symbol), utf8).

-spec default_compiler_options() -> clj_compiler:opts().
default_compiler_options() ->
  #{erl_flags => [binary, debug_info], output_dir => "ebin"}.

-spec new_c_var(cerl:ann()) -> cerl:c_var().
new_c_var(Anno) ->
  N = case erlang:get(local_var_counter) of
        ?NIL -> 0;
        X -> X
      end,
  erlang:put(local_var_counter, N + 1),
  Name = list_to_atom("clj " ++ integer_to_list(N)),
  cerl:ann_c_var([generated | Anno], Name).
