-module(clj_emitter).

-include("clojerl.hrl").
-include("clojerl_int.hrl").
-include("clojerl_expr.hrl").

-export([ emit/1
        , new_c_var/1
        ]).

-type ast()   :: cerl:cerl().

-type state() :: #{ asts                => [ast()]
                  , lexical_renames     => clj_scope:scope()
                  , force_remote_invoke => boolean()
                  }.

-spec emit(clj_env:env()) -> {[ast()], clj_env:env()}.
emit(Env0) ->
  {Expr, Env} = clj_env:pop_expr(Env0),
  State = ast(Expr, initial_state()),
  Asts  = lists:reverse(maps:get(asts, State)),
  {Asts, Env}.

-spec initial_state() -> state().
initial_state() ->
  #{ asts                => []
   , lexical_renames     => clj_scope:new()
   , force_remote_invoke => false
   }.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec ast(expr(), state()) -> state().
ast(#{op := constant, form := Form, env := Env}, State) when is_binary(Form) ->
  ?DEBUG(constant),
  Ast = binary_literal(ann_from(Env), Form),
  push_ast(Ast, State);
ast(#{op := constant, form := Form, env := Env}, State) ->
  ?DEBUG(constant),
  Ast = cerl:ann_abstract(ann_from(Env), Form),
  push_ast(Ast, State);
ast(#{op := quote, expr := Expr}, State) ->
  ?DEBUG(quote),
  ast(Expr, State);
%%------------------------------------------------------------------------------
%% var, binding & local
%%------------------------------------------------------------------------------
ast(#{op := var} = Expr, State) ->
  ?DEBUG(var),
  #{ var  := Var
   , env  := Env
   } = Expr,
  Module = 'clojerl.Var':module(Var),
  Name   = 'clojerl.Var':val_function(Var),
  Ast    = call_mfa(Module, Name, [], ann_from(Env)),

  push_ast(Ast, State);
ast(#{op := binding} = Expr, State) ->
  ?DEBUG(binding),
  #{pattern := PatternExpr} = Expr,
  ast(PatternExpr, State);
ast(#{op := local} = Expr, State) ->
  ?DEBUG(local),
  #{env := Env} = Expr,
  NameBin = get_lexical_rename(Expr, State),
  Ast     = cerl:ann_c_var(ann_from(Env), binary_to_atom(NameBin, utf8)),

  push_ast(Ast, State);
%%------------------------------------------------------------------------------
%% do
%%------------------------------------------------------------------------------
ast(#{op := do} = Expr, State) ->
  ?DEBUG(do),
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
  ?DEBUG(def),
  #{ var  := Var
   , init := InitExpr
   , env  := Env
   } = Expr,
  Module  = 'clojerl.Var':module(Var),
  Name    = 'clojerl.Var':function(Var),
  ValName = 'clojerl.Var':val_function(Var),

  ?DEBUG({def, Module, Name}),

  ok      = clj_module:ensure_loaded(file_from(Env), Module),
  VarAst  = cerl:abstract(Var),
  VarAnn  = ann_from(Env),

  {ValAst0, State1} =
    case InitExpr of
      #{op := fn} = FnExpr ->
        { VarAst
        , add_functions(Module, Name, VarAnn, FnExpr, State)
        };
      _ ->
        {InitAst0, StateTemp} = pop_ast(ast(InitExpr, State)),
        InitAst = case cerl:is_literal(InitAst0) of
                     true  -> InitAst0;
                     false ->
                      Init = clj_compiler:eval_expressions([InitAst0]),
                      ?ERROR_WHEN( not cerl:is_literal_term(Init)
                                 , [ <<"Init value for ">>, Var
                                   , <<" is not a literal: ">>
                                   , Init
                                   ]
                                 , clj_env:location(Env)
                                 ),
                      cerl:abstract(Init)
                  end,
        {InitAst, StateTemp}
    end,

  %% If the var is dynamic then the body of the val function needs
  %% to take this into account.
  ValAst = case 'clojerl.Var':is_dynamic(Var) of
             true  -> var_val_function(ValAst0, VarAst, VarAnn);
             false -> ValAst0
           end,

  ValFunAst = function_form(ValName, VarAnn, [], ValAst),

  clj_module:add_mappings([Var], Module),
  clj_module:add_functions([ValFunAst], Module),
  clj_module:add_exports([{ValName, 0}], Module),

  push_ast(VarAst, State1);
%%------------------------------------------------------------------------------
%% import
%%------------------------------------------------------------------------------
ast(#{op := import} = Expr, State) ->
  ?DEBUG(import),
  #{ typename := TypeName
   , env      := Env
   } = Expr,

  SymName   = lists:last(binary:split(TypeName, <<".">>, [global])),
  Type      = 'erlang.Type':?CONSTRUCTOR(binary_to_atom(TypeName, utf8)),
  CurrentNs = 'clojerl.Namespace':current(),
  NameSym   = 'clojerl.Namespace':name(CurrentNs),
  Module    = to_atom(NameSym),

  ok = clj_module:ensure_loaded(file_from(Env), Module),
  clj_module:add_mappings([{SymName, Type}], Module),

  TypenameAst = cerl:abstract(TypeName),
  Ann         = ann_from(Env),
  ImportAst   = call_mfa('clojerl.Namespace', import_type, [TypenameAst], Ann),

  push_ast(ImportAst, State);
%%------------------------------------------------------------------------------
%% type
%%------------------------------------------------------------------------------
ast(#{op := type} = Expr, State) ->
  ?DEBUG(type),
  #{ type := Type
   , env  := Env
   } = Expr,

  Ast = cerl:ann_abstract(ann_from(Env), Type),

  push_ast(Ast, State);
%%------------------------------------------------------------------------------
%% new
%%------------------------------------------------------------------------------
ast(#{op := new} = Expr, State) ->
  ?DEBUG(new),
  #{ type := TypeExpr
   , args := ArgsExprs
   , env  := Env
   } = Expr,

  {ArgsAsts, State1} = pop_ast( lists:foldl(fun ast/2, State, ArgsExprs)
                              , length(ArgsExprs)
                              ),

  {TypeAst, State2}  = pop_ast(ast(TypeExpr, State1)),
  Type               = cerl:concrete(TypeAst),
  TypeModule         = 'erlang.Type':module(Type),

  Ast = call_mfa(TypeModule, ?CONSTRUCTOR, ArgsAsts, ann_from(Env)),
  push_ast(Ast, State2);
%%------------------------------------------------------------------------------
%% deftype
%%------------------------------------------------------------------------------
ast(#{op := deftype} = Expr, State0) ->
  ?DEBUG(deftype),
  #{ type      := Type
   , name      := Name
   , fields    := FieldsExprs
   , methods   := MethodsExprs
   , protocols := ProtocolsExprs
   , env       := Env
   } = Expr,

  Module = 'erlang.Type':module(Type),
  Ann    = ann_from(Env),
  ok     = clj_module:ensure_loaded(file_from(Env), Module),

  %% Attributes
  Attributes = [ { cerl:ann_c_atom(Ann, behavior)
                 , cerl:ann_abstract(Ann, ['erlang.Type':module(ProtocolType)])
                 }
                 || #{type := ProtocolType} <- ProtocolsExprs
               ],

  %% Functions
  ForceRemote = maps:get(force_remote_invoke, State0),

  {AllFieldsAsts, State} = pop_ast( lists:foldl(fun ast/2, State0, FieldsExprs)
                                   , length(FieldsExprs)
                                   ),
  { MethodsAsts
  , State1
  }  = pop_ast( lists:foldl( fun ast/2
                           , State#{force_remote_invoke => true}
                           , MethodsExprs)
              , length(MethodsExprs)
              ),

  %% Expand the first argument to pattern match on all fields so that they are
  %% available in the functions scope.
  TypeTupleAst = type_map(Module, Ann, AllFieldsAsts, [], match),
  MethodsAsts1 = lists:map( fun(F) ->
                                expand_first_argument(F, TypeTupleAst)
                            end
                          , MethodsAsts
                          ),

  %% Predicate function to check if a field is one of the hidden record fields.
  IsRecordFld = fun(Field) ->
                    FieldName = atom_to_binary(cerl:var_name(Field), utf8),
                    not lists:member(FieldName, hidden_fields())
                  end,
  {FieldsAsts, HiddenFieldsAsts} = lists:partition(IsRecordFld, AllFieldsAsts),

  %% Generate accessor functions for all fields.
  AccessorsAsts = lists:map( fun(F) -> accessor_function(Module, F) end
                           , AllFieldsAsts
                           ),

  %% Create a constructor that doesn't include the hidded fields.
  CtorAst   = constructor_function( Module
                                  , Ann
                                  , AllFieldsAsts
                                  , HiddenFieldsAsts
                                  ),
  %% When there are hidden fields we assume it is a record, so we also want
  %% to create:
  %%   - a constructor function that includes the hidden fields.
  %%   - a create/1 function that takes a map.
  CtorsAsts = case HiddenFieldsAsts of
                [] -> [CtorAst];
                _  -> [ CtorAst
                      , constructor_function(Module, Ann, AllFieldsAsts, [])
                      , creation_function( Module
                                         , Ann
                                         , AllFieldsAsts
                                         , HiddenFieldsAsts
                                         )
                      , get_basis_function( Ann
                                          , FieldsExprs
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
                           , {get_basis, 0}
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

  clj_module:add_attributes(Attributes, Module),
  clj_module:add_exports(Exports, Module),
  clj_module:add_functions(Functions, Module),

  Opts   = clj_env:get(compiler_opts, default_compiler_options(), Env),
  Module = clj_compiler:compile_module(clj_module:get_module(Module), Opts),
  ok     = clj_module:remove(Module),

  Ast = cerl:ann_abstract(ann_from(Env), Name),

  push_ast(Ast, State1#{force_remote_invoke => ForceRemote});
%%------------------------------------------------------------------------------
%% methods
%%------------------------------------------------------------------------------
ast(#{op := fn_method} = Expr, State0) ->
  ?DEBUG(fn_method),
  #{ name := NameSym
   , env  := Env
   } = Expr,

  {ClauseAst, State1} = pop_ast(method_to_function_clause(Expr, State0)),
  Args   = cerl:clause_pats(ClauseAst),
  Body   = cerl:clause_body(ClauseAst),
  FunAst = function_form(to_atom(NameSym), ann_from(Env), Args, Body),

  push_ast(FunAst, State1);
%%------------------------------------------------------------------------------
%% defprotocol
%%------------------------------------------------------------------------------
ast(#{op := defprotocol} = Expr, State) ->
  ?DEBUG(defprotocol),
  #{ name         := NameSym
   , methods_sigs := MethodsSigs
   , env          := Env
   } = Expr,

  Module = to_atom(NameSym),
  ok     = clj_module:ensure_loaded(file_from(Env), Module),
  Ann    = ann_from(Env),

  FunctionFun = fun(Sig) ->
                    MethodSym = clj_rt:first(Sig),
                    Arity     = clj_rt:second(Sig),
                    Method    = to_atom(MethodSym),
                    Vars      = [new_c_var(Ann) || _ <- lists:seq(1, Arity)],
                    Args      = [ cerl:c_atom(Module)
                                , cerl:c_atom(Method)
                                | Vars
                                ],
                    Body      = call_mfa(clj_protocol, resolve, Args, Ann),
                    { cerl:c_fname(Method, Arity)
                    , cerl:ann_c_fun(Ann, Vars, Body)
                    }
                end,

  ProtocolAttr = {cerl:ann_c_atom(Ann, protocol), cerl:abstract([true])},
  Functions    = lists:map(FunctionFun, MethodsSigs),
  Exports      = [ {cerl:fname_id(FName), cerl:fname_arity(FName)}
                   || {FName, _} <- Functions
                 ],

  clj_module:add_attributes([ProtocolAttr], Module),
  clj_module:add_functions(Functions, Module),
  clj_module:add_exports(Exports, Module),

  Opts   = clj_env:get(compiler_opts, default_compiler_options(), Env),
  Module = clj_compiler:compile_module(clj_module:get_module(Module), Opts),
  ok     = clj_module:remove(Module),

  Ast = cerl:ann_abstract(Ann, NameSym),
  push_ast(Ast, State);
%%------------------------------------------------------------------------------
%% extend_type
%%------------------------------------------------------------------------------
ast(#{op := extend_type} = Expr, State) ->
  ?DEBUG(extend_type),
  #{ type  := #{type := Type}
   , impls := Impls
   , env   := Env
   } = Expr,

  EmitProtocolFun =
    fun(#{type := ProtoType} = Proto, StateAcc) ->
        ProtoBin = 'erlang.Type':str(ProtoType),
        TypeBin  = 'erlang.Type':str(Type),
        Module   = clj_protocol:impl_module(ProtoBin, TypeBin),

        clj_module:ensure_loaded(file_from(Env), Module),

        MethodsExprs = maps:get(Proto, Impls),

        %% Functions
        StateAcc1 = lists:foldl(fun ast/2, StateAcc, MethodsExprs),
        {FunctionsAsts, StateAcc2} = pop_ast(StateAcc1, length(MethodsExprs)),

        %% Exports
        Exports = lists:map(fun function_signature/1, FunctionsAsts),

        clj_module:remove_all_functions(Module),
        clj_module:add_exports(Exports, Module),
        clj_module:add_functions(FunctionsAsts, Module),

        Opts   = clj_env:get(compiler_opts, default_compiler_options(), Env),
        Module = clj_compiler:compile_module( clj_module:get_module(Module)
                                            , Opts
                                            ),
        ok     = clj_module:remove(Module),

        StateAcc2
    end,

  ForceRemote = maps:get(force_remote_invoke, State),

  %% We force remote calls for all functions since the function will
  %% live in its own module, but is analyzed in the context of the
  %% surrounding code where the extend-type is used.
  State1 = lists:foldl( EmitProtocolFun
                      , State#{force_remote_invoke => true}
                      , maps:keys(Impls)
                      ),

  Ast = cerl:ann_c_atom(ann_from(Env), ?NIL),
  push_ast(Ast, State1#{force_remote_invoke => ForceRemote});
%%------------------------------------------------------------------------------
%% fn, invoke, erl_fun
%%------------------------------------------------------------------------------
ast(#{op := fn} = Expr, State) ->
  ?DEBUG(fn),
  #{ local   := LocalExpr = #{name := NameSym}
   , env     := Env
   } = Expr,

  Ann = ann_from(Env),

  {DefsAsts, State1} = letrec_defs([LocalExpr], [Expr], State),

  NameAtom = to_atom(NameSym),
  FName    = cerl:ann_c_fname(Ann, NameAtom, 1),
  Ast      = cerl:ann_c_letrec(Ann, DefsAsts, FName),

  push_ast(Ast, State1);
ast(#{op := erl_fun} = Expr, State) ->
  ?DEBUG(erl_fun),
  #{ module   := Module
   , function := Function
   , arity    := Arity
   , env      := Env
   } = Expr,

  ?ERROR_WHEN( Arity == ?NIL
             , [ <<"Can't use an erlang function as a value without ">>
               , <<"specifying its arity: ">>
               , atom_to_binary(Module, utf8)
               , <<"/">>
               , atom_to_binary(Function, utf8)
               ]
             , clj_env:location(Env)
             ),

  clj_utils:check_erl_fun(Expr),

  Ann  = ann_from(Env),
  Ast  = case Module of
           ?NIL ->
             cerl:ann_c_fname(Ann, Function, Arity);
           _ ->
             call_mfa( erlang
                     , make_fun
                     , [ cerl:ann_c_atom(Ann, Module)
                       , cerl:ann_c_atom(Ann, Function)
                       , cerl:ann_c_int(Ann, Arity)
                       ]
                     , Ann
                     )
         end,

  push_ast(Ast, State);
ast(#{op := invoke} = Expr, State) ->
  ?DEBUG(invoke),
  #{ args := ArgsExpr
   , f    := FExpr
   , env  := Env
   } = Expr,

  Ann = ann_from(Env),

  {Args, State1} = pop_ast( lists:foldl(fun ast/2, State, ArgsExpr)
                          , length(ArgsExpr)
                          ),

  case FExpr of
    %% Var
    #{op := var, var := Var, form := Symbol, is_dynamic := false} ->
      Ast = var_invoke(Var, Symbol, Args, Ann, State),

      push_ast(Ast, State1);
    %% Erlang Function
    #{ op       := erl_fun
     , module   := Module
     , function := Function
     , env      := EnvErlFun
     } ->
      clj_utils:check_erl_fun(FExpr),

      AnnErlFun = ann_from(EnvErlFun),
      Ast       = case Module of
                    ?NIL -> call_fa(Function, Args, AnnErlFun);
                    _    -> call_mfa(Module, Function, Args, AnnErlFun)
                  end,

      push_ast(Ast, State);
    %% Resolve Target Type
    #{ op       := resolve_type
     , function := Function
     } ->
      ArgCount   = length(Args),
      TargetAst  = erlang:hd(Args),
      FVarAst    = new_c_var(Ann),
      ModuleAst  = call_mfa(clj_rt, type_module, [TargetAst], Ann),
      ResolveAst = call_mfa( erlang
                           , make_fun
                           , [ ModuleAst
                             , cerl:ann_c_atom(Ann, Function)
                             , cerl:ann_abstract(Ann, ArgCount)
                             ]
                           , Ann
                           ),

      Ast = cerl:ann_c_let( Ann
                          , [FVarAst]
                          , ResolveAst
                          , cerl:ann_c_apply(Ann, FVarAst, Args)
                          ),

      push_ast(Ast, State);
    %% Apply Fn
    _ ->
      {FunAst, State2} = pop_ast(ast(FExpr, State1)),
      ArgsAst = list_ast(Args),
      Ast     = call_mfa('clojerl.IFn', apply, [FunAst, ArgsAst], Ann),

      push_ast(Ast, State2)
  end;
%%------------------------------------------------------------------------------
%% letfn
%%------------------------------------------------------------------------------
ast(#{op := letfn} = Expr, State) ->
  ?DEBUG(letfn),
  #{ vars := VarsExprs
   , fns  := FnsExprs
   , body := BodyExpr
   , env  := Env
   } = Expr,

  Ann = ann_from(Env),

  {DefsAsts, State1} = letrec_defs(VarsExprs, FnsExprs, State),
  {BodyAst,  State2} = pop_ast(ast(BodyExpr, State1)),

  FNamesAsts = [FNameAst || {FNameAst, _} <- DefsAsts],
  VarsAsts   = [cerl:c_var(cerl:fname_id(FNameAst)) || FNameAst <- FNamesAsts],

  LetAst    = cerl:ann_c_let( Ann
                            , VarsAsts
                            , cerl:c_values(FNamesAsts)
                            , BodyAst
                            ),
  LetRecAst = cerl:ann_c_letrec(Ann, DefsAsts, LetAst),

  push_ast(LetRecAst, State2);
%%------------------------------------------------------------------------------
%% with-meta
%%------------------------------------------------------------------------------
ast(#{op := with_meta} = WithMetaExpr, State) ->
  ?DEBUG(with_meta),
  #{ meta := Meta
   , expr := Expr
   , env  := Env
   } = WithMetaExpr,

  {MetaAst, State1} = pop_ast(ast(Meta, State)),
  {ExprAst, State2} = pop_ast(ast(Expr, State1)),

  Ast = call_mfa(clj_rt, with_meta, [ExprAst, MetaAst], ann_from(Env)),

  push_ast(Ast, State2);
%%------------------------------------------------------------------------------
%% Literal data structures
%%------------------------------------------------------------------------------
ast(#{op := vector} = Expr, State) ->
  ?DEBUG(vector),
  #{ items := ItemsExprs
   , env   := Env
   } = Expr,

  {Items, State1} = pop_ast( lists:foldl(fun ast/2, State, ItemsExprs)
                           , length(ItemsExprs)
                           ),
  ListItems = list_ast(Items),

  Ast = call_mfa('clojerl.Vector', ?CONSTRUCTOR, [ListItems], ann_from(Env)),
  push_ast(Ast, State1);
ast(#{op := map} = Expr, State) ->
  ?DEBUG(map),
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

  Ast = call_mfa('clojerl.Map', ?CONSTRUCTOR, [ListItems], ann_from(Env)),
  push_ast(Ast, State2);
ast(#{op := erl_map} = Expr, State) ->
  ?DEBUG(erl_map),
  #{ keys := KeysExprs
   , vals := ValsExprs
   , env  := Env
   } = Expr,

  MapPairFun = case maps:get(pattern, Expr, false) of
                 true  -> fun c_map_pair_exact/2;
                 false -> fun cerl:c_map_pair/2
               end,

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
               PairUp(Tail1, Tail2, [MapPairFun(H1, H2) | Pairs])
           end,

  PairsAsts = PairUp(Keys, Vals, []),

  Ast = cerl:ann_c_map(ann_from(Env), PairsAsts),
  push_ast(Ast, State2);
ast(#{op := set} = Expr, State) ->
  ?DEBUG(set),
  #{ items := ItemsExprs
   , env   := Env
   } = Expr,

  {Items, State1} = pop_ast( lists:foldl(fun ast/2, State, ItemsExprs)
                           , length(ItemsExprs)
                           ),
  ListItems = list_ast(Items),

  Ast = call_mfa('clojerl.Set', ?CONSTRUCTOR, [ListItems], ann_from(Env)),
  push_ast(Ast, State1);
ast(#{op := tuple} = Expr, State) ->
  ?DEBUG(tuple),
  #{ items := ItemsExprs
   , env   := Env
   } = Expr,

  {Items, State1} = pop_ast( lists:foldl(fun ast/2, State, ItemsExprs)
                           , length(ItemsExprs)
                           ),

  Ast = cerl:ann_c_tuple(ann_from(Env), Items),
  push_ast(Ast, State1);
ast(#{op := erl_list} = Expr, State) ->
  ?DEBUG(erl_list),
  #{ env   := Env
   , items := ItemsExprs
   , tail  := TailExpr
   } = Expr,

  {Items, State1} = pop_ast( lists:foldl(fun ast/2, State, ItemsExprs)
                           , length(ItemsExprs)
                           ),
  {Tail, State2} = case TailExpr of
                     undefined -> {none, State1};
                     _ -> pop_ast(ast(TailExpr, State1))
                   end,

  Ast = cerl:ann_make_list(ann_from(Env), Items, Tail),
  push_ast(Ast, State2);
ast(#{op := erl_alias} = Expr, State0) ->
  ?DEBUG(erl_alias),
  #{ env      := Env
   , variable := VariableExpr
   , pattern  := PatternExpr
   } = Expr,

  Ann = ann_from(Env),

  {VariableAst, State1} = pop_ast(ast(VariableExpr, State0)),
  {PatternAst, State2}  = pop_ast(ast(PatternExpr, State1)),

  Ast = cerl:ann_c_alias(Ann, VariableAst, PatternAst),
  push_ast(Ast, State2);
%%------------------------------------------------------------------------------
%% if
%%------------------------------------------------------------------------------
ast(#{op := 'if'} = Expr, State) ->
  ?DEBUG('if'),
  #{ test := TestExpr
   , then := ThenExpr
   , else := ElseExpr
   , env  := Env
   } = Expr,

  Ann = ann_from(Env),

  %% Test
  {TestAst, State1} = pop_ast(ast(TestExpr, State)),

  %% Then
  False      = cerl:ann_c_atom(Ann, false),
  Nil        = cerl:ann_c_atom(Ann, ?NIL),
  ThenVar    = new_c_var(Ann),
  ThenGuard  = call_mfa( erlang
                           , 'and'
                           , [ call_mfa(erlang, '=/=', [ThenVar, False], Ann)
                             , call_mfa(erlang, '=/=', [ThenVar, Nil], Ann)
                             ]
                           , Ann
                           ),
  {ThenAst, State2} = pop_ast(ast(ThenExpr, State1)),
  ThenClause = cerl:ann_c_clause(Ann, [ThenVar], ThenGuard, ThenAst),

  %% Else
  ElseVar    = new_c_var(Ann),
  {ElseAst, State3} = pop_ast(ast(ElseExpr, State2)),
  ElseClause = cerl:ann_c_clause(Ann, [ElseVar], ElseAst),

  Ast = cerl:ann_c_case(Ann, TestAst, [ThenClause, ElseClause]),
  push_ast(Ast, State3);
%%------------------------------------------------------------------------------
%% case
%%------------------------------------------------------------------------------
ast(#{op := 'case'} = Expr, State) ->
  ?DEBUG('case'),
  #{ test    := TestExpr
   , clauses := ClausesExprs
   , default := DefaultExpr
   , env     := Env
   } = Expr,

  {TestAst, State1} = pop_ast(ast(TestExpr, State)),

  State2 = lists:foldl(fun clause/2, State1, ClausesExprs),
  {ClausesAsts0, State3} = pop_ast(State2, length(ClausesExprs), false),

  {ClausesAsts1, State4} =
    case DefaultExpr of
      ?NIL -> {ClausesAsts0, State3};
      _ ->
        EnvDefault    = maps:get(env, DefaultExpr),
        AnnDefault    = ann_from(EnvDefault),
        DefaultVarAst = new_c_var(AnnDefault),
        {DefaultAst, State3_1} = pop_ast(ast(DefaultExpr, State3)),
        DefaultClause = cerl:ann_c_clause( AnnDefault
                                         , [DefaultVarAst]
                                         , DefaultAst
                                         ),
        {[DefaultClause | ClausesAsts0], State3_1}
    end,

  Ann = ann_from(Env),
  ClausesAsts2 = [fail_clause(case_clause, Ann) | ClausesAsts1],
  ClausesAsts3 = lists:reverse(ClausesAsts2),

  CaseAst = cerl:ann_c_case(ann_from(Env), TestAst, ClausesAsts3),

  push_ast(CaseAst, State4);
%%------------------------------------------------------------------------------
%% let
%%------------------------------------------------------------------------------
ast(#{op := Op} = Expr, State0) when Op =:= 'let'; Op =:= loop ->
  ?DEBUG(Op),
  #{ body     := BodyExpr
   , bindings := BindingsExprs
   , env      := Env
   } = Expr,

  Ann = ann_from(Env),

  State1 = add_lexical_renames_scope(State0),
  State2 = lists:foldl(fun put_lexical_rename/2, State1, BindingsExprs),

  MatchAstFun =
    fun (BindingExpr, {Bindings, StateAcc}) ->
        #{init := InitExpr, env := BindingEnv} = BindingExpr,
        AnnBinding           = ann_from(BindingEnv),
        {Pattern, StateAcc1} = pop_ast(ast(BindingExpr, StateAcc)),
        {Init, StateAcc2}    = pop_ast(ast(InitExpr, StateAcc1)),
        Binding              = {cerl:type(Pattern), Pattern, Init, AnnBinding},
        {[Binding | Bindings], StateAcc2}
    end,

  {Bindings, State3} = lists:foldl(MatchAstFun, {[], State2}, BindingsExprs),
  {Body, State4}     = pop_ast(ast(BodyExpr, State3)),

  FoldFun =
    fun
      ({var, Var, Init, AnnBinding}, BodyAcc) ->
        cerl:ann_c_let(AnnBinding, [Var], Init, BodyAcc);
      ({_, Pattern, Init, AnnBinding}, BodyAcc) ->
        {PatArgs, PatGuards} = clj_emitter_pattern:pattern_list([Pattern]),
        Guard       = clj_emitter_pattern:fold_guards(PatGuards),
        ClauseAst   = cerl:ann_c_clause(AnnBinding, PatArgs, Guard, BodyAcc),
        BadmatchAst = fail_clause(badmatch, AnnBinding),
        cerl:ann_c_case(Ann, Init, [ClauseAst, BadmatchAst])
    end,

  Ast = case Op of
          'let' ->
            lists:foldl(FoldFun, Body, Bindings);
          loop ->
            Patterns   = [P || {_, P, _, _} <- lists:reverse(Bindings)],

            LoopId     = maps:get(loop_id, Expr),
            LoopIdAtom = to_atom(LoopId),

            FNameAst   = cerl:ann_c_fname(Ann, LoopIdAtom, length(Patterns)),
            ClauseAst  = cerl:ann_c_clause(Ann, Patterns, Body),
            {Vars, CaseAst} = case_from_clauses(Ann, [ClauseAst]),
            FunAst     = cerl:ann_c_fun(Ann, Vars, CaseAst),
            Defs       = [{FNameAst, FunAst}],
            ApplyAst   = cerl:ann_c_apply([local | Ann], FNameAst, Patterns),
            LetRecAst  = cerl:ann_c_letrec(Ann, Defs, ApplyAst),

            lists:foldl(FoldFun, LetRecAst, Bindings)
        end,

  State5 = remove_lexical_renames_scope(State4),

  push_ast(Ast, State5);
%%------------------------------------------------------------------------------
%% recur
%%------------------------------------------------------------------------------
ast(#{op := recur} = Expr, State) ->
  ?DEBUG(recur),
  #{ loop_id   := LoopId
   , loop_type := LoopType
   , exprs     := ArgsExprs
   , env       := Env
   } = Expr,

  Ann            = ann_from(Env),
  {Args, State1} = pop_ast( lists:foldl(fun ast/2, State, ArgsExprs)
                          , length(ArgsExprs)
                          ),
  LoopIdAtom     = to_atom(LoopId),

  %% We need to use invoke so that recur also works inside functions
  %% (i.e not funs)
  Ast = case LoopType of
          fn ->
            NameAst = cerl:ann_c_var(Ann, LoopIdAtom),
            ArgsAst = list_ast(Args),
            call_mfa('clojerl.IFn', apply, [NameAst, ArgsAst], Ann);
          _LoopType -> %% fn_method | loop | var
            NameAst = cerl:ann_c_fname(Ann, LoopIdAtom, length(Args)),
            cerl:ann_c_apply([local | Ann], NameAst, Args)
        end,
  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% throw
%%------------------------------------------------------------------------------
ast(#{op := throw} = Expr, State) ->
  ?DEBUG(throw),
  #{ exception := ExceptionExpr
   , env       := Env
   } = Expr,

  {Exception, State1} = pop_ast(ast(ExceptionExpr, State)),

  Ann  = ann_from(Env),
  Ast  = call_mfa(erlang, error, [Exception], Ann),
  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% try
%%------------------------------------------------------------------------------
ast(#{op := 'try'} = Expr, State) ->
  ?DEBUG('try'),
  #{ body    := BodyExpr
   , catches := CatchesExprs
   , finally := FinallyExpr
   , env     := Env
   } = Expr,

  Ann = ann_from(Env),

  {BodyAst, State1} = pop_ast(ast(BodyExpr, State)),
  {Catches, State1} = pop_ast( lists:foldl(fun ast/2, State, CatchesExprs)
                             , length(CatchesExprs)
                             , false
                             ),

  [_, Y, Z] = CatchVarsAsts = [ new_c_var(Ann)
                              , new_c_var(Ann)
                              , new_c_var(Ann)
                              ],
  RaiseAst          = cerl:c_primop(cerl:c_atom(raise), [Z, Y]),

  CatchAllClause    = cerl:ann_c_clause(Ann, CatchVarsAsts, RaiseAst),
  { ClausesVars
  , CaseAst
  } = case_from_clauses(Ann, lists:reverse([CatchAllClause | Catches])),

  {Finally, State2} = case FinallyExpr of
                        ?NIL -> {?NIL, State1};
                        _         -> pop_ast(ast(FinallyExpr, State))
                      end,

  VarAst = new_c_var(Ann),
  TryAst = cerl:ann_c_try( Ann
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
        FinallyName     = cerl:var_name(new_c_var(Ann)),
        FinallyFName    = cerl:ann_c_fname(Ann, FinallyName, 0),
        FinallyFunAst   = cerl:ann_c_fun(Ann, [], Finally),
        ApplyFinallyAst = cerl:ann_c_apply([local | Ann], FinallyFName, []),

        Defs = [{FinallyFName, FinallyFunAst}],
        OuterVarAst  = new_c_var(Ann),
        OuterBodyAst = cerl:ann_c_seq(Ann, ApplyFinallyAst, OuterVarAst),
        HandlerAst   = cerl:ann_c_seq(Ann, ApplyFinallyAst, RaiseAst),
        OuterTryAst  = cerl:ann_c_try( Ann
                                     , TryAst
                                     , [OuterVarAst]
                                     , OuterBodyAst
                                     , CatchVarsAsts
                                     , HandlerAst
                                     ),
        cerl:ann_c_letrec(Ann, Defs, OuterTryAst)
    end,

  push_ast(Ast, State2);
%%------------------------------------------------------------------------------
%% catch
%%------------------------------------------------------------------------------
ast(#{op := 'catch'} = Expr, State0) ->
  ?DEBUG('catch'),
  #{ class := ErrTypeExpr
   , local := PatternExpr
   , body  := BodyExpr
   , guard := GuardExpr
   , env   := Env
   } = Expr,

  Ann                   = ann_from(Env),
  {ClassAst, State1}    = case maps:get(op, ErrTypeExpr) of
                            binding -> pop_ast(ast(ErrTypeExpr, State0));
                            type    ->
                              ErrorTypeAst = cerl:ann_abstract(Ann, error),
                              {ErrorTypeAst, State0}
                          end,
  {PatternAst0, State2} = pop_ast(ast(PatternExpr, State1)),
  {Guard0, State3}      = pop_ast(ast(GuardExpr, State2)),
  {Body, State4}        = pop_ast(ast(BodyExpr, State3)),

  {[PatternAst1], PatGuards} = clj_emitter_pattern:pattern_list([PatternAst0]),
  VarsAsts = [ClassAst, PatternAst1, new_c_var(Ann)],
  Guard1   = clj_emitter_pattern:fold_guards(Guard0, PatGuards),

  Ast      = cerl:ann_c_clause(Ann, VarsAsts, Guard1, Body),
  push_ast(Ast, State4);
%%------------------------------------------------------------------------------
%% receive
%%------------------------------------------------------------------------------
ast(#{op := 'receive'} = Expr, State0) ->
  ?DEBUG('receive'),
  #{ env     := Env
   , clauses := ClausesExprs
   , 'after' := AfterExpr
   } = Expr,

  Ann    = ann_from(Env),
  State1 = lists:foldl(fun clause/2, State0, ClausesExprs),
  {ClausesAsts, State2} = pop_ast(State1, length(ClausesExprs)),

  case AfterExpr of
    ?NIL ->
      Ast = cerl:ann_c_receive(Ann, ClausesAsts),
      push_ast(Ast, State2);
    #{ op      := 'after'
     , timeout := TimeoutExpr
     , body    := BodyExpr
     } ->
      {TimeoutAst, State3} = pop_ast(ast(TimeoutExpr, State2)),
      {BodyAst, State4}    = pop_ast(ast(BodyExpr, State3)),
      Ast = cerl:ann_c_receive(Ann, ClausesAsts, TimeoutAst, BodyAst),
      push_ast(Ast, State4)
  end;
%%------------------------------------------------------------------------------
%% on_load
%%------------------------------------------------------------------------------
ast(#{op := on_load} = Expr, State) ->
  ?DEBUG(on_load),
  #{ body := BodyExpr
   , env  := Env
   } = Expr,

  {Ast, State1} = pop_ast(ast(BodyExpr, State)),

  CurrentNs  = 'clojerl.Namespace':current(),
  NameSym    = 'clojerl.Namespace':name(CurrentNs),
  ModuleName = to_atom(NameSym),
  ok         = clj_module:ensure_loaded(file_from(Env), ModuleName),
  clj_module:add_on_load(Ast, ModuleName),

  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% Erlang binary
%%------------------------------------------------------------------------------
ast(#{op := erl_binary} = Expr, State) ->
  ?DEBUG(erl_binary),
  #{ env      := Env
   , segments := SegmentsExprs
   } = Expr,

  Ann    = ann_from(Env),
  State1 = lists:foldl(fun ast/2, State, SegmentsExprs),
  {SegmentsAsts, State2} = pop_ast(State1, length(SegmentsExprs)),
  Ast    = cerl:ann_c_binary(Ann, SegmentsAsts),

  push_ast(Ast, State2);
ast(#{op := binary_segment} = Expr, State0) ->
  ?DEBUG(binary_segment),
  #{ env   := Env
   , value := ValueExpr
   , size  := SizeExpr
   , unit  := UnitExpr
   , type  := TypeExpr
   , flags := FlagsExpr
   } = Expr,

  {ValueAst, State1} = pop_ast(ast(ValueExpr, State0)),
  {TypeAst, State2}  = pop_ast(ast(TypeExpr, State1)),
  {SizeAst, State3}  = pop_ast(ast(SizeExpr, State2)),
  {UnitAst, State4}  = pop_ast(ast(UnitExpr, State3)),
  {FlagsAst, State5} = pop_ast(ast(FlagsExpr, State4)),

  Ann = ann_from(Env),
  Ast = cerl:ann_c_bitstr( Ann
                         , ValueAst
                         , SizeAst
                         , UnitAst
                         , TypeAst
                         , FlagsAst
                         ),

  push_ast(Ast, State5);
%%------------------------------------------------------------------------------
%% Unknown op
%%------------------------------------------------------------------------------
ast(#{op := Unknown}, _State) ->
  error({unknown_op, Unknown}).

%%------------------------------------------------------------------------------
%% AST Helper Functions
%%------------------------------------------------------------------------------

-spec list_ast(list()) -> ast().
list_ast(List) ->
  list_ast(List, false).

-spec list_ast(list(), boolean()) -> ast().
list_ast(?NIL, _Pattern) ->
  cerl:c_nil();
list_ast([], _Pattern) ->
  cerl:c_nil();
list_ast(List, Pattern) when is_list(List) ->
  list_ast(List, cerl:c_nil(), Pattern).

-spec list_ast(list(), any(), boolean()) -> ast().
list_ast(Heads, Tail, true) when is_list(Heads) ->
  do_list_ast(Heads, Tail);
list_ast(Heads, Tail, false) when is_list(Heads) ->
  do_list_let_ast(Heads, Tail, []).

-spec do_list_let_ast(list(), ast(), [ast()]) -> ast().
do_list_let_ast([], Tail, Vars) ->
  do_list_ast(lists:reverse(Vars), Tail);
do_list_let_ast([H | Hs], Tail, Vars) ->
  Var  = new_c_var([]),
  Body = do_list_let_ast(Hs, Tail, [Var | Vars]),
  cerl:c_let([Var], H, Body).

-spec do_list_ast(list(), ast()) -> ast().
do_list_ast([], Tail) ->
  Tail;
do_list_ast([H | Hs], Tail) ->
  cerl:c_cons(H, do_list_ast(Hs, Tail)).

%% ----- deftype -------

-spec hidden_fields() -> [binary()].
hidden_fields() ->
  [<<"__extmap">>, <<"__meta">>].

%% @doc Replaces the first argument of a function with a pattern match.
%%
%% This function is used for deftype methods so that the fields are available
%% in the scope of the body of each method.
-spec expand_first_argument({ast(), ast()}, ast()) -> {ast(), ast()}.
expand_first_argument( {FNameAst, FunAst}
                     , TypeTupleAst
                     ) ->
  Vars = cerl:fun_vars(FunAst),
  Body = cerl:fun_body(FunAst),
  Ann  = cerl:get_ann(FunAst),
  [_ | VarsTail] = Vars,
  ClauseAst = cerl:c_clause([TypeTupleAst | VarsTail], Body),
  CaseAst   = cerl:ann_c_case(Ann, cerl:c_values(Vars), [ClauseAst]),

  { FNameAst
  , cerl:ann_c_fun(Ann, Vars, CaseAst)
  }.

%% @doc Builds a constructor function for the specified type.
%%
%% The constructor will take AllFields -- HiddenFields as arguments.
%% Hidden fields will be assigned the value `?NIL'.
-spec constructor_function(atom(), [term()], [ast()], [ast()]) ->
  {ast(), ast()}.
constructor_function(Typename, Ann, AllFieldsAsts, HiddenFieldsAsts) ->
  FieldsAsts  = AllFieldsAsts -- HiddenFieldsAsts,
  TupleAst    = type_map( Typename
                        , Ann
                        , AllFieldsAsts
                        , HiddenFieldsAsts
                        , create
                        ),
  function_form(?CONSTRUCTOR, Ann, FieldsAsts, TupleAst).

%% @doc Builds an accessor function for the specified type and field.
%%
%% The accessor function generated has a '-' as a prefix.
-spec accessor_function(atom(), ast()) -> {ast(), ast()}.
accessor_function(Typename, FieldAst) ->
  Ann             = cerl:get_ann(FieldAst),
  FieldName       = cerl:var_name(FieldAst),
  TupleAst        = type_map(Typename, Ann, [FieldAst], [], match),
  AccessorName    = field_fun_name(FieldName),
  ClauseAst       = cerl:ann_c_clause(Ann, [TupleAst], FieldAst),
  {Vars, CaseAst} = case_from_clauses(Ann, [ClauseAst]),
  function_form(AccessorName, Ann, Vars, CaseAst).

-spec creation_function(atom(), [term()], [ast()], [ast()]) -> {ast(), ast()}.
creation_function(Typename, Ann, AllFieldsAsts, HiddenFieldsAsts) ->
  MapVarAst     = new_c_var(Ann),
  GetAstFun     = fun(FName) ->
                      ArgsAst = [MapVarAst, cerl:ann_c_atom(Ann, FName)],
                      call_mfa(clj_rt, get, ArgsAst, Ann)
                  end,

  DissocFoldFun = fun(FieldAst, MapAst) ->
                      FName = cerl:var_name(FieldAst),
                      case lists:member(FieldAst, HiddenFieldsAsts) of
                        true  -> MapAst;
                        false ->
                          FAtom = cerl:ann_c_atom(Ann, FName),
                          call_mfa(clj_rt, dissoc, [MapAst, FAtom], Ann)
                      end
                  end,

  %% Coerce argument into a clojerl.Map
  EmptyMapAst   = cerl:abstract('clojerl.Map':?CONSTRUCTOR([])),
  ArgsListAst   = list_ast([EmptyMapAst, MapVarAst], true),
  MergeCallAst  = call_mfa(clj_rt, merge, [ArgsListAst], Ann),

  ExtMapAst     = lists:foldl(DissocFoldFun, MergeCallAst, AllFieldsAsts),
  NilOrExtMapAst= call_mfa(clj_rt, seq_or_else, [ExtMapAst], Ann),

  AssocAtom     = cerl:c_atom(assoc),
  NilAtom       = cerl:c_atom(?NIL),
  MapPairsFun   = fun(FieldAst) ->
                      FieldName = cerl:var_name(FieldAst),
                      Value = case lists:member(FieldAst, HiddenFieldsAsts) of
                                true when FieldName =:= '__extmap' ->
                                  NilOrExtMapAst;
                                true ->
                                  NilAtom;
                                false ->
                                  GetAstFun(FieldName)
                              end,
                      cerl:ann_c_map_pair( Ann
                                         , AssocAtom
                                         , cerl:ann_c_atom(Ann, FieldName)
                                         , Value
                                         )
                  end,
  MapPairs      = lists:map(MapPairsFun, AllFieldsAsts),
  MapAst        = type_map_ast(Typename, MapPairs, create),

  function_form(create, Ann, [MapVarAst], MapAst).

-spec get_basis_function([any()], [expr()]) -> {ast(), ast()}.
get_basis_function(Ann, FieldsExprs) ->
  FilterMapFun   = fun(#{pattern := #{name := NameSym}}) ->
                       NameBin = 'clojerl.Symbol':name(NameSym),
                       case lists:member(NameBin, hidden_fields()) of
                         true  -> false;
                         false -> {true, cerl:ann_abstract(Ann, NameSym)}
                       end
                   end,
  FieldNamesAsts = lists:filtermap(FilterMapFun, FieldsExprs),
  ListAst        = list_ast(FieldNamesAsts),
  VectorAst      = call_mfa('clojerl.Vector', ?CONSTRUCTOR, [ListAst], Ann),

  function_form(get_basis, Ann, [], VectorAst).

-spec field_fun_name(atom()) -> atom().
field_fun_name(FieldName) when is_atom(FieldName) ->
  list_to_atom("-" ++ atom_to_list(FieldName)).

%% @doc Builds a map's ast tagged with ?TYPE which is used
%%      to build Clojerl data types.
-spec type_map(atom(), [ast()], [ast()], [ast()], create | match) ->
  ast().
type_map(Typename, Ann, AllFieldsAsts, HiddenFieldsAsts, MapType) ->
  MapFieldType = case MapType of
                   create -> cerl:c_atom(assoc);
                   match  -> cerl:c_atom(exact)
                 end,

  NilAtom       = cerl:c_atom(?NIL),
  MapPairFun    = fun(FieldAst) ->
                      FieldName = cerl:var_name(FieldAst),
                      Value = case lists:member(FieldAst, HiddenFieldsAsts) of
                                true when MapType =:= create ->
                                  NilAtom;
                                _ ->
                                  FieldAst
                              end,
                      cerl:ann_c_map_pair( Ann
                                         , MapFieldType
                                         , cerl:c_atom(FieldName)
                                         , Value
                                         )
                  end,
  MapPairsAsts  = lists:map(MapPairFun, AllFieldsAsts),
  type_map_ast(Typename, MapPairsAsts, MapType).

-spec type_map_ast(atom(), [ast()], create | match) -> ast().
type_map_ast(Typename, MapPairsAsts, MapType) ->
  MakeMap  = case MapType of
               create -> fun cerl:c_map/1;
               match  -> fun cerl:c_map_pattern/1
             end,
  MakePair = case MapType of
               create -> fun cerl:c_map_pair/2;
               match  -> fun c_map_pair_exact/2
             end,
  TypeMapPairAst = MakePair(cerl:c_atom(?TYPE), cerl:c_atom(Typename)),
  MakeMap([TypeMapPairAst | MapPairsAsts]).

%% ----- Case and receive Clauses -------

-spec fail_clause(atom(), [any()]) -> ast().
fail_clause(Reason, Ann) ->
  VarAst      = new_c_var(Ann),
  BadmatchAst = cerl:c_tuple([cerl:c_atom(Reason), VarAst]),
  FailAst     = cerl:ann_c_primop(Ann, cerl:c_atom(match_fail), [BadmatchAst]),
  cerl:ann_c_clause([compiler_generated | Ann], [VarAst], FailAst).

-spec clause({expr(), expr()}, state()) -> state().
clause({PatternExpr, BodyExpr}, StateAcc) ->
  #{ env   := EnvPattern
   , guard := GuardExpr
   } = PatternExpr,

  AnnPattern  = ann_from(EnvPattern),

  {Pattern, StateAcc1} = pop_ast(ast(PatternExpr, StateAcc)),
  {Guard, StateAcc2}   = pop_ast(ast(GuardExpr, StateAcc1)),
  {Body, StateAcc3}    = pop_ast(ast(BodyExpr, StateAcc2)),

  ClauseAst = cerl:ann_c_clause(AnnPattern, [Pattern], Guard, Body),
  push_ast(ClauseAst, StateAcc3).

%% ----- Functions -------

-spec function_form(atom(), [term()], [ast()], ast()) ->
  {ast(), ast()}.
function_form(Name, Ann, Args, Body) when is_atom(Name) ->
  EvalName = cerl:c_fname(Name, length(Args)),
  EvalFun  = cerl:ann_c_fun(Ann, Args, Body),
  {EvalName, EvalFun}.

-spec function_signature({ast(), ast()}) -> {atom(), arity()}.
function_signature({FName, _}) ->
  {cerl:fname_id(FName), cerl:fname_arity(FName)}.

%% ----- Methods -------

-spec method_to_function_clause(clj_analyzer:expr(), state()) -> state().
method_to_function_clause(MethodExpr, State) ->
  method_to_clause(MethodExpr, State, function).

-spec method_to_case_clause(clj_analyzer:expr(), state()) -> state().
method_to_case_clause(MethodExpr, State) ->
  method_to_clause(MethodExpr, State, 'case').

-spec method_to_clause(clj_analyzer:expr(), state(), function | 'case') ->
  state().
method_to_clause(MethodExpr, State0, ClauseFor) ->
  #{ op     := fn_method
   , params := ParamsExprs
   , body   := BodyExpr
   , guard  := GuardExpr
   , env    := Env
   } = MethodExpr,

  %% Get this value this way since it might not be there
  IsVariadic = maps:get('variadic?', MethodExpr, false),

  State00 = add_lexical_renames_scope(State0),
  State = lists:foldl(fun put_lexical_rename/2, State00, ParamsExprs),

  {Args, State1} = pop_ast( lists:foldl(fun ast/2, State, ParamsExprs)
                          , length(ParamsExprs)
                          ),

  {PatternArgs, PatternGuards} = clj_emitter_pattern:pattern_list(Args),

  {BodyAst, State2} = pop_ast(ast(BodyExpr, State1)),

  ParamCount = length(ParamsExprs),
  Args1 = case ClauseFor of
            function ->
              PatternArgs;
            'case' when IsVariadic, ParamCount == 1 ->
              PatternArgs;
            'case' when IsVariadic ->
              [list_ast( lists:droplast(PatternArgs), lists:last(PatternArgs)
                       , true
                       )
              ];
            'case' ->
              [list_ast(PatternArgs, true)]
          end,

  {Guard0, State3} = pop_ast(ast(GuardExpr, State2)),

  Guard1 = clj_emitter_pattern:fold_guards(Guard0, PatternGuards),

  Clause = cerl:ann_c_clause(ann_from(Env)
                            , Args1
                            , Guard1
                            , BodyAst
                            ),

  State4 = remove_lexical_renames_scope(State3),

  push_ast(Clause, State4).

-spec call_mfa(module(), atom(), list(), [term()]) -> ast().
call_mfa(Module, Function, Args, Ann) ->
  cerl:ann_c_call(Ann, cerl:c_atom(Module), cerl:c_atom(Function), Args).

-spec call_fa(atom(), list(), [term()]) -> ast().
call_fa(Function, Args, Ann) ->
  FName = cerl:ann_c_fname(Ann, Function, length(Args)),
  cerl:ann_c_apply(Ann, FName, Args).

-spec group_methods([expr()]) -> #{integer() => [expr()]}.
group_methods(Methods) ->
  ParamCountFun = fun(#{params := Params}) -> length(Params) end,
  clj_utils:group_by(ParamCountFun, Methods).

-spec add_functions(module(), atom(), [term()], expr(), state()) ->
  state().
add_functions(Module, Name, Ann, #{op := fn, methods := Methods}, State) ->
  GroupedMethods = group_methods(Methods),

  ExportFun = fun(Arity) ->
                  clj_module:add_exports([{Name, Arity}], Module)
              end,

  lists:foreach(ExportFun, maps:keys(GroupedMethods)),

  FunctionFun =
    fun(MethodsList, StateAcc) ->
        StateAcc1 = lists:foldl( fun method_to_function_clause/2
                               , StateAcc
                               , MethodsList
                               ),
        {ClausesAst, StateAcc2} = pop_ast(StateAcc1, length(MethodsList)),

        {Vars, CaseAst} = body_from_clauses(Ann, ClausesAst),
        FunAst = function_form(Name, Ann, Vars, CaseAst),

        clj_module:add_functions([FunAst], Module),

        StateAcc2
    end,

  lists:foldl(FunctionFun, State, maps:values(GroupedMethods)).

-spec body_from_clauses([term()], [cerl:c_clause()]) ->
  {[cerl:c_var()], ast()}.
body_from_clauses(Ann, [ClauseAst]) ->
  case cerl:clause_guard(ClauseAst) =:= cerl:c_atom(true) of
    true  ->
      Patterns = cerl:clause_pats(ClauseAst),
      BodyAst  = cerl:clause_body(ClauseAst),
      {Patterns, BodyAst};
    false ->
      case_from_clauses(Ann, [ClauseAst])
  end;
body_from_clauses(Ann, ClauseAsts) ->
  case_from_clauses(Ann, ClauseAsts).

-spec case_from_clauses([term()], [cerl:c_clause()]) ->
  {[cerl:c_var()], ast()}.
case_from_clauses(Ann, [ClauseAst | _] = ClausesAst) ->
  Patterns = cerl:clause_pats(ClauseAst),
  Vars = [new_c_var(Ann) || _ <- lists:seq(0, length(Patterns) - 1)],
  CaseAst = cerl:ann_c_case(Ann, cerl:c_values(Vars), ClausesAst),
  {Vars, CaseAst}.

%% ----- letrec -------

-spec letrec_defs([expr()], [expr()], state()) ->
  {[{ast(), ast()}], state()}.
letrec_defs(VarsExprs, FnsExprs, State0) ->
  {VarsAsts, State1} = pop_ast( lists:foldl(fun ast/2, State0, VarsExprs)
                              , length(VarsExprs)
                              ),

  FNamesAsts = [ cerl:ann_c_fname(ann_from(VarEnv), to_atom(VarName), 1)
                 || #{name := VarName, env := VarEnv} <- VarsExprs
               ],

  FoldFun =
    fun(#{op := fn} = FnExpr, StateAcc) ->
        #{ methods := Methods
         , env     := Env
         } = FnExpr,

        Ann = ann_from(Env),

        StateAcc1 = lists:foldl(fun method_to_case_clause/2, StateAcc, Methods),
        {ClausesAsts, StateAcc2} = pop_ast(StateAcc1, length(Methods)),

        ArgsVar  = cerl:ann_c_var(Ann, args),
        CaseAst  = cerl:ann_c_case(Ann, ArgsVar, ClausesAsts),
        LetAst   = cerl:ann_c_let( Ann
                                 , VarsAsts
                                 , cerl:c_values(FNamesAsts)
                                 , CaseAst
                                 ),
        FunAst   = cerl:ann_c_fun(Ann, [ArgsVar], LetAst),

        push_ast(FunAst, StateAcc2)
    end,

  {FnsAsts, State2} = pop_ast( lists:foldl(FoldFun, State1, FnsExprs)
                             , length(FnsExprs)
                             ),

  {lists:zip(FNamesAsts, FnsAsts), State2}.

%% ----- Binary literal -------

-spec binary_literal(any(), binary()) -> ast().
binary_literal(Ann, Binary) ->
  Size     = cerl:abstract(8),
  Type     = cerl:abstract(integer),
  Flags     = cerl:abstract([unsigned, big]),
  Segments = [cerl:ann_c_bitstr(Ann, cerl:abstract(B), Size, Type, Flags)
              || B <- binary_to_list(Binary)],
  cerl:ann_c_binary(Ann, Segments).

%% ----- Push & pop asts -------

-spec push_ast(ast() | {ast(), ast()}, state()) -> state().
push_ast(Ast, State = #{asts := Asts}) ->
  State#{asts => [Ast | Asts]}.

-spec pop_ast(state()) -> {ast(), state()}.
pop_ast(State = #{asts := [Ast | Asts]}) ->
  {Ast, State#{asts => Asts}}.

-spec pop_ast(state(), non_neg_integer()) -> {[ast()], state()}.
pop_ast(State, N) ->
  pop_ast(State, N, true).

-spec pop_ast(state(), non_neg_integer(), boolean()) -> {[ast()], state()}.
pop_ast(State = #{asts := Asts}, N, Reverse) ->
  {ReturnAsts, RestAsts} = lists:split(N, Asts),
  { case Reverse of true -> lists:reverse(ReturnAsts); false -> ReturnAsts end
  , State#{asts => RestAsts}
  }.

%% ----- Lexical renames -------

-spec add_lexical_renames_scope(state()) -> state().
add_lexical_renames_scope(State = #{lexical_renames := Renames}) ->
  State#{lexical_renames => clj_scope:new(Renames)}.

-spec remove_lexical_renames_scope(state()) -> state().
remove_lexical_renames_scope(State = #{lexical_renames := Renames}) ->
  State#{lexical_renames => clj_scope:parent(Renames)}.

%% @doc Finds and returns the name of the lexical rename.
%%
%% This function always returns something valid because the LocalExpr
%% is always registered in the lexixal scope, the analyzer makes sure
%% this happens.
%% @end
-spec get_lexical_rename(local_expr(), state()) -> binary().
get_lexical_rename(LocalExpr, State) ->
  #{lexical_renames := Renames} = State,
  IsUnderscore = maps:get(underscore, LocalExpr, false),
  HasShadow    = shadow_depth(LocalExpr) > 0,

  RenameSym    = if
                   IsUnderscore ->
                     Code = underscore_hash(LocalExpr),
                     clj_scope:get(Code, Renames);
                   HasShadow ->
                     Code = hash_scope(LocalExpr),
                     clj_scope:get(Code, Renames);
                   true ->
                     maps:get(name, LocalExpr)
                 end,

  case RenameSym of
    ?NIL      -> <<"">>;
    RenameSym -> 'clojerl.Symbol':str(RenameSym)
  end.

-spec put_lexical_rename(binding_expr(), state()) -> state().
put_lexical_rename(#{pattern := #{underscore := true} = LocalExpr}, State) ->
  #{lexical_renames := Renames} = State,

  Code       = underscore_hash(LocalExpr),
  ShadowName = <<"__underscore__">>,
  NewRenames = clj_scope:put(Code, clj_rt:gensym(ShadowName), Renames),

  State#{lexical_renames => NewRenames};
put_lexical_rename(#{shadow := ?NIL}, State) ->
  State;
put_lexical_rename(#{pattern := #{name := Name} = LocalExpr}, State) ->
  #{lexical_renames := Renames} = State,

  Code       = hash_scope(LocalExpr),
  NameBin    = 'clojerl.Symbol':name(Name),
  ShadowName = <<NameBin/binary, "__shadow__">>,

  NewRenames = clj_scope:put(Code, clj_rt:gensym(ShadowName), Renames),

  State#{lexical_renames => NewRenames};
put_lexical_rename(_, State) ->
  State.

-spec underscore_hash(local_expr()) -> integer().
underscore_hash(#{underscore := true} = LocalExpr) ->
  %% Keep only some fields to avoid calculating a hash of a deeply nested value
  erlang:phash2(maps:with([id, name, op], LocalExpr)).

-spec hash_scope(local_expr()) -> binary().
hash_scope(LocalExpr) ->
  Depth = shadow_depth(LocalExpr),
  #{name := Name} = LocalExpr,
  NameBin = 'clojerl.Symbol':name(Name),
  term_to_binary({NameBin, Depth}).

-spec shadow_depth(local_expr()) -> non_neg_integer().
shadow_depth(LocalExpr = #{shadow := _}) ->
  do_shadow_depth(LocalExpr, 0);
shadow_depth(_) ->
  0.

-spec do_shadow_depth(local_expr(), non_neg_integer()) -> non_neg_integer().
do_shadow_depth(#{shadow := Shadowed}, Depth) when Shadowed =/= ?NIL ->
  do_shadow_depth(Shadowed, Depth + 1);
do_shadow_depth(_, Depth) ->
  Depth.

%% ----- Vars -------

-spec var_val_function(ast(), ast(), [term()]) -> ast().
var_val_function(ValAst, VarAst, Ann) ->
  TestAst        = call_mfa('clojerl.Var', dynamic_binding, [VarAst], Ann),

  NilAtom        = cerl:ann_c_atom(Ann, ?NIL),
  NilClauseAst   = cerl:ann_c_clause(Ann, [NilAtom], ValAst),

  XVar           = new_c_var(Ann),
  TupleAst       = cerl:ann_c_tuple(Ann, [cerl:c_atom(ok), XVar]),
  ValueClauseAst = cerl:ann_c_clause(Ann, [TupleAst], XVar),

  cerl:ann_c_case(Ann, TestAst, [NilClauseAst, ValueClauseAst]).

-spec var_invoke( 'clojerl.Var':type()
                , 'clojerl.Symbol':type()
                , [ast()]
                , [term()]
                , state()) -> ast().
var_invoke(Var, Symbol, Args, Ann, State) ->
  VarMeta = clj_rt:meta(Var),
  Module  = 'clojerl.Var':module(Var),

  case clj_rt:get(VarMeta, 'fn?', false) of
    true ->
      Function     = 'clojerl.Var':function(Var),
      {_, Args1}   = 'clojerl.Var':process_args(Var, Args, fun list_ast/1),
      CurrentNs    = 'clojerl.Namespace':current(),
      CurrentNsSym = 'clojerl.Namespace':name(CurrentNs),
      NsName       = 'clojerl.Symbol':name(CurrentNsSym),
      VarNsName    = 'clojerl.Var':namespace(Var),
      ForceRemote  = maps:get(force_remote_invoke, State),
      %% When the var's symbol is not namespace qualified and the var's
      %% namespace is the current namespace, emit a local function
      %% call, otherwise emit a remote call.
      case 'clojerl.Symbol':namespace(Symbol) of
        ?NIL when NsName =:= VarNsName, not ForceRemote ->
          call_fa(Function, Args1, Ann);
        _ ->
          call_mfa(Module, Function, Args1, Ann)
      end;
    false ->
      ValFunction = 'clojerl.Var':val_function(Var),
      FunAst      = call_mfa(Module, ValFunction, [], Ann),
      ArgsAst     = list_ast(Args),
      call_mfa('clojerl.IFn', apply, [FunAst, ArgsAst], Ann)
  end.

%% ----- Map -------

%% cerl:c_map_pair_exact/2 doesn't exist in 18.3
-spec c_map_pair_exact(ast(), ast()) -> cerl:c_map_pair().
c_map_pair_exact(K, V) ->
  Exact = cerl:c_atom(exact),
  cerl:ann_c_map_pair([], Exact, K, V).

%% ----- Annotations -------

-spec file_from(clj_env:env()) -> binary().
file_from(Env) ->
  case clj_env:location(Env) of
    ?NIL -> <<>>;
    Location -> maps:get(file, Location, <<>>)
  end.

-spec ann_from(clj_env:env()) -> [term()].
ann_from(Env) ->
  case clj_env:location(Env) of
    ?NIL -> [0];
    Location  ->
      [ maps:get(line, Location, 0)
      , {file, maps:get(file, Location, "")}
      ]
  end.

%% ----- Misc -------

-spec to_atom('clojerl.Symbol':type()) -> atom().
to_atom(Symbol) ->
  binary_to_atom('clojerl.Symbol':name(Symbol), utf8).

-spec default_compiler_options() -> clj_compiler:opts().
default_compiler_options() ->
  #{erl_flags => [binary, debug_info], output_dir => <<"ebin">>}.

-spec new_c_var(cerl:ann()) -> cerl:c_var().
new_c_var(Ann) ->
  N = case erlang:get(local_var_counter) of
        ?NIL -> 0;
        X -> X
      end,
  erlang:put(local_var_counter, N + 1),
  Name = list_to_atom("clj " ++ integer_to_list(N)),
  cerl:ann_c_var([compiler_generated | Ann], Name).
