%% @doc Clojerl emitter.
%%
%% Emits Core Erlang from the expressions it receives from the
%% analyzer.
%%
%% This is the third and last step in the compilation process.
%%
%% The generated Core Erlang modules are maintained and updated in
%% memory through the usage of `clj_module'.
-module(clj_emitter).

-include("clojerl.hrl").
-include("clojerl_int.hrl").
-include("clojerl_protocol.hrl").
-include("clojerl_expr.hrl").

-export([ emit/1
        , new_c_var/1
        , function_form/4
        ]).

-type ast()   :: cerl:cerl().

-type state() :: #{ asts                => [ast()]
                  , lexical_renames     => clj_scope:scope()
                  , force_remote_invoke => boolean()
                  }.

%% @doc Emits Core Erlang from the expression found in `Env'.
-spec emit(clj_env:env()) -> {[ast()], clj_env:env()}.
emit(Env0) ->
  {Expr, Env} = clj_env:pop_expr(Env0),
  State = ast(Expr, initial_state()),
  Asts  = lists:reverse(maps:get(asts, State)),
  {Asts, Env}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec initial_state() -> state().
initial_state() ->
  #{ asts                => []
   , lexical_renames     => clj_scope:new()
   , force_remote_invoke => false
   }.

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
ast(#{op := local} = Expr, State0) ->
  ?DEBUG(local),
  #{env := Env} = Expr,
  {NameBin, State1} = get_lexical_rename(Expr, State0),
  NameAtom          = binary_to_atom(NameBin, utf8),
  Ast               = cerl:ann_c_var(ann_from(Env), NameAtom),

  push_ast(Ast, State1);
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
        InitAst = case
                    cerl:is_literal(InitAst0)
                    andalso cerl:is_literal_term(InitAst0)
                  of
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

  ProtocolModules = [ 'erlang.Type':module(ProtocolType)
                      || #{type := ProtocolType} <- ProtocolsExprs
                    ],

  %% Add this new type to the protocol
  Opts = clj_env:get(compiler_opts, #{}, Env),
  [ protocol_add_type(Module, ProtocolModule, Opts)
    || ProtocolModule <- ProtocolModules
  ],

  %% Attributes
  Attributes = [ { cerl:ann_c_atom(Ann, behavior)
                 , cerl:ann_abstract(Ann, [ProtocolModule])
                 }
                 || ProtocolModule <- ProtocolModules
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

  %% Create a constructor that doesn't include the hidden fields.
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

  Opts = clj_env:get(compiler_opts, #{}, Env),
  clj_compiler:module(clj_module:get_module(Module), Opts),

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

  ProtocolAttr = {cerl:ann_c_atom(Ann, protocol), cerl:abstract([true])},
  Functions0   = [protocol_function(Sig, Module, Ann) || Sig <- MethodsSigs],
  Callbacks    = [ {cerl:fname_id(FName), cerl:fname_arity(FName)}
                   || {FName, _} <- Functions0
                 ],

  Satifies     = satisfies_function(Ann),
  Extends      = extends_function(Ann),
  BehaviorInfo = behaviour_info_function(Ann, Callbacks),

  Functions1   = [Satifies, Extends, BehaviorInfo | Functions0],
  Exports      = [ {cerl:fname_id(FName), cerl:fname_arity(FName)}
                   || {FName, _} <- Functions1
                 ],


  clj_module:add_attributes([ProtocolAttr], Module),
  clj_module:add_functions(Functions1, Module),
  clj_module:add_exports(Exports, Module),

  Opts = clj_env:get(compiler_opts, #{}, Env),
  clj_compiler:module(clj_module:get_module(Module), Opts),

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

  TypeBin    = 'erlang.Type':str(Type),
  TypeModule = 'erlang.Type':module(Type),

  EmitProtocolFun =
    fun(#{type := ProtoType} = Proto, StateAcc) ->
        ProtoBin    = 'erlang.Type':str(ProtoType),
        ProtoModule = 'erlang.Type':module(ProtoType),

        ImplModule  = clj_protocol:impl_module(ProtoBin, TypeBin),

        clj_module:ensure_loaded(file_from(Env), ImplModule),

        MethodsExprs = maps:get(Proto, Impls),

        %% Functions
        StateAcc1  = lists:foldl(fun ast/2, StateAcc, MethodsExprs),
        {FunctionsAsts, StateAcc2} = pop_ast(StateAcc1, length(MethodsExprs)),

        %% Exports
        Exports    = lists:map(fun function_signature/1, FunctionsAsts),

        clj_module:remove_all_functions(ImplModule),
        clj_module:add_exports(Exports, ImplModule),
        clj_module:add_functions(FunctionsAsts, ImplModule),

        Opts       = clj_env:get(compiler_opts, #{}, Env),

        clj_compiler:module(clj_module:get_module(ImplModule), Opts),
        protocol_add_type(TypeModule, ImplModule, ProtoModule, Opts),

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
%% behaviour
%%------------------------------------------------------------------------------
ast(#{op := behaviour} = Expr, State) ->
  #{ name := NameSym
   , env  := Env
   } = Expr,

  Ann       = ann_from(Env),
  Name      = to_atom(NameSym),
  CurrentNs = 'clojerl.Namespace':current(),
  NsNameSym = 'clojerl.Namespace':name(CurrentNs),
  Module    = to_atom(NsNameSym),
  ok        = clj_module:ensure_loaded(file_from(Env), Module),

  BehaviourAttr = {cerl:ann_c_atom(Ann, behavior), cerl:abstract([Name])},
  clj_module:add_attributes([BehaviourAttr], Module),

  Ast = cerl:ann_c_atom(Ann, ?NIL),
  push_ast(Ast, State);
%%------------------------------------------------------------------------------
%% fn, invoke, erl_fun
%%------------------------------------------------------------------------------
ast(#{op := fn} = Expr, State) ->
  ?DEBUG(fn),
  #{ local   := LocalExpr
   , env     := Env
   } = Expr,

  Ann = ann_from(Env),

  %% Use letrec_defs which defines a let binding for recursion.
  %% Ideally this would only be introduced for functions where
  %% it is needed, but there is currently no flag that marks a
  %% fn as having recursion.
  { DefsAsts
  , [FnValue]
  , State1
  } = letrec_defs([LocalExpr], [Expr], State),
  Ast = cerl:ann_c_letrec(Ann, DefsAsts, FnValue),

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

      ApplyAst = cerl:ann_c_apply([?LOCAL | Ann], FVarAst, Args),
      Ast      = cerl:ann_c_let(Ann, [FVarAst], ResolveAst, ApplyAst),

      push_ast(Ast, State);
    %% Apply Fn
    _ ->
      {FunAst, State2} = pop_ast(ast(FExpr, State1)),
      ArgsAst = list_ast(Args),
      Module  = case FExpr of
                  #{tag := ?NO_TAG}         -> 'clojerl.IFn';
                  #{tag := #{type := Type}} -> 'erlang.Type':module(Type)
                end,
      Ast = call_mfa(Module, apply, [FunAst, ArgsAst], Ann),

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

  {DefsAsts, WrappedAsts, State1} = letrec_defs(VarsExprs, FnsExprs, State),
  {BodyAst,  State2} = pop_ast(ast(BodyExpr, State1)),

  FNamesAsts  = [FNameAst || {FNameAst, _} <- DefsAsts],
  VarsAsts    = [cerl:c_var(cerl:fname_id(FNameAst)) || FNameAst <- FNamesAsts],
  LetAst      = cerl:ann_c_let( Ann
                              , VarsAsts
                              , cerl:c_values(WrappedAsts)
                              , BodyAst
                              ),
  LetRecAst   = cerl:ann_c_letrec(Ann, DefsAsts, LetAst),

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
ast(#{op := vector, items := [], env := Env}, State) ->
  Ast = cerl:ann_abstract(ann_from(Env), 'clojerl.Vector':?CONSTRUCTOR([])),
  push_ast(Ast, State);
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
ast(#{op := map, keys := [], env := Env}, State) ->
  Ast = cerl:ann_abstract(ann_from(Env), 'clojerl.Map':?CONSTRUCTOR([])),
  push_ast(Ast, State);
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
ast(#{op := set, items := [], env := Env}, State) ->
  Ast = cerl:ann_abstract(ann_from(Env), 'clojerl.Set':?CONSTRUCTOR([])),
  push_ast(Ast, State);
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
  {ThenAst, State2} = pop_ast(ast(ThenExpr, State1)),

  TestVar           = new_c_var(Ann),
  Nil               = cerl:ann_c_atom(Ann, ?NIL),
  False             = cerl:ann_c_atom(Ann, false),

  NilGuard          = call_mfa(erlang, '=/=', [TestVar, Nil], Ann),
  FalseGuard        = call_mfa(erlang, '=/=', [TestVar, False], Ann),

  TrueClause        = cerl:ann_c_clause(Ann, [], NilGuard, FalseGuard),
  FalseClause       = cerl:ann_c_clause(Ann, [], False),

  ThenGuardAst      = cerl:ann_c_case( Ann
                                     , cerl:ann_c_values(Ann, [])
                                     , [TrueClause, FalseClause]
                                     ),

  ThenClause        = cerl:ann_c_clause(Ann, [TestVar], ThenGuardAst, ThenAst),

  %% Else
  ElseVar           = new_c_var(Ann),
  {ElseAst, State3} = pop_ast(ast(ElseExpr, State2)),
  ElseClause        = cerl:ann_c_clause(Ann, [ElseVar], ElseAst),

  Ast = cerl:ann_c_case(Ann, TestAst, [ThenClause, ElseClause]),
  push_ast(Ast, State3);
%%------------------------------------------------------------------------------
%% case
%%------------------------------------------------------------------------------
ast(#{op := 'case'} = Expr, State0) ->
  ?DEBUG('case'),
  #{ test    := TestExpr
   , clauses := ClausesExprs
   , default := DefaultExpr
   , env     := Env
   } = Expr,

  State1 = add_lexical_renames_scope(State0),

  {TestAst, State2} = pop_ast(ast(TestExpr, State1)),
  State3 = lists:foldl(fun clause/2, State2, ClausesExprs),
  {ClausesAsts0, State4} = pop_ast(State3, length(ClausesExprs), false),

  {ClausesAsts1, State5} =
    case DefaultExpr of
      ?NIL -> {ClausesAsts0, State4};
      _ ->
        EnvDefault    = maps:get(env, DefaultExpr),
        AnnDefault    = ann_from(EnvDefault),
        DefaultVarAst = new_c_var(AnnDefault),
        {DefaultAst, State4_1} = pop_ast(ast(DefaultExpr, State4)),
        DefaultClause = cerl:ann_c_clause( AnnDefault
                                         , [DefaultVarAst]
                                         , DefaultAst
                                         ),
        {[DefaultClause | ClausesAsts0], State4_1}
    end,

  Ann = ann_from(Env),
  ClausesAsts2 = [fail_clause(case_clause, Ann) | ClausesAsts1],
  ClausesAsts3 = lists:reverse(ClausesAsts2),

  CaseAst = cerl:ann_c_case(ann_from(Env), TestAst, ClausesAsts3),
  State6  = remove_lexical_renames_scope(State5),

  push_ast(CaseAst, State6);
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

  MatchAstFun =
    fun (BindingExpr, {Bindings, StateAcc}) ->
        #{init := InitExpr, env := BindingEnv} = BindingExpr,
        AnnBinding           = ann_from(BindingEnv),
        {Pattern, StateAcc1} = pop_ast(ast(BindingExpr, StateAcc)),
        {Init, StateAcc2}    = pop_ast(ast(InitExpr, StateAcc1)),
        Binding              = {cerl:type(Pattern), Pattern, Init, AnnBinding},
        {[Binding | Bindings], StateAcc2}
    end,

  {Bindings, State2} = lists:foldl(MatchAstFun, {[], State1}, BindingsExprs),
  {Body, State3}     = pop_ast(ast(BodyExpr, State2)),

  FoldFun =
    fun
      ({var, Var, Init, AnnBinding}, BodyAcc) ->
        cerl:ann_c_let(AnnBinding, [Var], Init, BodyAcc);
      ({_, Pattern, Init, AnnBinding}, BodyAcc) ->
        {PatArgs, PatGuards} = clj_emitter_pattern:patterns([Pattern]),
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

            Arity      = length(Patterns),
            FNameAst   = cerl:ann_c_fname(Ann, LoopIdAtom, Arity),
            ClauseAst  = cerl:ann_c_clause(Ann, Patterns, Body),
            { Vars
            , CaseAst} = case_from_clauses(Ann, [ClauseAst]),
            FunAst     = cerl:ann_c_fun(Ann, Vars, CaseAst),
            Defs       = [{FNameAst, FunAst}],
            ApplyAst   = cerl:ann_c_apply([?LOCAL | Ann], FNameAst, Patterns),
            LetRecAst  = cerl:ann_c_letrec(Ann, Defs, ApplyAst),

            lists:foldl(FoldFun, LetRecAst, Bindings)
        end,

  State4 = remove_lexical_renames_scope(State3),

  push_ast(Ast, State4);
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
            cerl:ann_c_apply([?LOCAL | Ann], NameAst, Args)
        end,
  push_ast(Ast, State1);
%%------------------------------------------------------------------------------
%% throw
%%------------------------------------------------------------------------------
ast(#{op := throw} = Expr, State0) ->
  ?DEBUG(throw),
  #{ exception  := ExceptionExpr
   , stacktrace := StacktraceExpr
   , env        := Env
   } = Expr,

  {Exception,  State1} = pop_ast(ast(ExceptionExpr, State0)),
  {Stacktrace, State2} = case StacktraceExpr of
                           ?NIL -> {?NIL, State1};
                           _    -> pop_ast(ast(StacktraceExpr, State1))
                         end,

  Ann  = ann_from(Env),
  Ast  = case Stacktrace of
           ?NIL -> call_mfa(erlang, error, [Exception], Ann);
           _    ->
             ErrorAtom = cerl:abstract(error),
             Args = [ErrorAtom, Exception, Stacktrace],
             call_mfa(erlang, raise, Args, Ann)
         end,
  push_ast(Ast, State2);
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
  {Catches, State2} = pop_ast( lists:foldl(fun ast/2, State1, CatchesExprs)
                             , length(CatchesExprs)
                             , false
                             ),

  [_, Y, Z] = CatchVarsAsts = [ new_c_var(Ann)
                              , new_c_var(Ann)
                              , new_c_var(Ann)
                              ],
  RaiseAst          = cerl:c_primop(cerl:c_atom(raise), [Z, Y]),

  %% A last catch-all clause is mandatory for when none of the previous
  %% catch clauses were matched.
  CatchAllClause    = cerl:ann_c_clause(Ann, CatchVarsAsts, RaiseAst),
  { ClausesVars
  , CaseAst
  } = case_from_clauses(Ann, lists:reverse([CatchAllClause | Catches])),

  {Finally, State3} = case FinallyExpr of
                        ?NIL -> {?NIL, State2};
                        _    -> pop_ast(ast(FinallyExpr, State))
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
        ApplyFinallyAst = cerl:ann_c_apply([?LOCAL | Ann], FinallyFName, []),

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

  push_ast(Ast, State3);
%%------------------------------------------------------------------------------
%% catch
%%------------------------------------------------------------------------------
ast(#{op := 'catch'} = Expr, State0) ->
  ?DEBUG('catch'),
  #{ class      := ErrTypeExpr
   , local      := PatternExpr
   , stacktrace := StackExpr
   , body       := BodyExpr
   , guard      := GuardExpr
   , env        := Env
   } = Expr,

  Ann                   = ann_from(Env),
  State1                = add_lexical_renames_scope(State0),
  {ClassAst, State2}    = case maps:get(op, ErrTypeExpr) of
                            binding -> pop_ast(ast(ErrTypeExpr, State1));
                            type    ->
                              ErrorTypeAst = cerl:ann_abstract(Ann, error),
                              {ErrorTypeAst, State1}
                          end,
  TempStackAst          = new_c_var(Ann),
  {PatternAst0, State3} = pop_ast(ast(PatternExpr, State2)),
  {StackAst, State4}    = case StackExpr of
                            ?NIL -> {?NIL, State3};
                            _    -> pop_ast(ast(StackExpr, State3))
                          end,
  {Guard0, State5}      = pop_ast(ast(GuardExpr, State4)),
  {Body, State6}        = catch_body(StackAst, TempStackAst, BodyExpr, State5),

  { [PatternAst1]
  , PatGuards
  } = clj_emitter_pattern:patterns([PatternAst0], locals(Env, State6)),
  VarsAsts = [ClassAst, PatternAst1, TempStackAst],
  Guard1   = clj_emitter_pattern:fold_guards(Guard0, PatGuards),
  Ast      = cerl:ann_c_clause(Ann, VarsAsts, Guard1, Body),

  State7   = remove_lexical_renames_scope(State6),
  push_ast(Ast, State7);
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

  {Ast0, State1} = pop_ast(ast(BodyExpr, State)),

  CurrentNs  = 'clojerl.Namespace':current(),
  NameSym    = 'clojerl.Namespace':name(CurrentNs),
  ModuleName = to_atom(NameSym),
  ok         = clj_module:ensure_loaded(file_from(Env), ModuleName),
  Ast1       = clj_module:replace_remote_calls(Ast0, ModuleName),
  clj_module:add_on_load(Ast1, ModuleName),

  push_ast(Ast0, State1);
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
  FName = cerl:fname_id(FNameAst),
  Arity = cerl:fname_arity(FNameAst),
  FailClause = fail_function_clause(Ann, FName, Arity),
  Clauses = [ClauseAst, FailClause],
  CaseAst = cerl:ann_c_case(Ann, cerl:c_values(Vars), Clauses),

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
  {Vars, CaseAst} = case_from_clauses(Ann, [ClauseAst], AccessorName, 1),
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
  Value    = case Typename of
               '_'      -> new_c_var([]);
               Typename -> cerl:c_atom(Typename)
             end,
  TypeMapPairAst = MakePair(cerl:c_atom(?TYPE), Value),
  MakeMap([TypeMapPairAst | MapPairsAsts]).

%% ----- defprotocol ------

%% The clauses for these types/tags in the protocol dipatch
%% functions should always be present
-spec default_types() -> [atom() | module()].
default_types() ->
  [ %% Fallback clause for tagged maps since otherwise
    %% the is_map/1 clause might be matched
    ?CATCH_ALL_MAP_TAG
    %% is_binary/1 and is_bitstring/1 might return
    %% true for the same value so we add a default
    %% not implemented clause
  , 'clojerl.String'
    %% is_atom will return true for boolean and
    %% undefined (i.e. nil)
  , 'clojerl.Boolean'
  , ?NIL_TYPE
    %% Fallback clause
  , ?CATCH_ALL_TAG
  ].

-spec protocol_function(any(), module(), any()) ->
  {cerl:c_var(), cerl:c_fun()}.
protocol_function(Sig, Module, Ann) ->
  MethodSym      = clj_rt:first(Sig),
  Arity          = clj_rt:second(Sig),
  Method         = to_atom(MethodSym),
  Vars           = [new_c_var(Ann) || _ <- lists:seq(1, Arity)],
  FirstVar       = hd(Vars),
  Args           = [ cerl:c_atom(Module)
                   , cerl:c_atom(Method)
                   , FirstVar
                   ],

  NotImplemented = call_mfa(clj_protocol, not_implemented, Args, Ann),
  Clauses        = [ protocol_clause(TagOrType, NotImplemented)
                     || TagOrType <- default_types()
                   ],
  Body           = cerl:ann_c_case(Ann, FirstVar, Clauses),

  { cerl:c_fname(Method, Arity)
  , cerl:ann_c_fun(Ann, Vars, Body)
  }.

-spec satisfies_function(any()) ->
  {cerl:c_var(), cerl:c_fun()}.
satisfies_function(Ann) ->
  FalseBody   = cerl:abstract(false),
  Clauses     = [ protocol_clause(TagOrType, FalseBody)
                  || TagOrType <- default_types()
                ],
  Arg         = new_c_var(Ann),
  Body        = cerl:ann_c_case(Ann, Arg, Clauses),

  { cerl:c_fname(?SATISFIES, 1)
  , cerl:ann_c_fun(Ann, [Arg], Body)
  }.

-spec extends_function(any()) ->
  {cerl:c_var(), cerl:c_fun()}.
extends_function(Ann) ->
  Arg         = new_c_var(Ann),

  FalseBody   = cerl:abstract(false),
  Clause      = cerl:ann_c_clause(Ann, [new_c_var(Ann)], FalseBody),
  Body        = cerl:ann_c_case(Ann, Arg, [Clause]),

  { cerl:c_fname(?EXTENDS, 1)
  , cerl:ann_c_fun(Ann, [Arg], Body)
  }.

-spec behaviour_info_function(any(), [{atom(), arity()}]) ->
  {cerl:c_var(), cerl:c_fun()}.
behaviour_info_function(Ann, FunArityList) ->
  Arg          = new_c_var(Ann),

  ClauseBody   = cerl:abstract(FunArityList),

  Callbacks    = cerl:abstract(callbacks),
  Clause1      = cerl:ann_c_clause(Ann, [Callbacks], ClauseBody),

  OptCallbacks = cerl:abstract(optional_callbacks),
  Clause2      = cerl:ann_c_clause(Ann, [OptCallbacks], ClauseBody),

  Body         = cerl:ann_c_case(Ann, Arg, [Clause1, Clause2]),

  { cerl:c_fname(?BEHAVIOUR_INFO, 1)
  , cerl:ann_c_fun(Ann, [Arg], Body)
  }.

-spec protocol_add_type(module(), module(), map()) -> ok.
protocol_add_type(TypeModule, ProtocolModule, Opts) ->
  protocol_add_type(TypeModule, TypeModule, ProtocolModule, Opts).

-spec protocol_add_type(module(), module(), module(), map()) -> ok.
protocol_add_type(TypeModule, ImplModule, ProtocolModule, Opts) ->
  clj_module:ensure_loaded(<<"">>, ProtocolModule),
  Functions0 = clj_module:get_functions(ProtocolModule),
  Functions1 = [ protocol_function_add_type(F, TypeModule, ImplModule)
                 || F <- Functions0
               ],
  clj_module:add_functions(Functions1, ProtocolModule),
  clj_compiler:module(clj_module:get_module(ProtocolModule), Opts),
  ok.

-spec protocol_function_add_type( { {atom(), arity()}
                                  , {cerl:c_var(), cerl:c_fun()}
                                  }
                                , module()
                                , module()
                                ) -> {cerl:c_var(), cerl:c_fun()}.
protocol_function_add_type({{Name, _}, X}, _TypeModule, _ImplModule)
  when Name =:= module_info;
       Name =:= behavior_info;
       Name =:= behaviour_info ->
  X;
protocol_function_add_type({{Name, Arity}, {FName, Fun0}}
                          , TypeModule
                          , ImplModule
                          ) ->
  Fun1  = clean_protocol_function(Fun0),
  Vars  = cerl:fun_vars(Fun1),
  Body0 = cerl:fun_body(Fun1),

  ClauseBody   = case {Name, Arity} of
                   {?EXTENDS,   1} -> cerl:c_atom(true);
                   {?SATISFIES, 1} -> cerl:c_atom(true);
                   _ -> call_mfa(ImplModule, Name, Vars, [])
                 end,
  CreateClause = case {Name, Arity} of
                   {?EXTENDS, 1} -> fun protocol_clause_no_guard/2;
                   _ -> fun protocol_clause/2
                 end,

  Body1 = case_add_type(TypeModule, Body0, ClauseBody, CreateClause),
  Fun2  = cerl:update_c_fun(Fun1, Vars, Body1),

  {FName, Fun2}.

-spec clean_protocol_function(cerl:cerl()) -> cerl:cerl().
clean_protocol_function(Fun0) ->
  Fun1 = remove_nested_cases(Fun0),
  remove_duplicated_catch_all(Fun1).

%% Remove nested cases
-spec remove_nested_cases(cerl:cerl()) -> cerl:cerl().
remove_nested_cases(Fun) ->
  Case         = cerl:fun_body(Fun),
  [Clause | _] = cerl:case_clauses(Case),
  ClauseBody = cerl:clause_body(Clause),

  case cerl:is_c_case(ClauseBody) of
    true ->
      InnerCase = ClauseBody,
      Args      = cerl:clause_pats(Clause),
      cerl:update_c_fun(Fun, Args, InnerCase);
    _ ->
      Fun
  end.

%% Remove duplicated catch all clause
-spec remove_duplicated_catch_all(cerl:cerl()) -> cerl:cerl().
remove_duplicated_catch_all(Fun) ->
  Case0   = cerl:fun_body(Fun),
  Clauses = case cerl:case_clauses(Case0) of
              Clauses_ when length(Clauses_) >= 2 ->
                [_Last, BeforeLast | _] = lists:reverse(Clauses_),
                [Pattern] = cerl:clause_pats(BeforeLast),
                Guard     = cerl:clause_guard(BeforeLast),
                case {cerl:type(Pattern), cerl:type(Guard)} of
                  {var, literal} -> lists:droplast(Clauses_);
                  _ -> Clauses_
                end;
              Clauses_ -> Clauses_
            end,
  Args0   = cerl:case_arg(Case0),
  Case1   = cerl:update_c_case(Case0, Args0, Clauses),
  Vars    = cerl:fun_vars(Fun),
  cerl:update_c_fun(Fun, Vars, Case1).

%% The clause should only be replaced when its body is:
%% a. The literal 'false'
%% b. A call to clj_protocol:not_implemented/3
-spec should_replace(cerl:cerl()) -> boolean().
should_replace(Clause) ->
  Body = cerl:clause_body(Clause),
  case cerl:type(Body) of
    literal -> cerl:concrete(Body) =:= false;
    call ->
      Module   = cerl:call_module(Body),
      Function = cerl:call_name(Body),

      { cerl:concrete(Module)
      , cerl:concrete(Function)
      } =:= {clj_protocol, not_implemented};
    _ -> false
  end.

-spec case_add_type(atom(), cerl:cerl(), cerl:cerl(), function()) ->
  cerl:cerl().
case_add_type(?DEFAULT_TYPE, Case, ClauseBody, CreateClause) ->
  Arg         = cerl:case_arg(Case),
  Clauses0    = cerl:case_clauses(Case),
  ClausesMap0 = clauses_to_map(Clauses0),

  Replace     =
    fun (Tag, OriginalClause) ->
        case
          lists:member(Tag, default_types())
          andalso should_replace(OriginalClause)
        of
          true -> CreateClause(Tag, ClauseBody);
          _ -> OriginalClause
        end
    end,
  %% Replace the clauses for some types
  ClausesMap1 = maps:map(Replace, ClausesMap0),

  Clauses1    = sort_clauses(ClausesMap1),

  cerl:update_c_case(Case, Arg, Clauses1);
case_add_type(TypeModule, Case, ClauseBody, CreateClause) ->
  Arg         = cerl:case_arg(Case),
  Clauses0    = cerl:case_clauses(Case),
  ClausesMap0 = clauses_to_map(Clauses0),

  Clause      = CreateClause(TypeModule, ClauseBody),
  Tag         = type_from_clause(Clause),

  ClausesMap1 = ClausesMap0#{Tag => Clause},
  Clauses1    = sort_clauses(ClausesMap1),

  cerl:update_c_case(Case, Arg, Clauses1).

-spec clauses_to_map([cerl:cerl()]) -> #{atom() => cerl:cerl()}.
clauses_to_map(Clauses) ->
  FoldFun     = fun(Item, ClausesMap) ->
                    ClauseType = type_from_clause(Item),
                    ClausesMap#{ClauseType => Item}
                end,
  lists:foldl(FoldFun, #{}, Clauses).

%% @doc Generate the clause for a type in the protocol dispatch
%%
%% The generated code will be either (a) for a custom type or
%% (b) for a primitive type.
%%
%% (a) #{?TYPE := 'some.Type'} -> body;
%% (b) ?NIL                    -> body;
%% (c) X when is_something(X)  -> body;
-spec protocol_clause(module(), cerl:cerl()) -> cerl:cerl().
protocol_clause(?CATCH_ALL_TAG, Body) ->
  Pattern = new_c_var([]),
  cerl:c_clause([Pattern], Body);
protocol_clause(?CATCH_ALL_MAP_TAG, Body) ->
  Pattern = type_map_ast('_', [], match),
  cerl:c_clause([Pattern], Body);
protocol_clause(?NIL_TYPE, Body) ->
  Pattern = cerl:c_atom(?NIL),
  cerl:c_clause([Pattern], Body);
protocol_clause(Type, Body) ->
  case maps:get(Type, ?PRIMITIVE_TYPES, custom) of
    custom ->
      Pattern = type_map_ast(Type, [], match),
      cerl:c_clause([Pattern], Body);
    #{pred := Predicate} ->
      Pattern = new_c_var([]),
      Guard   = call_mfa(erlang, Predicate, [Pattern], []),
      cerl:c_clause([Pattern], Guard, Body)
  end.

-spec protocol_clause_no_guard(module(), cerl:cerl()) -> cerl:cerl().
protocol_clause_no_guard(?CATCH_ALL_TAG, Body) ->
  Pattern = new_c_var([]),
  cerl:c_clause([Pattern], Body);
protocol_clause_no_guard(Type, Body) ->
  Pattern = cerl:c_atom(Type),
  cerl:c_clause([Pattern], Body).

-spec type_from_clause(cerl:cerl()) -> {atom(), cerl:cerl()}.
type_from_clause(Clause) ->
  Guard     = cerl:clause_guard(Clause),
  [Pattern] = cerl:clause_pats(Clause),
  case {cerl:type(Pattern), cerl:type(Guard)} of
    %% X when true ->
    {var, literal}     -> ?CATCH_ALL_TAG;
    %% 'TypeAtom' when true ->
    {literal, literal} -> cerl:concrete(Pattern);
    %% #{?TYPE := 'type.Name'} when true ->
    {map, literal}     ->
      [MapPairAst] = cerl:map_es(Pattern),
      ValAst       = cerl:map_pair_val(MapPairAst),
      case cerl:type(ValAst) of
        literal -> cerl:concrete(ValAst);
        var     -> ?CATCH_ALL_MAP_TAG
      end;
    %% X when is_something(X) ->
    _ ->
      GuardNameAst = cerl:call_name(Guard),
      GuardName = cerl:concrete(GuardNameAst),
      maps:get(GuardName, ?GUARDS2TYPES)
  end.

-spec sort_clauses(#{atom() => cerl:cerl()}) -> [cerl:cerl()].
sort_clauses(Clauses) ->
  Sorted = lists:sort(fun compare/2, maps:to_list(Clauses)),
  [X || {_, X} <- Sorted].

-spec compare({atom(), cerl:cerl()}, {atom(), cerl:cerl()}) -> boolean().
compare({X, _}, {Y, _})  -> order(X) < order(Y).

-spec order(atom()) -> non_neg_integer().
order(?CATCH_ALL_TAG) -> 1000;
order(?CATCH_ALL_MAP_TAG) -> 0;
order(T) ->
   case maps:get(T, ?PRIMITIVE_TYPES, -1) of
     #{order := Order} -> Order;
     Order -> Order
   end.

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

  {PatternAst0, StateAcc1}   = pop_ast(ast(PatternExpr, StateAcc)),
  {GuardAst0, StateAcc2}     = pop_ast(ast(GuardExpr, StateAcc1)),
  {BodyAst0, StateAcc3}      = pop_ast(ast(BodyExpr, StateAcc2)),

  { [PatternAst1]
  , PatGuards
  } = clj_emitter_pattern:patterns( [PatternAst0]
                                  , locals(EnvPattern, StateAcc3)
                                  ),
  GuardAst1 = clj_emitter_pattern:fold_guards(GuardAst0, PatGuards),

  ClauseAst = cerl:ann_c_clause(AnnPattern, [PatternAst1], GuardAst1, BodyAst0),
  push_ast(ClauseAst, StateAcc3).

%% ----- Functions -------

%% @private
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

-spec method_to_function_clause(expr(), state()) -> state().
method_to_function_clause(MethodExpr, State) ->
  method_to_clause(MethodExpr, State, function).

-spec method_to_case_clause(expr(), state()) -> state().
method_to_case_clause(MethodExpr, State) ->
  method_to_clause(MethodExpr, State, 'case').

-spec method_to_clause(expr(), state(), function | 'case') ->
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

  State = add_lexical_renames_scope(State0),

  {Args, State1} = pop_ast( lists:foldl(fun ast/2, State, ParamsExprs)
                          , length(ParamsExprs)
                          ),

  {PatternArgs, PatternGuards} = clj_emitter_pattern:patterns(Args),

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
    fun({Arity, MethodsList}, StateAcc) ->
        StateAcc1 = lists:foldl( fun method_to_function_clause/2
                               , StateAcc
                               , MethodsList
                               ),
        {ClausesAst, StateAcc2} = pop_ast(StateAcc1, length(MethodsList)),

        {Vars, BodyAst} = case_from_clauses(Ann, ClausesAst, Name, Arity),
        FunAst = function_form(Name, Ann, Vars, BodyAst),

        clj_module:add_functions([FunAst], Module),

        StateAcc2
    end,

  lists:foldl(FunctionFun, State, maps:to_list(GroupedMethods)).

-spec case_from_clauses([term()], [cerl:c_clause()], atom(), arity()) ->
  {[cerl:c_var()], ast()}.
case_from_clauses(Ann, ClausesAst0, Name, Arity) ->
  MatchFailClause = fail_function_clause(Ann, Name, Arity),
  ClausesAst1     = ClausesAst0 ++ [MatchFailClause],
  Vars            = [new_c_var(Ann) || _ <- lists:seq(0, Arity - 1)],
  CaseAst         = cerl:ann_c_case(Ann, cerl:c_values(Vars), ClausesAst1),
  {Vars, CaseAst}.

-spec case_from_clauses([term()], [cerl:c_clause()]) ->
  {[cerl:c_var()], ast()}.
case_from_clauses(Ann, [ClauseAst | _] = ClausesAst) ->
  Patterns = cerl:clause_pats(ClauseAst),
  Vars     = [new_c_var(Ann) || _ <- lists:seq(0, length(Patterns) - 1)],
  CaseAst  = cerl:ann_c_case(Ann, cerl:c_values(Vars), ClausesAst),
  {Vars, CaseAst}.

-spec fail_function_clause([term()], atom(), arity()) -> ast().
fail_function_clause(Ann, Name, Arity) ->
  PrimOpVars      = [new_c_var(Ann) || _ <- lists:seq(0, Arity - 1)],
  PrimOpArgs      = cerl:c_tuple([cerl:c_atom(function_clause) | PrimOpVars]),
  MatchFailPrimOp = cerl:ann_c_primop( [{function_name, {Name, Arity}} | Ann]
                                     , cerl:c_atom(match_fail)
                                     , [PrimOpArgs]
                                     ),

  cerl:c_clause(PrimOpVars, MatchFailPrimOp).

%% ----- letrec -------

-spec letrec_defs([expr()], [expr()], state()) ->
  {[{ast(), ast()}], [ast()], state()}.
letrec_defs(VarsExprs, FnsExprs, State0) ->
  {VarsAsts, State1} = pop_ast( lists:foldl(fun ast/2, State0, VarsExprs)
                              , length(VarsExprs)
                              ),

  FNamesAsts   = [ generate_fname(VarExpr, FnExpr)
                   || {VarExpr, FnExpr} <- lists:zip(VarsExprs, FnsExprs)
                 ],

  FnValuesAsts = [ generate_fn_values(FNameAst, FnExpr)
                   || {FNameAst, FnExpr} <- lists:zip(FNamesAsts, FnsExprs)
                 ],

  FoldFun = fun
              ({FNameAst, #{'erlang-fn?' := true} = FnExpr}, StateAcc) ->
                erlang_fun(VarsAsts, FnValuesAsts, FNameAst, FnExpr, StateAcc);
              ({FNameAst, FnExpr}, StateAcc) ->
                clojure_fun(VarsAsts, FnValuesAsts, FNameAst, FnExpr, StateAcc)
            end,

  PairsExprs = lists:zip(FNamesAsts, FnsExprs),

  {FunsAsts, State2} = pop_ast( lists:foldl(FoldFun, State1, PairsExprs)
                              , length(PairsExprs)
                              ),

  {lists:zip(FNamesAsts, FunsAsts), FnValuesAsts, State2}.

-spec generate_fname(expr(), expr()) -> ast().
generate_fname(#{name := VarName, env := VarEnv}, FnExpr) ->
  Arity = arity_from_fn(FnExpr),
  cerl:ann_c_fname(ann_from(VarEnv), to_atom(VarName), Arity).

-spec arity_from_fn(expr()) -> arity().
arity_from_fn(#{'erlang-fn?' := true, min_fixed_arity := A}) ->
  A;
arity_from_fn(_) ->
  1.

-spec generate_fn_values(ast(), expr()) -> ast().
generate_fn_values(FNameAst, #{'erlang-fn?' := true}) ->
  FNameAst;
generate_fn_values(FNameAst, _) ->
  call_mfa('clojerl.Fn', ?CONSTRUCTOR, [FNameAst], []).

%% clojerl.Fn functions are emitted as single argument Erlang
%% functions, where the argument should always be an Erlang list.
-spec clojure_fun([expr()], [ast()], ast(), expr(), state()) -> state().
clojure_fun(VarsAsts, FnValuesAsts, FNameAst, FnExpr, State0) ->
  #{ op      := fn
   , methods := Methods
   , env     := Env
   } = FnExpr,

  Ann = ann_from(Env),

  State1 = lists:foldl(fun method_to_case_clause/2, State0, Methods),
  {ClausesAsts, State2} = pop_ast(State1, length(Methods)),

  %% Create the clause that will handle the cases when the arguments
  %% provided are wrong or there is no match in the clauses
  CatchAllVar = new_c_var(Ann),
  Length      = call_mfa(erlang, length, [CatchAllVar], Ann),
  FName0      = cerl:fname_id(FNameAst),
  FName1      = cerl:abstract(atom_to_binary(FName0, utf8)),
  ErrorCtror  = call_mfa( 'clojerl.ArityError'
                        , ?CONSTRUCTOR
                        , [Length, FName1]
                        , Ann),
  ThrowErr    = call_mfa(erlang, error, [ErrorCtror], Ann),
  CatchAll    = cerl:ann_c_clause( Ann
                                 , [CatchAllVar]
                                 , cerl:abstract(true)
                                 , ThrowErr
                                 ),

  ArgsVar  = new_c_var(Ann),
  CaseAst  = cerl:ann_c_case(Ann, ArgsVar, ClausesAsts ++ [CatchAll]),

  %% Wrap the body in a let which includes a binding for recursion
  LetAst   = cerl:ann_c_let( Ann
                           , VarsAsts
                           , cerl:c_values(FnValuesAsts)
                           , CaseAst
                           ),
  FunAst   = cerl:ann_c_fun(Ann, [ArgsVar], LetAst),

  push_ast(FunAst, State2).

-spec erlang_fun([expr()], [ast()], ast(), expr(), state()) -> state().
erlang_fun(VarsAsts, FnValuesAsts, FNameAst, FnExpr, State0) ->
  #{ op      := fn
   , methods := Methods
   , env     := Env
   } = FnExpr,

  Ann = ann_from(Env),
  FName = cerl:fname_id(FNameAst),
  Arity = cerl:fname_arity(FNameAst),
  State1 = lists:foldl(fun method_to_function_clause/2, State0, Methods),
  {ClausesAsts, State2} = pop_ast(State1, length(Methods)),
  {Vars, BodyAst} = case_from_clauses(Ann, ClausesAsts, FName, Arity),

  %% Wrap the body in a let which includes a binding for recursion
  LetAst   = cerl:ann_c_let( Ann
                           , VarsAsts
                           , cerl:c_values(FnValuesAsts)
                           , BodyAst
                           ),
  FunAst   = cerl:ann_c_fun(Ann, Vars, LetAst),

  push_ast(FunAst, State2).

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

-spec locals(clj_env:env(), state()) -> [atom()].
locals(Env, State) ->
  [ begin
      {Name, _} = get_lexical_rename(LocalExpr, State),
      clj_rt:keyword(Name)
    end
    || LocalExpr <- clj_env:get_locals(Env)
  ].

-spec add_lexical_renames_scope(state()) -> state().
add_lexical_renames_scope(State = #{lexical_renames := Renames}) ->
  State#{lexical_renames => clj_scope:new(Renames)}.

-spec remove_lexical_renames_scope(state()) -> state().
remove_lexical_renames_scope(State = #{lexical_renames := Renames}) ->
  State#{lexical_renames => clj_scope:parent(Renames)}.

%% @doc Finds and returns the name of the lexical rename.
%%
%% This function always returns something valid because the LocalExpr
%% is always registered in the lexical scope, the analyzer makes sure
%% this happens.
%% @end
-spec get_lexical_rename(local_expr(), state()) -> {binary(), state()}.
get_lexical_rename(LocalExpr, State0) ->
  #{lexical_renames := Renames} = State0,
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
    ?NIL ->
      State1 = put_lexical_rename(LocalExpr, State0),
      get_lexical_rename(LocalExpr, State1);
    RenameSym ->
      {'clojerl.Symbol':str(RenameSym), State0}
  end.

-spec put_lexical_rename(local_expr(), state()) -> state().
put_lexical_rename(#{underscore := true} = LocalExpr, State) ->
  #{lexical_renames := Renames} = State,

  Code       = underscore_hash(LocalExpr),
  ShadowName = <<"__underscore__">>,
  NewRenames = clj_scope:put(Code, clj_rt:gensym(ShadowName), Renames),

  State#{lexical_renames => NewRenames};
put_lexical_rename(#{shadow := ?NIL}, State) ->
  State;
put_lexical_rename(#{name := Name} = LocalExpr, State) ->
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

-spec hash_scope(local_expr()) -> integer().
hash_scope(LocalExpr) ->
  Depth = shadow_depth(LocalExpr),
  #{name := Name} = LocalExpr,
  NameBin = 'clojerl.Symbol':name(Name),
  erlang:phash2({NameBin, Depth}).

%% @doc Returns 0 if the local is not shadow or the depth otherwise
%%
%% In a binding context (let, loop, fn, letfn), bindings
%% with the same name as other bindings or locals should
%% be shadowed. Locals in a pattern should match the name
%% of the shadowed local (if any).
%% This will avoid Erlang's single assignment behaviour
%% (which is not how Clojure works), and avoid conflicts with
%% variables with the same name (which the Core Erlang linter
%% complains about).
-spec shadow_depth(local_expr()) -> non_neg_integer().
shadow_depth(LocalExpr = #{shadow := _, binding := true}) ->
  do_shadow_depth(LocalExpr, 0);
shadow_depth(LocalExpr = #{shadow := _, binding := false}) ->
  do_shadow_depth(LocalExpr, 0) - 1;
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
      Function       = 'clojerl.Var':function(Var),
      {Arity, Args1} = case 'clojerl.Var':process_args(VarMeta, Args) of
                       {Arity_, Args_, Rest_} ->
                         {Arity_, Args_ ++ [list_ast(Rest_)]};
                       X -> X
                     end,
      ValidArity     = 'clojerl.Var':is_valid_arity(VarMeta, Arity),
      CurrentNs      = 'clojerl.Namespace':current(),
      CurrentNsSym   = 'clojerl.Namespace':name(CurrentNs),
      NsName         = 'clojerl.Symbol':name(CurrentNsSym),
      VarNsName      = 'clojerl.Var':namespace(Var),
      ForceRemote    = maps:get(force_remote_invoke, State),
      %% Emit a local function call when:
      %%  - var's symbol is not namespace qualified
      %%  - var's namespace is the current namespace
      %%  - arity used is valid
      %% Otherwise emit a remote call.
      case 'clojerl.Symbol':namespace(Symbol) of
        ?NIL when ValidArity, NsName =:= VarNsName, not ForceRemote ->
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

%% ----- catch -------

-spec catch_body(ast(), ast(), expr(), state()) -> {ast(), state()}.
catch_body(?NIL, _TempAst, BodyExpr, State0) ->
  pop_ast(ast(BodyExpr, State0));
catch_body(StackAst, TempAst, BodyExpr, State0) ->
  {BodyAst, State1} = pop_ast(ast(BodyExpr, State0)),
  GetStacktrace     = generate_stacktrace(TempAst),
  LetAst            = cerl:c_let([StackAst], GetStacktrace, BodyAst),
  {LetAst, State1}.

-ifdef(FUN_STACKTRACE).
generate_stacktrace(_TempAst) ->
  call_mfa(erlang, get_stacktrace, [], []).
-else.
generate_stacktrace(TempAst) ->
  cerl:c_primop(cerl:c_atom(build_stacktrace), [TempAst]).
-endif.

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

%% @private
-spec new_c_var([term()]) -> cerl:c_var().
new_c_var(Ann) ->
  N = case erlang:get(local_var_counter) of
        ?NIL -> 0;
        X -> X
      end,
  erlang:put(local_var_counter, N + 1),
  Name = list_to_atom(integer_to_list(N)),
  cerl:ann_c_var([compiler_generated | Ann], Name).
