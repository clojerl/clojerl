-module(clj_module).

-behavior(gen_server).

-compile({no_auto_import, [get/1, delete_module/1]}).

-include("clojerl.hrl").
-include("clojerl_int.hrl").
-include_lib("compiler/src/core_parse.hrl").

-export([ with_context/1
        , in_context/0

        , all_modules/0
        , get_module/1
        , ensure_loaded/2
        , maybe_ensure_loaded/1
        , is_loaded/1

        , fake_fun/3
        , replace_calls/2

        , add_mappings/2
        , add_alias/3
        , add_attributes/2
        , add_exports/2
        , add_functions/2
        , get_functions/1
        , remove_all_functions/1
        , add_on_load/2

        , is_clojure/1
        , is_protocol/1
        ]).

%% gen_server callbacks
-export([ start_link/0
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-type function_id() :: {atom(), integer()}.

-record(module, { name              :: atom(),
                  source = ""       :: string(),
                  %% ETS table where mappings are kept. The key is the var's
                  %% name as a binary and the value can be either a var or
                  %% a type.
                  mappings          :: ets:tid(),
                  %% ETS table where aliases are kept. The key is the var's
                  %% name as a binary and the value is the namespace symbol.
                  aliases           :: ets:tid(),
                  %% ETS table where functions are kept. The key is the
                  %% function's name and arity.
                  funs              :: ets:tid(),
                  %% ETS table where fake functions are kept. The key is the
                  %% funs value (i.e. Module:Function/Arity).
                  fake_funs         :: ets:tid(),
                  %% ETS table where function exports are kept. The key is the
                  %% function's name and arity.
                  exports           :: ets:tid(),
                  %% ETS table where expressions that will be included in the
                  %% on_load function are kept. The key is the expression
                  %% itself.
                  on_load           :: ets:tid(),
                  %% ETS table where attributes that are kept.
                  attrs             :: ets:tid()
                }).

-type clj_module() :: #module{}.

-export_type([clj_module/0]).

-define(PID_TO_MODULES,         'clj_module_pid_to_modules').
-define(MODULE_TO_FAKE_MODULES, 'clj_module_module_to_fake_modules').
-define(ON_LOAD_FUNCTION,       '$_clj_on_load').
-define(MODULE_INFO,            'module_info').
-define(CLJ_MODULE_CONTEXT,     '$clj_module_compiling').

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------

-spec with_context(fun()) -> ok.
with_context(Fun) ->
  try
    erlang:put(?CLJ_MODULE_CONTEXT, true),
    Fun()
  after
    erlang:erase(?CLJ_MODULE_CONTEXT)
  end.

-spec in_context() -> boolean().
in_context() ->
  erlang:get(?CLJ_MODULE_CONTEXT) =:= true.

%% @doc Returns a list where each element is Core Erlang module.
%% @end
-spec all_modules() -> [cerl:c_module()].
all_modules() ->
  Self     = erlang:self(),
  {_, All} = clj_utils:ets_get(?PID_TO_MODULES, Self, {Self, []}),
  lists:map(fun to_module/1, All).

-spec get_module(atom()) -> cerl:c_module().
get_module(ModuleName) when is_atom(ModuleName) ->
  to_module(fetch_module(ModuleName)).

%% @doc Makes sure the clj_module is loaded.
-spec ensure_loaded(binary(), module()) -> ok.
ensure_loaded(Source, Name) ->
  case not is_loaded(Name) of
    true  -> load(Source, Name), ok;
    false -> ok
  end.

%% @doc Ensures a module is loaded when in a compiling context
%%
%% This is used from in-ns so that namespaces without any vars
%% definition still get registered when compiling.
%% @end
-spec maybe_ensure_loaded(module()) -> ok.
maybe_ensure_loaded(Name) ->
  File = clj_compiler:current_file(),
  in_context() andalso ensure_loaded(File, Name),
  ok.

%% @doc Gets the named fake fun that corresponds to the mfa provided.
%%
%% A fake fun is generated during compile-time and it provides the
%% same functionality as its original. The only difference is that
%% all calls to functions in the same module are replaced by a call
%% to clj_module:fake_fun/3.
%%
%% This is necessary so that macro functions can be used without
%% having to generate, compile and load the binary for the partial
%% module each time a macro is found.
%%
%% A fake module is generated for each fake fun because a previous
%% attempt that used erl_eval to generate and execute the fake_fun
%% was too slow.
%% @end
-spec fake_fun(module(), atom(), integer()) -> function().
fake_fun(ModuleName, Function, Arity) ->
  case fetch_module(ModuleName) of
    ?NIL ->
      fun ModuleName:Function/Arity;
    Module ->
      FA     = {Function, Arity},
      case clj_utils:ets_get(Module#module.fake_funs, FA) of
        ?NIL ->
          Fun = build_fake_fun(Function, Arity, Module),
          clj_utils:ets_save(Module#module.fake_funs, {FA, Fun}),
          Fun;
        {_, Fun} ->
          Fun
      end
  end.

-spec replace_calls( cerl:cerl() | [cerl:cerl()] | {cerl:cerl(), cerl:cerl()}
                   , module()
                   ) ->
  cerl:cerl() | [cerl:cerl()] | {cerl:cerl(), cerl:cerl()}.
replace_calls(Ast, CurrentModule) ->
  replace_calls(Ast, CurrentModule, undefined).

-spec add_mappings(['clojerl.Var':type()], module() | clj_module()) ->
  clj_module().
add_mappings(_, ?NIL) -> error(badarg);
add_mappings(Mappings, ModuleName) when is_atom(ModuleName)  ->
  add_mappings(Mappings, fetch_module(ModuleName));
add_mappings(Mappings, Module) ->
  AddFun = fun
             ({K, V}) ->
               clj_utils:ets_save(Module#module.mappings, {K, V});
             (V) ->
               K = clj_rt:name(V),
               clj_utils:ets_save(Module#module.mappings, {K, V})
           end,
  lists:foreach(AddFun, Mappings),
  Module.

-spec add_alias( 'clojerl.Symbol':type()
               , 'clojerl.Symbol':type()
               , module() | clj_module()
               ) -> clj_module().
add_alias(AliasSym, AliasedNsSym, ModuleName) when is_atom(ModuleName)  ->
  ok = ensure_loaded(<<?NO_SOURCE>>, ModuleName),
  add_alias(AliasSym, AliasedNsSym, fetch_module(ModuleName));
add_alias(AliasSym, AliasedNsSym, Module) ->
  K = clj_rt:name(AliasSym),
  clj_utils:ets_save(Module#module.aliases, {K, AliasedNsSym}),
  Module.

-spec add_attributes([{cerl:cerl(), cerl:cerl()}], clj_module() | module()) ->
  clj_module().
add_attributes(_, ?NIL) -> error(badarg);
add_attributes(Attrs, ModuleName) when is_atom(ModuleName)  ->
  add_attributes(Attrs, fetch_module(ModuleName));
add_attributes([], Module) ->
  Module;
add_attributes(Attrs, Module) ->
  AddAttr = fun(E) -> clj_utils:ets_save(Module#module.attrs, {E}) end,
  ok = lists:foreach(AddAttr, Attrs),
  Module.

-spec add_exports([{atom(), non_neg_integer()}], clj_module() | module()) ->
  clj_module().
add_exports(_, ?NIL) -> error(badarg);
add_exports(Exports, ModuleName) when is_atom(ModuleName)  ->
  add_exports(Exports, fetch_module(ModuleName));
add_exports(Exports, Module) ->
  AddExport = fun(E) ->
                  clj_utils:ets_save(Module#module.exports, {E})
              end,
  ok = lists:foreach(AddExport, Exports),
  Module.

-spec add_functions([{cerl:cerl(), cerl:cerl()}], module() | clj_module()) ->
  clj_module().
add_functions(_, ?NIL) -> error(badarg);
add_functions(Funs, ModuleName) when is_atom(ModuleName)  ->
  add_functions(Funs, fetch_module(ModuleName));
add_functions(Funs, Module) ->
  SaveFun = fun(F) ->
                FunctionId  = function_id(F),
                ok          = delete_fake_fun(FunctionId, Module),
                clj_utils:ets_save(Module#module.funs, {FunctionId, F})
            end,
  lists:foreach(SaveFun, Funs),
  Module.

-spec remove_all_functions(module() | clj_module()) ->
  clj_module().
remove_all_functions(?NIL) -> error(badarg);
remove_all_functions(ModuleName) when is_atom(ModuleName)  ->
  remove_all_functions(fetch_module(ModuleName));
remove_all_functions(Module) ->
  true = ets:delete_all_objects(Module#module.funs),
  true = ets:delete_all_objects(Module#module.exports),
  Module.

-spec get_functions(module() | clj_module()) ->
  [{function_id(), {cerl:c_fname(), cerl:c_fun()}}].
get_functions(?NIL) -> error(badarg);
get_functions(ModuleName) when is_atom(ModuleName)  ->
  get_functions(fetch_module(ModuleName));
get_functions(Module) ->
  ets:tab2list(Module#module.funs).

-spec add_on_load(cerl:cerl(), module() | clj_module()) ->
  clj_module().
add_on_load(_, ?NIL) -> error(badarg);
add_on_load(Expr, ModuleName) when is_atom(ModuleName) ->
  add_on_load(Expr, fetch_module(ModuleName));
add_on_load(Expr, Module) ->
  clj_utils:ets_save(Module#module.on_load, {Expr, Expr}),
  Module.

-spec is_clojure(module()) -> boolean().
is_clojure(Name) ->
  Key = {?MODULE, is_clojure, Name},
  case clj_cache:get(Key) of
    undefined ->
      Attrs = Name:module_info(attributes),
      IsClojure = lists:keymember(clojure, 1, Attrs),
      clj_cache:put(Key, IsClojure),
      IsClojure;
    {ok, Value} ->
      Value
  end.

-spec is_protocol(module() | cerl:c_module()) -> boolean().
is_protocol(Name) when is_atom(Name) ->
  Key = {?MODULE, is_protocol, Name},
  case clj_cache:get(Key) of
    undefined ->
      Attrs = Name:module_info(attributes),
      IsProtocol = lists:keymember(protocol, 1, Attrs),
      clj_cache:put(Key, IsProtocol),
      IsProtocol;
    {ok, Value} ->
      Value
  end;
is_protocol(CoreModule) ->
  AllAttrs       = cerl:module_attrs(CoreModule),
  {_, Extracted} = extract_attrs(AllAttrs, [clojure, protocol]),
  maps:size(Extracted) == 2.

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(?MODULE, [named_table, set, protected, {keypos, 2}]),
  %% Keeps track of the modules loaded by pid.
  ets:new(?PID_TO_MODULES, [named_table, set, protected, {keypos, 1}]),
  %% Keeps track of fake modules created by which modules.
  %% It's necessary to keep it as a separate table because the
  %% information needs to survive the compiling processes during
  %% the cleanup.
  ets:new(?MODULE_TO_FAKE_MODULES, [named_table, set, protected, {keypos, 1}]),
  {ok, #{}}.

handle_call({load, Module}, {Pid, _}, State) ->
  Module = clj_utils:ets_save(?MODULE, Module),
  case clj_utils:ets_get(?PID_TO_MODULES, Pid) of
    ?NIL ->
      %% Monitor the process so we can cleanup when it dies
      erlang:monitor(process, Pid),
      clj_utils:ets_save(?PID_TO_MODULES, {Pid, [Module]});
    {Pid, Modules}  ->
      clj_utils:ets_save(?PID_TO_MODULES, {Pid, [Module | Modules]})
  end,
  {reply, ok, State};
handle_call({fetch, ModuleName}, _From, State) ->
  Module = clj_utils:ets_get(?MODULE, ModuleName),
  {reply, Module, State}.

handle_cast({fake_module, Module, FakeModule}, State) ->
  {_, FakeModules} = clj_utils:ets_get( ?MODULE_TO_FAKE_MODULES
                                      , Module
                                      , {Module, []}
                                      ),
  clj_utils:ets_save( ?MODULE_TO_FAKE_MODULES
                    , {Module, [FakeModule | FakeModules]}
                    ),

  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%% Cleanup modules when the process dies
handle_info({'DOWN', _MonitorRef, _Type, Pid, _Info}, State) ->
  {Pid, Modules} = clj_utils:ets_get(?PID_TO_MODULES, Pid, {Pid, []}),

  [cleanup(M#module.name) || M <- Modules],

  true = ets:delete(?PID_TO_MODULES, Pid),

  {noreply, State};
handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Msg, _State) ->
  ok.

code_change(_Msg, _From, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Helper Functions
%%------------------------------------------------------------------------------

-spec fetch_module(module()) -> ?NIL | clj_module().
fetch_module(ModuleName) ->
  gen_server:call(?MODULE, {fetch, ModuleName}).

-spec cleanup(module()) -> ok.
cleanup(Module) ->
  ets:delete(?MODULE, Module),
  {_, FakeModules} = clj_utils:ets_get( ?MODULE_TO_FAKE_MODULES
                                      , Module
                                      , {Module, []}
                                      ),
  [begin code:purge(M), code:delete(M) end || M <- FakeModules],
  ok.

%% @private
%% @doc
%% Loads the module `Name' into memory. This function assumes it is not
%% loaded already, so this check should be done before calling it.
%% The value of `Source' is used to set the `file' attribute of the module
%% if the module's binary is not found, which is interpreted as if the
%% module is new.
%% @end
-spec load(binary(), module()) -> clj_module().
load(Path, Name) when is_binary(Path) ->
  PathStr = binary_to_list(Path),
  Module = case code:ensure_loaded(Name) of
             {module, Name} ->
               new(PathStr, clj_utils:code_from_binary(Name));
             {error, _} ->
               new(PathStr, Name)
           end,
  ok = gen_server:call(?MODULE, {load, Module}),
  Module.

-spec is_loaded(module()) -> boolean().
is_loaded(Name) ->
  ets:member(?MODULE, Name).

%% @doc Processes a function's ast and modifies all calls to functions
%%      in the function's own module for a call to the fun returned by
%%      clj_module:fake_fun/3.
%% @end
-spec replace_calls( cerl:cerl() | [cerl:cerl()] | {cerl:cerl(), cerl:cerl()}
                   , module()
                   , {atom(), arity()} | undefined
                   ) ->
  cerl:cerl() | [cerl:cerl()] | {cerl:cerl(), cerl:cerl()}.
replace_calls( #c_call{ module = ModuleAst
                      , name   = FunctionAst
                      , args   = ArgsAsts
                      , anno   = Ann
                      }
             , CurrentModule
             , FA
             ) ->
  %% Only replace the call if the module is loaded. If it is not, then the
  %% replacement is happening for the evaluation of an expression where the
  %% called function hasn't been declared in the same evaluation.
  Module = cerl:concrete(ModuleAst),
  case is_loaded(Module) of
    true  ->
      fake_fun_call(Ann, CurrentModule, ModuleAst, FunctionAst, ArgsAsts);
    false ->
      ArgsAsts1 = replace_calls(ArgsAsts, CurrentModule, FA),
      cerl:ann_c_call(Ann, ModuleAst, FunctionAst, ArgsAsts1)
  end;
%% Detect non-remote calls done to other functions in the module, so we
%% can replace them with fake_funs when necessary.
replace_calls( #c_apply{ op   = #c_var{name = {_, _} = FA0} = FNameAst
                       , args = ArgsAsts
                       , anno = Ann
                       }
             , Module
             , FA1
             ) ->
  case FA0 =:= FA1 orelse lists:member(?LOCAL, Ann) of
    true ->
      ArgsAsts1 = replace_calls(ArgsAsts, Module, FA1),
      cerl:ann_c_apply(Ann, FNameAst, ArgsAsts1);
    false ->
      ModuleAst   = cerl:ann_c_atom(Ann, Module),
      FunctionAst = cerl:ann_c_atom(Ann, cerl:fname_id(FNameAst)),
      fake_fun_call(Ann, Module, ModuleAst, FunctionAst, ArgsAsts)
  end;
replace_calls(Ast, Module, FA) when is_tuple(Ast) ->
  list_to_tuple(replace_calls(tuple_to_list(Ast), Module, FA));
replace_calls(Asts, Module, FA) when is_list(Asts) ->
  [replace_calls(Item, Module, FA) || Item <- Asts];
replace_calls(Ast, _Module, _FA) when is_map(Ast) ->
  replace_vars(Ast);
replace_calls(Ast, _Module, _FA) ->
  Ast.

%% @doc Adds the fake_fun flag to literal vars
-spec replace_vars(T) -> T when T :: any().
replace_vars(#{?TYPE := 'clojerl.Var'} = Var0) ->
  'clojerl.Var':fake_fun(Var0, true);
replace_vars(Map) when is_map(Map) ->
  Fun = fun(K0, V0, Acc) ->
            {K1, V1} = replace_vars({K0, V0}),
            Acc#{K1 => V1}
        end,
  maps:fold(Fun, #{}, Map);
replace_vars(X) when is_tuple(X) ->
  list_to_tuple(replace_vars(tuple_to_list(X)));
replace_vars(X) when is_list(X) ->
  [replace_vars(Item) || Item <- X];
replace_vars(X) ->
  X.

-spec fake_fun_call( [term()]
                   , module()
                   , cerl:cerl()
                   , cerl:cerl()
                   , [cerl:cerl()]
                   ) -> cerl:cerl().
fake_fun_call(Ann, CurrentModule, ModuleAst, FunctionAst, ArgsAsts) ->
  Args1    = replace_calls(ArgsAsts, CurrentModule),
  Arity    = length(ArgsAsts),
  CallArgs = [ ModuleAst
             , FunctionAst
             , cerl:ann_c_int(Ann, Arity)
             ],
  CallAst  = cerl:ann_c_call( Ann
                            , cerl:c_atom(?MODULE)
                            , cerl:c_atom(fake_fun)
                            , CallArgs
                            ),
  VarAst   = clj_emitter:new_c_var(Ann),
  ApplyAst = cerl:ann_c_apply(Ann, VarAst, Args1),

  cerl:ann_c_let(Ann, [VarAst], CallAst, ApplyAst).

%% @private
-spec build_fake_fun(atom(), integer(), clj_module()) -> function().
build_fake_fun(Function, Arity, Module) ->
  FunctionAst = case clj_utils:ets_get(Module#module.funs, {Function, Arity}) of
                  {_, FunctionAst_} -> FunctionAst_;
                  undefined ->
                    throw({not_found, Module#module.name, Function, Arity})
                end,

  {FName, _} = Fun = replace_calls( FunctionAst
                                  , Module#module.name
                                  , {Function, Arity}
                                  ),
  Int = erlang:unique_integer([positive]),
  FakeModuleName = list_to_atom("fake_module_" ++ integer_to_list(Int)),

  {Names, Defs} = module_info_funs(FakeModuleName),
  ModuleName = cerl:c_atom(FakeModuleName),
  Exports    = [FName | Names],
  Clojure    = {cerl:c_atom(clojure), cerl:abstract([true])},

  FakeModule = cerl:c_module( ModuleName
                            , Exports
                            , [Clojure]
                            , [Fun | Defs]
                            ),

  try
    Bindings    = #{<<"#'clojure.core/*compile-files*">> => false},
    ok          = 'clojerl.Var':push_bindings(Bindings),
    CompileOpts = #{fake => true},
    clj_compiler:module(FakeModule, CompileOpts)
  after
    ok = 'clojerl.Var':pop_bindings()
  end,

  gen_server:cast(?MODULE, {fake_module, Module#module.name, FakeModuleName}),

  erlang:make_fun(FakeModuleName, Function, Arity).

%% @doc Deletes all fake_funs for Module:Function of all arities.
%%
%% This is used so that they can be replaced with new ones, when
%% redifining a function, for example.
%% @end
-spec delete_fake_fun(function_id(), clj_module()) -> ok.
delete_fake_fun(FunctionId, Module) ->
  true = ets:delete(Module#module.fake_funs, FunctionId),
  ok.

%% @private
-spec to_module(clj_module()) -> cerl:c_module().
to_module(#module{} = Module) ->
  #module{ name     = Name
         , source   = Source
         , mappings = MappingsTable
         , aliases  = AliasesTable
         , funs     = FunsTable
         , exports  = ExportsTable
         , on_load  = OnLoadTable
         , attrs    = AttrsTable
         } = Module,

  add_module_info_functions(Module),

  FileAttr     = {cerl:c_atom(file), cerl:abstract(Source)},

  MappingsList = ets:tab2list(MappingsTable),
  Mappings     = maps:from_list(MappingsList),
  MappingsAttr = {cerl:c_atom(mappings), cerl:abstract([Mappings])},

  AliasesList  = ets:tab2list(AliasesTable),
  Aliases      = maps:from_list(AliasesList),
  AliasesAttr  = {cerl:c_atom(aliases), cerl:abstract([Aliases])},

  Exports      = [cerl:c_fname(FName, Arity)
                  || {{FName, Arity}} <- ets:tab2list(ExportsTable)
                 ],

  ClojureAttr  = {cerl:c_atom(clojure), cerl:abstract([true])},

  NameSym      = clj_rt:symbol(atom_to_binary(Name, utf8)),
  Meta         = case 'clojerl.Namespace':find(NameSym) of
                   ?NIL -> ?NIL;
                   Ns   -> 'clojerl.Namespace':meta(Ns)
                 end,
  MetaAttr     = {cerl:c_atom(meta), cerl:abstract([Meta])},

  Attrs        = [X || {X} <- ets:tab2list(AttrsTable)],
  UniqueAttrs  = lists:usort([ClojureAttr, MetaAttr | Attrs]),

  AllAttrs     = [FileAttr, MappingsAttr, AliasesAttr | UniqueAttrs],

  Defs         = [X || {_, X} <- ets:tab2list(FunsTable)],

  maybe_on_load(OnLoadTable, cerl:c_atom(Name), Exports, AllAttrs, Defs).

add_module_info_functions(Module) ->
  {_, Funs} = module_info_funs(Module#module.name),
  add_functions(Funs, Module),
  add_exports([{module_info, 0}, {module_info, 1}], Module).

-spec module_info_funs(module()) ->
  {[cerl:cerl()], [{cerl:cerl(), cerl:cerl()}]}.
module_info_funs(Name) ->
  InfoName0 = cerl:c_fname(?MODULE_INFO, 0),
  InfoFun0  = cerl:c_fun( []
                        , cerl:c_call( cerl:c_atom(erlang)
                                     , cerl:c_atom(get_module_info)
                                     , [cerl:c_atom(Name)]
                                     )
                        ),

  InfoName1 = cerl:c_fname(?MODULE_INFO, 1),
  Arg       = cerl:c_var(x),
  InfoFun1  = cerl:c_fun( [Arg]
                        , cerl:c_call( cerl:c_atom(erlang)
                                     , cerl:c_atom(get_module_info)
                                     , [cerl:c_atom(Name), Arg]
                                     )
                        ),

  { [InfoName0, InfoName1]
  , [ {InfoName0, InfoFun0}
    , {InfoName1, InfoFun1}
    ]
  }.

%% @doc Only add an on_load function if there are any expressions to be added.
-spec maybe_on_load( ets:tid()
                   , cerl:c_atom()
                   , [cerl:c_var()]
                   , [{cerl:c_atom(), cerl:cerl()}]
                   , [cerl:c_fun()]
                   ) ->
  cerl:c_module().
maybe_on_load(OnLoadTable, Name, Exports0, Attrs0, Defs0) ->
  case ets:info(OnLoadTable, size) of
    0 ->
      ?DEBUG({no_on_load, cerl:atom_val(Name)}),
      cerl:c_module(Name, Exports0, Attrs0, Defs0);
    _ ->
      Attr   = { cerl:c_atom(on_load)
               , cerl:abstract([{?ON_LOAD_FUNCTION, 0}])
               },

      FName  = cerl:c_fname(?ON_LOAD_FUNCTION, 0),
      Fun    = on_load_function(OnLoadTable),
      Def    = {FName, Fun},

      Attrs1   = lists:usort([Attr | Attrs0]),
      Defs1    = [Def  | Defs0],

      cerl:c_module(Name, Exports0, Attrs1, Defs1)
  end.

%% @private
-spec on_load_function(ets:tid()) -> empty | cerl:cerl().
on_load_function(OnLoadTable) ->
  [Head | Tail] = [Expr || {_, Expr} <- ets:tab2list(OnLoadTable)],
  SeqFun = fun(X, Acc) -> cerl:c_seq(Acc, X) end,
  Body   = lists:foldl(SeqFun, Head, Tail),
  cerl:c_fun([], cerl:c_seq(Body, cerl:c_atom(ok))).

-spec new(string(), atom() | cerl:c_module()) -> clj_module().
new(Path, Name) when is_atom(Name), is_list(Path) ->
  FileAttr = {cerl:c_atom(file), cerl:abstract(Path)},
  new(cerl:c_module(cerl:c_atom(Name), [], [FileAttr], []));
new(Path, #c_module{attrs = Attrs} = CoreModule) when is_list(Path) ->
  FileAttr = {cerl:c_atom(file), cerl:abstract(Path)},
  new(CoreModule#c_module{attrs = [FileAttr | Attrs]}).

-spec new(cerl:cerl()) -> clj_module().
new(CoreModule) ->
  Name     = cerl:concrete(cerl:module_name(CoreModule)),
  Exports  = [ {cerl:fname_id(E), cerl:fname_arity(E)}
               || E <- cerl:module_exports(CoreModule)
             ],
  AllAttrs = cerl:module_attrs(CoreModule),
  Funs     = cerl:module_defs(CoreModule),

  {Attrs, Extracted} = extract_attrs(AllAttrs, [mappings, aliases, file]),

  Mappings = case maps:get(mappings, Extracted, #{}) of
               [V] -> V;
               V -> V
             end,
  Aliases  = case maps:get(aliases, Extracted, #{}) of
               [X] -> X;
               X -> X
             end,
  Path     = maps:get(file, Extracted, ""),

  %% Tables need to be public so that other compiler processes can modify them.
  TableOpts = [set, public, {keypos, 1}],
  Module = #module{ name         = Name
                  , source       = Path
                  , mappings     = ets:new(mappings, TableOpts)
                  , aliases      = ets:new(aliases, TableOpts)
                  , funs         = ets:new(funs, TableOpts)
                  , fake_funs    = ets:new(fake_funs, TableOpts)
                  , exports      = ets:new(exports, TableOpts)
                  , on_load      = ets:new(on_load, TableOpts)
                  , attrs        = ets:new(attributes, TableOpts)
                  },

  Module = add_functions(Funs, Module),
  Module = add_mappings(maps:to_list(Mappings), Module),
  [add_alias(A, Ns, Module) || {A, Ns} <- maps:to_list(Aliases)],
  Module = add_attributes(Attrs, Module),

  %% Keep expressions from the on_load function.
  %% IMPORTANT: This means that for wiping them all out, the namespace
  %% needs to be compiled from scratch.
  OnLoadId = {?ON_LOAD_FUNCTION, 0},
  case clj_utils:ets_get(Module#module.funs, OnLoadId) of
    ?NIL ->
      ok;
    {OnLoadId, {_OnLoadName, OnLoadFun}} ->
      Body = cerl:fun_body(OnLoadFun),
      add_on_load(Body, Module),
      true = ets:delete(Module#module.funs, OnLoadId)
  end,

  add_exports(Exports, Module).

-spec function_id({cerl:cerl(), cerl:cerl()}) -> function_id().
function_id({Name, _}) ->
  {cerl:fname_id(Name), cerl:fname_arity(Name)}.

-spec extract_attrs([{cerl:c_atom(), cerl:cerl()}], [atom()]) ->
  {[{cerl:c_atom(), cerl:cerl()}], map()}.
extract_attrs(Attrs, Names) ->
  do_extract_attrs(Attrs, Names, [], #{}).

do_extract_attrs([], _Names, NewAttrs, Extracted) ->
  {NewAttrs, Extracted};
do_extract_attrs( [{NameAbst, ValAbst} = Attr | Attrs]
                , Names
                , NewAttrs
                , Extracted
                ) ->
  Name = cerl:concrete(NameAbst),
  case lists:member(Name, Names) of
    true  ->
      Val = cerl:concrete(ValAbst),
      do_extract_attrs(Attrs, Names, NewAttrs, Extracted#{Name => Val});
    false ->
      do_extract_attrs(Attrs, Names, [Attr | NewAttrs], Extracted)
  end.
