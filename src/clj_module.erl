-module(clj_module).

-behavior(gen_server).

-compile({no_auto_import, [get/1]}).

-include_lib("compiler/src/core_parse.hrl").

-export([ with_context/1

        , all_forms/0
        , get_forms/1
        , ensure_loaded/2
        , is_loaded/1
        , remove/1

        , fake_fun/3
        , replace_calls/2

        , add_vars/2
        , add_attributes/2
        , add_exports/2
        , add_functions/2
        , add_on_load/2

        , is_clojure/1
        , is_protocol/1

        , module_info_funs/1
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
                  %% ETS table where vars are kept. The key is the var's
                  %% name as a binary.
                  vars              :: ets:tid(),
                  %% ETS table where functions are kept. The key is the
                  %% function's name and arity.
                  funs              :: ets:tid(),
                  %% ETS table where fake functions are kept. The key is the
                  %% funs value (i.e. Module:Function/Arity).
                  fake_funs         :: ets:tid(),
                  %% ETS table where fake modules are kept. The key is the
                  %% modules name.
                  fake_modules      :: ets:tid(),
                  %% ETS table where function exports are kept. The key is the
                  %% function's name and arity.
                  exports           :: ets:tid(),
                  %% ETS table where expressions that will be included in the
                  %% on_load function are kept. The key is the expression
                  %% itself.
                  on_load           :: ets:tid(),
                  %% ETS table where attributes that are kept.
                  attrs             :: ets:tid(),
                  rest              :: [erl_parse:abstract_form()]
                }).

-type clj_module() :: #module{}.

-export_type([clj_module/0]).

-define(ON_LOAD_FUNCTION, '$_clj_on_load').
-define(MODULE_INFO, 'module_info').

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------

-spec with_context(fun()) -> ok.
with_context(Fun) ->
  try
    Fun()
  after
    cleanup()
  end.

%% @doc Returns a list where each element is a list with the abstract
%%      forms of all stored modules.
%% @end
-spec all_forms() -> [cerl:c_module()].
all_forms() ->
  All = gen_server:call(?MODULE, all),
  lists:map(fun to_forms/1, All).

-spec get_forms(atom()) -> cerl:c_module().
get_forms(ModuleName) when is_atom(ModuleName) ->
  to_forms(get(?MODULE, ModuleName)).

%% @doc Makes sure the clj_module is loaded.
-spec ensure_loaded(atom(), binary()) -> ok.
ensure_loaded(Name, Source) ->
  case is_loaded(Name) of
    true  -> ok;
    false -> load(Name, Source), ok
  end.

%% @doc Remove the module from the loaded modules in clj_module.
-spec remove(module()) -> ok.
remove(Module) when is_atom(Module)->
  gen_server:cast(?MODULE, {remove, self(), Module}).

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
  case get(?MODULE, ModuleName) of
    undefined ->
      fun ModuleName:Function/Arity;
    Module ->
      FA     = {Function, Arity},
      case ets:lookup(Module#module.fake_funs, FA) of
        [] ->
          Fun = build_fake_fun(Module, Function, Arity),
          save(Module#module.fake_funs, {FA, Fun}),
          Fun;
        [{_, Fun}] ->
          Fun
      end
  end.

%% @doc Processes a function's ast and modifies all calls to functions
%%      in the function's own module for a call to the fun returned by
%%      clj_module:fake_fun/3.
%% @end
-spec replace_calls(cerl:cerl(), module()) -> cerl:cerl().
replace_calls( #c_call{ module = ModuleAst
                      , name   = FunctionAst
                      , args   = ArgsAsts
                      , anno   = Anno
                      }
             , CurrentModule
             ) ->
  %% Only replace the call if the module is loaded. If it is not, then the
  %% replacement is happening for the evaluation of an expression where the
  %% called function hasn't been declared in the same evaluation.
  Module = cerl:concrete(ModuleAst),
  case is_loaded(Module) of
    true  ->
      fake_fun_call(Anno, CurrentModule, ModuleAst, FunctionAst, ArgsAsts);
    false ->
      ArgsAsts1 = replace_calls(ArgsAsts, CurrentModule),
      cerl:ann_c_call(Anno, ModuleAst, FunctionAst, ArgsAsts1)
  end;
%% Detect non-remote calls done to other functions in the module, so we
%% can replace them with fake_funs when necessary.
replace_calls(#c_apply{ op   = #c_var{name = {_, _}} = FNameAst
                      , args = ArgsAsts
                      , anno = Anno
                      }
             , Module) ->
  case lists:member(local, Anno) of
    true ->
      ArgsAsts1 = replace_calls(ArgsAsts, Module),
      cerl:ann_c_apply(Anno, FNameAst, ArgsAsts1);
    false ->
      ModuleAst   = cerl:ann_c_atom(Anno, Module),
      FunctionAst = cerl:ann_c_atom(Anno, cerl:fname_id(FNameAst)),
      fake_fun_call(Anno, Module, ModuleAst, FunctionAst, ArgsAsts)
  end;
replace_calls(Ast, Module) when is_tuple(Ast) ->
  list_to_tuple(replace_calls(tuple_to_list(Ast), Module));
replace_calls(Asts, Module) when is_list(Asts) ->
  [replace_calls(Item, Module) || Item <- Asts];
replace_calls(Ast, _) ->
  Ast.

-spec fake_fun_call( cerl:anno()
                   , module()
                   , cerl:cerl()
                   , cerl:cerl()
                   , [cerl:cerl()]
                   ) -> cerl:cerl().
fake_fun_call(Anno, CurrentModule, ModuleAst, FunctionAst, ArgsAsts) ->
  Args1    = replace_calls(ArgsAsts, CurrentModule),
  Arity    = length(ArgsAsts),
  CallArgs = [ ModuleAst
             , FunctionAst
             , cerl:ann_c_int(Anno, Arity)
             ],
  CallAst  = cerl:ann_c_call( Anno
                            , cerl:c_atom(?MODULE)
                            , cerl:c_atom(fake_fun)
                            , CallArgs
                            ),
  VarAst   = new_c_var(Anno, "f"),
  ApplyAst = cerl:ann_c_apply(Anno, VarAst, Args1),

  cerl:ann_c_let(Anno, [VarAst], CallAst, ApplyAst).

-spec new_c_var(cerl:ann(), string()) -> cerl:c_var().
new_c_var(Anno, Prefix) ->
  N = erlang:unique_integer([positive]),
  Name = list_to_atom(Prefix ++ integer_to_list(N)),
  cerl:ann_c_var(Anno, Name).

-spec add_vars(module() | clj_module(), ['clojerl.Var':type()]) -> clj_module().
add_vars(ModuleName, Vars) when is_atom(ModuleName)  ->
  add_vars(get(?MODULE, ModuleName), Vars);
add_vars(Module, Vars) ->
  AddFun = fun(V) ->
               K = clj_core:name(V),
               save(Module#module.vars, {K, V})
           end,
  lists:foreach(AddFun, Vars),
  Module.

-spec add_attributes(clj_module(), [erl_parse:abstract_form()]) -> clj_module().
add_attributes(ModuleName, Attrs) when is_atom(ModuleName)  ->
  add_attributes(get(?MODULE, ModuleName), Attrs);
add_attributes(Module, []) ->
  Module;
add_attributes(Module, Attrs) ->
  AddAttr = fun(E) -> save(Module#module.attrs, {E}) end,
  ok = lists:foreach(AddAttr, Attrs),
  Module.

-spec add_exports(clj_module(), [{atom(), non_neg_integer()}]) ->
  clj_module().
add_exports(ModuleName, Exports) when is_atom(ModuleName)  ->
  add_exports(get(?MODULE, ModuleName), Exports);
add_exports(Module, Exports) ->
  AddExport = fun(E) ->
                  save(Module#module.exports, {E})
              end,
  ok = lists:foreach(AddExport, Exports),
  Module.

-spec add_functions(module() | clj_module(), [erl_parse:abstract_form()]) ->
  clj_module().
add_functions(ModuleName, Funs) when is_atom(ModuleName)  ->
  add_functions(get(?MODULE, ModuleName), Funs);
add_functions(Module, Funs) ->
  SaveFun = fun(F) ->
                FunctionId  = function_id(F),
                ok          = delete_fake_fun(Module, FunctionId),
                save(Module#module.funs, {FunctionId, F})
            end,
  lists:foreach(SaveFun, Funs),
  Module.

-spec add_on_load(module() | clj_module(), erl_parse:abstract_expr()) ->
  clj_module().
add_on_load(ModuleName, Expr) when is_atom(ModuleName) ->
  add_on_load(get(?MODULE, ModuleName), Expr);
add_on_load(Module, Expr) ->
  save(Module#module.on_load, {Expr, Expr}),
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

-spec is_protocol(module()) -> boolean().
is_protocol(Name) ->
  Key = {?MODULE, is_protocol, Name},
  case clj_cache:get(Key) of
    undefined ->
      Attrs = Name:module_info(attributes),
      IsProtocol = lists:keymember(protocol, 1, Attrs),
      clj_cache:put(Key, IsProtocol),
      IsProtocol;
    {ok, Value} ->
      Value
  end.

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(?MODULE, [named_table, set, protected, {keypos, 2}]),
  TabId = ets:new(loaded_modules, [set, protected, {keypos, 1}]),
  {ok, #{loaded_modules => TabId}}.

handle_call({load, Module}, {Pid, _}, #{loaded_modules := TabId} = State) ->
  Module = save(?MODULE, Module),
  case get(TabId, Pid) of
    undefined ->
      save(TabId, {Pid, [Module]});
    {Pid, Modules}  ->
      save(TabId, {Pid, [Module | Modules]})
  end,

  {reply, ok, State};
handle_call(cleanup, {Pid, _}, #{loaded_modules := TabId} = State) ->
  Modules = case get(TabId, Pid) of
              undefined   -> [];
              {Pid, Mods} -> Mods
            end,

  true = ets:delete(TabId, Pid),
  ok   = lists:foreach(fun(M) -> ets:delete(?MODULE, M#module.name) end, Modules),

  {reply, Modules, State};
handle_call(all, {Pid, _}, #{loaded_modules := TabId} = State) ->
  Modules = case get(TabId, Pid) of
              undefined   -> [];
              {Pid, Mods} -> Mods
            end,

  {reply, Modules, State}.

handle_cast({remove, Pid, ModuleName}, #{loaded_modules := TabId} = State) ->
  true = ets:delete(?MODULE, ModuleName),
  case get(TabId, Pid) of
    {Pid, Modules} ->
      NewModules = [M || M <- Modules, M#module.name =/= ModuleName],
      save(TabId, {Pid, NewModules});
    undefined   -> ok
  end,
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Msg, _State) ->
  ok.

code_change(_Msg, _From, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Helper Functions
%%------------------------------------------------------------------------------

%% @private
-spec cleanup() -> ok.
cleanup() ->
  Modules = gen_server:call(?MODULE, cleanup),
  lists:foreach(fun delete_fake_modules/1, Modules).

%% @private
%% @doc
%% Loads the module `Name' into memory. This function assumes it is not
%% loaded already, so this check should be done before calling it.
%% The value of `Source' is used to set the `file' attribute of the module
%% if the module's binary is not found, which is interpreted as if the
%% module is new.
%% @end
-spec load(atom(), binary()) -> ok | {error, term()}.
load(Name, Source) when is_binary(Source) ->
  SourceStr = binary_to_list(Source),
  Module = case code:ensure_loaded(Name) of
             {module, Name} ->
               new(clj_utils:code_from_binary(Name), SourceStr);
             {error, _} ->
               new(Name, SourceStr)
           end,
  ok = gen_server:call(?MODULE, {load, Module}),
  Module.

-spec is_loaded(module()) -> boolean().
is_loaded(Name) ->
  ets:member(?MODULE, Name).

%% @private
-spec build_fake_fun(clj_module(), atom(), integer()) -> function().
build_fake_fun(Module, Function, Arity) ->
  {_, FunctionAst} = get(Module#module.funs, {Function, Arity}),

  {FName, _} = Fun = replace_calls(FunctionAst, Module#module.name),
  Int = erlang:unique_integer([positive]),
  FakeModuleName = list_to_atom("fake_module_" ++ integer_to_list(Int)),

  {Names, Defs} = module_info_funs(FakeModuleName),
  ModuleName = cerl:c_atom(FakeModuleName),
  Exports    = [FName | Names],
  Clojure    = {cerl:c_atom(clojure), cerl:c_atom(true)},

  FakeModule = cerl:c_module( ModuleName
                            , Exports
                            , [Clojure]
                            , [Fun | Defs]
                            ),

  %% io:format("======== Fake ~p:~p =======~n", [Module#module.name, Function]),
  %% io:format("~s~n", [core_pp:format(FakeModule)]),

  CompileOpts = #{erl_flags => [from_core, binary]},
  clj_compiler:compile_forms(FakeModule, CompileOpts),

  save(Module#module.fake_modules, {FakeModuleName}),

  erlang:make_fun(FakeModuleName, Function, Arity).

%% @doc Deletes all fake_funs for Module:Function of all arities.
%%
%% This is used so that they can be replaced with new ones, when
%% redifining a function, for example.
%% @end
-spec delete_fake_fun(clj_module(), function_id()) -> ok.
delete_fake_fun(Module, FunctionId) ->
  true = ets:delete(Module#module.fake_funs, FunctionId),
  ok.

%% @private
-spec delete_fake_modules(clj_module()) -> ok.
delete_fake_modules(Module) ->
  FakeModulesId = Module#module.fake_modules,
  [code:delete(Name) || {Name} <- ets:tab2list(FakeModulesId)],
  ok.

%% @private
-spec to_forms(clj_module()) -> [erl_parse:abstract_form()].
to_forms(#module{} = Module) ->
  #module{ name    = Name
         , source  = Source
         , vars    = VarsTable
         , funs    = FunsTable
         , exports = ExportsTable
         , on_load = OnLoadTable
         , attrs   = AttrsTable
         %% , rest    = Rest
         } = Module,

  add_module_info_functions(Module),

  FileAttr    = {cerl:c_atom(file), cerl:abstract(Source)},

  VarsList    = [{clj_core:name(X), X} || {_, X} <- ets:tab2list(VarsTable)],
  Vars        = maps:from_list(VarsList),
  VarsAttr    = {cerl:c_atom(vars), cerl:abstract([Vars])},

  Exports     = [cerl:c_fname(FName, Arity)
                 || {{FName, Arity}} <- ets:tab2list(ExportsTable)
                ],

  ClojureAttr = {cerl:c_atom(clojure), cerl:abstract(true)},
  OnLoadAttr  = {cerl:c_atom(on_load), cerl:abstract([{?ON_LOAD_FUNCTION, 0}])},

  Attrs       = [X || {X} <- ets:tab2list(AttrsTable)],
  UniqueAttrs = lists:usort([ClojureAttr, OnLoadAttr | Attrs]),

  AllAttrs    = [FileAttr, VarsAttr | UniqueAttrs],

  OnLoadName  = cerl:c_fname(?ON_LOAD_FUNCTION, 0),
  OnLoadFun   = on_load_function(OnLoadTable),
  Funs        = [X || {_, X} <- ets:tab2list(FunsTable)],
  Defs        = [{OnLoadName, OnLoadFun} | Funs],

  cerl:c_module(cerl:c_atom(Name), Exports, AllAttrs, Defs).

add_module_info_functions(Module) ->
  {_, Funs} = module_info_funs(Module#module.name),
  add_functions(Module, Funs),
  add_exports(Module, [{module_info, 0}, {module_info, 1}]).

-spec module_info_funs(module()) -> {[cerl:cerl()], [cerl:cerl()]}.
module_info_funs(Name) ->
  InfoName0 = cerl:c_fname(?MODULE_INFO, 0),
  InfoFun0  = cerl:c_fun( []
                        , cerl:c_call( cerl:c_atom(erlang)
                                     , cerl:c_atom(get_module_info)
                                     , [cerl:c_atom(Name)]
                                     )
                        ),

  InfoName1 = cerl:c_fname(?MODULE_INFO, 1),
  Arg             = cerl:c_var(x),
  InfoFun1  = cerl:c_fun( [Arg]
                        , cerl:c_call( cerl:c_atom(erlang)
                                     , cerl:c_atom(get_module_info)
                                     , [cerl:c_atom(Name), Arg]
                                     )
                        ),

  { [InfoName0, InfoName1]
  , [ {InfoName0, InfoFun0}
    , {InfoName1, InfoFun1}
    ]}.

%% @private
on_load_function(OnLoadTable) ->
  Body = case [Expr || {_, Expr} <- ets:tab2list(OnLoadTable)] of
           []       -> cerl:c_atom(ok);
           [Head | Tail]  ->
             SeqFun = fun(X, Acc) -> cerl:c_seq(Acc, X) end,
             lists:foldl(SeqFun, Head, Tail)
         end,
  cerl:c_fun([], Body).

-spec get(atom(), module()) -> any().
get(Table, Id) ->
  get(Table, Id, false).

-spec get(atom(), module(), boolean()) -> any().
get(undefined, Id, _) -> %% If there is no table then nothing will be found.
  throw({no_table, Id});
get(Table, Id, Throw) ->
  case ets:lookup(Table, Id) of
    [] when Throw -> throw({not_found, Id});
    []      -> undefined;
    [Value] -> Value
  end.

-spec save(atom(), term()) -> term().
save(Table, Value) ->
  true = ets:insert(Table, Value),
  Value.

-spec new(atom(), string()) -> ok | {error, term()}.
new(Name, Source) when is_atom(Name), is_list(Source) ->
  FileAttr = {cerl:c_atom(file), cerl:abstract(Source)},
  new(cerl:c_module(cerl:c_atom(Name), [], [FileAttr], []));
new(#c_module{attrs = Attrs} = CoreModule, Source) when is_list(Source) ->
  FileAttr = {cerl:c_atom(file), cerl:abstract(Source)},
  new(CoreModule#c_module{attrs = [FileAttr | Attrs]}).

-spec new(cerl:cerl()) -> ok | {error, term()}.
new(CoreModule) ->
  Name     = cerl:concrete(cerl:module_name(CoreModule)),
  Exports  = [ {cerl:fname_id(E), cerl:fname_arity(E)}
               || E <- cerl:module_exports(CoreModule)
             ],
  AllAttrs = cerl:module_attrs(CoreModule),
  Funs     = cerl:module_defs(CoreModule),

  {Attrs, Extracted} = extract_attrs(AllAttrs, [vars, file]),

  Vars   = case maps:get(vars, Extracted, #{}) of
             [V] -> V;
             V -> V
           end,
  Source = maps:get(file, Extracted, ""),

  %% Tables need to be public so that other compiler processes can modify them.
  TableOpts = [set, public, {keypos, 1}],
  Module = #module{ name              = Name
                  , source            = Source
                  , vars              = ets:new(var, TableOpts)
                  , funs              = ets:new(funs, TableOpts)
                  , fake_funs         = ets:new(fake_funs, TableOpts)
                  , fake_modules      = ets:new(fake_modules, TableOpts)
                  , exports           = ets:new(exports, TableOpts)
                  , on_load           = ets:new(on_load, TableOpts)
                  , attrs             = ets:new(attributes, TableOpts)
                  },

  Module = add_functions(Module, Funs),
  Module = add_vars(Module, maps:values(Vars)),
  Module = add_attributes(Module, Attrs),

  %% Remove the on_load function and all its contents.
  %% IMPORTANT: This means that whenever a namespace is recompiled all
  %% `on-load*' expressions need to be included in the compilation as
  %% well or they won't be present in the resulting binary.
  OnLoadId = {?ON_LOAD_FUNCTION, 0},
  true     = ets:delete(Module#module.funs, OnLoadId),

  add_exports(Module, Exports).

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
