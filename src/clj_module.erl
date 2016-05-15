-module(clj_module).

-behavior(gen_server).

-compile({no_auto_import, [get/1]}).

-export([ with_context/1

        , all_forms/0
        , ensure_loaded/2
        , is_loaded/1

        , fake_fun/3
        , replace_calls/3

        , add_vars/2
        , add_attributes/2
        , add_exports/2
        , add_functions/2
        , add_on_load/2

        , is_clojure/1
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
                  vars              :: ets:tid(),
                  funs              :: ets:tid(),
                  fake_funs         :: ets:tid(),
                  fake_modules      :: ets:tid(),
                  exports           :: ets:tid(),
                  on_load           :: ets:tid(),
                  attrs             :: [erl_parse:abstract_form()],
                  rest              :: [erl_parse:abstract_form()]
                }).

-type clj_module() :: #module{}.

-export_type([clj_module/0]).

-define(ON_LOAD_FUNCTION, '$_clj_on_load').

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

%% @private
-spec cleanup() -> ok.
cleanup() ->
  Modules = gen_server:call(?MODULE, cleanup),
  lists:foreach(fun delete_fake_modules/1, Modules).

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
  Module = get(?MODULE, ModuleName, true),
  FA     = {Function, Arity},
  case ets:lookup(Module#module.fake_funs, FA) of
    [] ->
      Fun = build_fake_fun(Module, Function, Arity),
      save(Module#module.fake_funs, {FA, Fun}),
      Fun;
    [{_, Fun}] ->
      Fun
  end.

%% @private
-spec build_fake_fun(clj_module(), atom(), integer()) -> function().
build_fake_fun(Module, Function, Arity) ->
  {_, FunctionAst} = get(Module#module.funs, {Function, Arity}),

  {function, _, _, _, Clauses} =
    replace_calls(FunctionAst, Module#module.name, Function),
  Int = erlang:unique_integer([positive]),
  FakeModuleName = list_to_atom("fake_module_" ++ integer_to_list(Int)),

  ModuleAst  = {attribute, 0, module, FakeModuleName},
  ExportAst  = {attribute, 0, export, [{Function, Arity}]},
  ClojureAst = {attribute, 0, clojure, true},
  FunAst     = {function, 0, Function, Arity, Clauses},
  Forms      = [ModuleAst, ExportAst, ClojureAst, FunAst],

  Binary = case compile:forms(Forms, []) of
             {ok, _, Bin} -> Bin;
             Error -> throw({Error, Module#module.name, Function, Arity})
           end,
  code:load_binary(FakeModuleName, "", Binary),

  save(Module#module.fake_modules, {FakeModuleName}),

  Fun = erlang:make_fun(FakeModuleName, Function, Arity),
  check_var_val(Function, Arity, Fun).

%% @doc Deletes all fake_funs for Module:Function of all arities.
%%
%% This is used so that they can be replaced with new ones, when
%% redifining a function, for example.
%% @end
-spec delete_fake_fun(clj_module(), function_id()) -> ok.
delete_fake_fun(Module, FunctionId) ->
  true = ets:delete(Module#module.fake_funs, FunctionId),
  ok.

%% @doc Checks if the function is the one that provides the value of a var.
%%
%% Then it checks if the value returned is the var itself and
%% if so replaces the returned value for a clojerl.FakeVar, which
%% reimplements clojerl.IFn protocol so that the invoke is done using
%% the var's corresponding fake fun.
%% @end
-spec check_var_val(atom(), integer(), function()) -> function().
check_var_val(Function, 0, Fun) ->
  FunctionBin = atom_to_binary(Function, utf8),
  case clj_utils:ends_with(FunctionBin, <<"__val">>) of
    true ->
      Value = Fun(),
      FakeValFun = fun() -> 'clojerl.FakeVar':new(Value) end,
      case clj_core:'var?'(Value) of
        true  -> FakeValFun;
        false -> Fun
      end;
    false -> Fun
  end;
check_var_val(_Function, _Arity, Fun) ->
  Fun.

%% @doc Processes a function's ast and modifies all calls to functions
%%      in the function's own module for a call to the fun returned by
%%      clj_module:fake_fun/3.
%%
%% If the call is a recursive call to the top function then it changes
%% the call to a variable built using the value of TopFunction.
%% @end
-spec replace_calls(erl_parse:abstract_form(), module(), atom()) ->
  erl_parse:abstract_form().
replace_calls( { call, Line
               , { remote, _
                 , {atom, _, Module}
                 , {atom, _, Function}
                 } = RemoteOriginal
               , Args
               }
             , CurrentModule
             , TopFunction) when CurrentModule =:= Module;
                                 CurrentModule =:= '_' ->

  Args1 = replace_calls(Args, CurrentModule, TopFunction),
  Arity = length(Args),
  %% Only replace the call if the module is loaded. If it is not, then the
  %% replacement is happening for the evaluation of an expression where the
  %% called function hasn't been declared in the same evaluation.
  case is_loaded(Module) of
    true  ->
      Remote = {remote, Line,
                {atom, Line, ?MODULE},
                {atom, Line, fake_fun}
               },

      FunCall = { call, Line, Remote
                , [ {atom, Line, Module}
                  , {atom, Line, Function}
                  , {integer, Line, Arity}
                  ]
                },
      {call, Line, FunCall, Args1};
    false ->
      {call, Line, RemoteOriginal, Args1}
  end;
%% Detect non-remote calls done to other functions in the module, so we
%% can replace them with fake_funs when necessary.
replace_calls( {call, Line, {atom, Line2, Function}, Args}
             , ModuleName
             , TopFunction) ->
  Arity = length(Args),
  Args1 = replace_calls(Args, ModuleName, TopFunction),
  case find_fun(ModuleName, Function, Arity) of
    %% Since the module is not loaded just make a remote call.
    %% This can happen with functions such as clojure.core/in-ns.
    module_not_loaded ->
      Remote = {remote, Line,
                {atom, Line, ModuleName},
                {atom, Line, Function}
               },

      {call, Line, Remote, Args1};
    %% This shouldn't happen, all calls to functions in the module should
    %% have been resolved by now in the analyzer.
    undefined ->
      throw({undef, {ModuleName, Function, Arity}});
    %% The function is in fact in the module so we need to get the
    %% fake_fun for it.
    _F ->

      Remote = {remote, Line,
                {atom, Line, ?MODULE},
                {atom, Line, fake_fun}
               },
      FunCall = { call, Line, Remote
                , [ {atom, Line2, ModuleName}
                  , {atom, Line2, Function}
                  , {integer, Line2, Arity}
                  ]
                },
      {call, Line, FunCall, Args1}
  end;
replace_calls(Ast, Module, TopFunction) when is_tuple(Ast) ->
  list_to_tuple(replace_calls(tuple_to_list(Ast), Module, TopFunction));
replace_calls(Ast, Module, TopFunction) when is_list(Ast) ->
  [replace_calls(Item, Module, TopFunction) || Item <- Ast];
replace_calls(Ast, _, _) ->
  Ast.

%% @private
%% @doc Find a function that may be loaded in the specified module.
%%
%% Returns the abstract form of the function if found, `not_found' if it
%% is not found and `module_not_loaded' when the module is not loaded.
-spec find_fun(module(), atom(), non_neg_integer()) ->
  module_not_loaded | not_found | erl_parse:abstract_form().
find_fun(ModuleName, Function, Arity) ->
  case get(?MODULE, ModuleName) of
    undefined -> module_not_loaded;
    Module ->
      case ets:lookup(Module#module.funs, {Function, Arity}) of
        [] -> not_found;
        [{_, F}] -> F
      end
  end.

%% @doc Makes sure the clj_module is loaded.
-spec ensure_loaded(atom(), string()) -> ok.
ensure_loaded(Name, Source) ->
  case is_loaded(Name) of
    true  -> ok;
    false -> load(Name, Source), ok
  end.

%% @private
-spec load(atom(), string()) -> ok | {error, term()}.
load(Name, Source) ->
  Module = case code:ensure_loaded(Name) of
             {module, Name} ->
               new(clj_utils:code_from_binary(Name));
             {error, _} ->
               new(Name, Source)
           end,
  ok = gen_server:call(?MODULE, {load, Module}),
  Module.

-spec is_loaded(module()) -> boolean().
is_loaded(Name) ->
  ets:member(?MODULE, Name).

%% @doc Returns a list with all modules that where loaded by the
%%      current process.
%% @private
-spec all() -> [clj_module()].
all() ->
  gen_server:call(?MODULE, all).

%% @doc Returns a list where each element is a list with the abstract
%%      forms of all stored modules.
%% @end
-spec all_forms() -> [[erl_parse:abstract_form()]].
all_forms() ->
  lists:map(fun to_forms/1, all()).

%% @private
-spec to_forms(clj_module()) -> [erl_parse:abstract_form()].
to_forms(#module{} = Module) ->
  #module{ name    = Name
         , source  = Source
         , vars    = VarsTable
         , funs    = FunsTable
         , exports = ExportsTable
         , on_load = OnLoadTable
         , attrs   = Attrs
         , rest    = Rest
         } = Module,

  ModuleAttr  = {attribute, 0, module, Name},
  FileAttr    = {attribute, 0, file, {Source, 0}},
  %% To avoid conflicts with Clojure functions with the same name
  %% as some functions in the `erlang' module.
  CompileAttr = {attribute, 0, compile, [no_auto_import]},

  VarsList    = [{clj_core:name(X), X} || {_, X} <- ets:tab2list(VarsTable)],
  Vars        = maps:from_list(VarsList),
  VarsAttr    = {attribute, 0, vars, Vars},

  Exports     = [X || {X} <- ets:tab2list(ExportsTable)],
  ExportAttr  = {attribute, 0, export, Exports},

  ClojureAttr = {attribute, 0, clojure, true},
  OnLoadAttr  = {attribute, 0, on_load, {?ON_LOAD_FUNCTION, 0}},

  UniqueAttrs = lists:usort([ClojureAttr, OnLoadAttr, CompileAttr | Attrs]),

  OnLoadFun   = on_load_function(OnLoadTable),
  Funs        = [X || {_, X} <- ets:tab2list(FunsTable)],

  [ ModuleAttr, FileAttr, VarsAttr, ExportAttr |
    UniqueAttrs ++ Rest ++ [OnLoadFun | Funs]
  ].

%% @private
on_load_function(OnLoadTable) ->
  Exprs = case [Expr || {_, Expr} <- ets:tab2list(OnLoadTable)] of
            []       -> [{atom, 0, ok}];
            ExprsTmp -> ExprsTmp
          end,
  Clause = {clause, 0, [], [], Exprs},
  {function, 0, ?ON_LOAD_FUNCTION, 0, [Clause]}.

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
  Module1 = Module#module{attrs = Attrs ++ Module#module.attrs},
  save(?MODULE, Module1).

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
  ok = lists:foreach(fun(M) -> ets:delete(?MODULE, M#module.name) end, Modules),

  {reply, Modules, State};
handle_call(all, {Pid, _}, #{loaded_modules := TabId} = State) ->
  Modules = case get(TabId, Pid) of
              undefined   -> [];
              {Pid, Mods} -> Mods
            end,

  {reply, Modules, State}.

handle_cast(_Msg, State) ->
  {ok, State}.

handle_info(_Msg, State) ->
  {ok, State}.

terminate(_Msg, State) ->
  {ok, State}.

code_change(_Msg, _From, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Helper Functions
%%------------------------------------------------------------------------------

%% @private
-spec delete_fake_modules(clj_module()) -> ok.
delete_fake_modules(Module) ->
  FakeModulesId = Module#module.fake_modules,
  [code:delete(Name) || {Name} <- ets:tab2list(FakeModulesId)],
  ok.

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
new(Name, Source) when is_atom(Name), is_binary(Source) ->
  new(Name, binary_to_list(Source));
new(Name, Source) when is_atom(Name), is_list(Source) ->
  new([ {attribute, 0, module, Name}
      , {attribute, 0, file, {Source, 0}}
      ]
     ).

-spec new([erl_parse:abstract_form()]) -> ok | {error, term()}.
new(Forms) when is_list(Forms) ->
  {[ModuleAttr], Rest} = lists:partition(is_attribute_fun(module), Forms),
  {AllAttrs, Rest1} = lists:partition(fun is_attribute/1, Rest),
  {Funs, Rest2} = lists:partition(fun is_function/1, Rest1),

  {attribute, _, module, Name} = ModuleAttr,
  {VarsAttrs, RestAttrs} = lists:partition(is_attribute_fun(vars), AllAttrs),

  clj_utils:throw_when( length(VarsAttrs) > 1
                      , [ <<"The module ">>
                        , atom_to_binary(Name, utf8)
                        , <<" contains more than one 'vars' attributes.">>
                        ]
                      ),
  Vars = case VarsAttrs of
           [] -> #{};
           [VarsAttr] ->
             {attribute, _, _, V} = VarsAttr,
             V
         end,

  {ExportAttrs, RestAttrs1} = lists:partition(is_attribute_fun(export)
                                             , RestAttrs
                                             ),

  Exports = flat_exports(ExportAttrs),

  {Source, Attrs} = source_file(RestAttrs1),

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
                  , attrs             = Attrs
                  , rest              = Rest2
                  },

  Module = add_functions(Module, Funs),
  Module = add_vars(Module, maps:values(Vars)),

  %% Remove the on_load function and all its contents.
  %% IMPORTANT: This means that whenever a namespace is recompiled all
  %% `on-load*' expressions need to be included in the compilation as
  %% well or they won't be present in the resulting binary.
  OnLoadId = {?ON_LOAD_FUNCTION, 0},
  true     = ets:delete(Module#module.funs, OnLoadId),

  add_exports(Module, Exports).

-spec is_attribute(erl_parse:abstract_form()) -> boolean.
is_attribute({attribute, _, _, _}) -> true;
is_attribute(_) -> false.

-spec is_attribute_fun(atom()) -> function().
is_attribute_fun(Name) ->
  fun
    ({attribute, _, AttrName, _}) when Name =:= AttrName -> true;
    (_) -> false
  end.

-spec is_function(erl_parse:abstract_form()) -> boolean.
is_function({function, _, _, _, _}) -> true;
is_function(_) -> false.

-spec function_name(erl_parse:abstract_form()) -> atom().
function_name({function, _, Name, _, _}) -> Name.

-spec function_arity(erl_parse:abstract_form()) -> integer().
function_arity({function, _, _, Arity, _}) -> Arity.

-spec function_id(erl_parse:abstract_form()) -> function_id().
function_id(Function) ->
  {function_name(Function), function_arity(Function)}.

-spec flat_exports([erl_parse:abstract_form()]) ->
  [{atom(), non_neg_integer()}].
flat_exports(ExportAttrs) ->
  lists:flatmap(fun({attribute, _, _, Vals}) -> Vals end, ExportAttrs).

-spec source_file([erl_parse:abstract_form()]) ->
  {binary(), [erl_parse:abstract_form()]}.
source_file(Attrs) ->
  case lists:partition(is_attribute_fun(file), Attrs) of
    {[], Attrs} -> Attrs;
    {[FileAttr], RestAttrs} ->
      {attribute, _, file, {Source, _}} = FileAttr,
      {Source, RestAttrs}
  end.
