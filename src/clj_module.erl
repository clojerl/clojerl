-module(clj_module).

-compile({no_auto_import, [get/1]}).

-export([ init/0

        , all/0
        , all_forms/0
        , load/1
        , is_loaded/1
        , to_forms/1

        , fake_fun/3
        , replace_calls/3

        , add_vars/2
        , add_attributes/2
        , add_exports/2
        , add_functions/2

        , is_clojure/1
        ]).

%% -type var_id()      :: binary().
-type function_id() :: {atom(), integer()}.
%% -type export()      :: {atom(), non_neg_integer()}.

-record(module, { name      :: atom(),
                  vars      :: ets:tid(),
                  funs      :: ets:tid(),
                  fake_funs :: ets:tid(),
                  exports   :: ets:tid(),
                  attrs     :: [erl_parse:abstract_form()],
                  rest      :: [erl_parse:abstract_form()]
                }).

-type clj_module() :: #module{}.

-export_type([clj_module/0]).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------

-spec init() -> ok.
init() ->
  TabId = ets:new(?MODULE, [set, protected, {keypos, 2}]),
  erlang:put(?MODULE, TabId),
  ok.

%% @doc Gets the named fake fun that corresponds to the mfa provided.
%%      A fake fun is generated during compile-time and it provides the
%%      same functionality as its original. The only difference is that
%%      all calls to functions in the same module are replaced by a call
%%      to clj_module:fake_fun/3.
%%      This is necessary so that macro functions can be used without
%%      having to generate, compile and load the binary for the partial
%%      module each time a macro is found.
-spec fake_fun(module(), atom(), integer()) -> function() | notfound.
fake_fun(ModuleName, Function, Arity) ->
  Module        = get(modules_table_id(), ModuleName),
  FakeFunsTable = Module#module.fake_funs,
  FA = {Function, Arity},
  case ets:lookup(FakeFunsTable, FA) of
    [] ->
      case get(Module#module.funs, FA) of
        undefined -> notfound;
        {_, FunctionAst} ->
          {function, _, _, _, Clauses} =
            replace_calls(FunctionAst, ModuleName, Function),
          FunAst = {'named_fun', 0, Function, Clauses},
          {value, Fun, _} = erl_eval:expr(FunAst, []),
          Fun1 = check_var_val(Function, Arity, Fun),
          save(Module#module.fake_funs, {FA, Fun1}),
          Fun1
      end;
    [{_, Fun}] ->
      Fun
  end.

%% @doc Checks if the function is actually the one that provides the value
%%      of a var, Then it checks if the value returned is the var itself and
%%      if so replaces the returned value for a clojerl.FakeVar, which
%%      reimplements clojerl.IFn protocol so that the invoke is done using
%%      the var's corresponding fake fun.
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

%% @doc Processes a function's ast and modifies all calls to functions in the
%%      function's own module for a call to the fun returned by
%%      clj_module:fake_fun/3.
%%      If the call is a recursive call to the top function then it changes
%%      the call to a variable built using the value of TopFunction.
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
  %% Only replace the call if the module is loaded exists. If it is not,
  %% then the replacement is happening for the evaluation of an expression
  %% where the called function hasn't been declared in the same evaluation.
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
replace_calls( {call, Line, {atom, _, TopFunction}, Args}
             , Module
             , TopFunction) ->
  Args1 = replace_calls(Args, Module, TopFunction),
  {call, Line, {var, Line, TopFunction}, Args1};
replace_calls(Ast, Module, TopFunction) when is_tuple(Ast) ->
  list_to_tuple(replace_calls(tuple_to_list(Ast), Module, TopFunction));
replace_calls(Ast, Module, TopFunction) when is_list(Ast) ->
  [replace_calls(Item, Module, TopFunction) || Item <- Ast];
replace_calls(Ast, _, _) ->
  Ast.

-spec load(atom()) -> ok | {error, term()}.
load(Name) ->
  case code:ensure_loaded(Name) of
    {module, Name} ->
      new(clj_utils:code_from_binary(Name));
    {error, _} ->
      new([attribute_module(Name)])
  end.

-spec is_loaded(module()) -> boolean().
is_loaded(Name) ->
  ets:member(modules_table_id(), Name).

-spec all() -> [clj_module()].
all() -> ets:tab2list(modules_table_id()).

%% @doc Returns a list where each element is a list with the abstract
%%      forms of all stored modules.
-spec all_forms() -> [[erl_parse:abstract_form()]].
all_forms() ->
  lists:map(fun clj_module:to_forms/1, all()).

-spec to_forms(clj_module()) -> [erl_parse:abstract_form()].
to_forms(#module{name = Name} = Module) ->
  #module{ vars    = VarsTable
         , funs    = FunsTable
         , exports = ExportsTable
         , attrs   = Attrs
         , rest    = Rest
         } = Module,

  ModuleAttr   = attribute_module(Name),

  VarsList    = [{clj_core:name(X), X} || {_, X} <- ets:tab2list(VarsTable)],
  Vars        = maps:from_list(VarsList),
  VarsAttr    = {attribute, 0, vars, Vars},

  Exports     = [X || {X} <- ets:tab2list(ExportsTable)],
  ExportAttr  = {attribute, 0, export, Exports},

  ClojureAttr = {attribute, 0, clojure, true},

  UniqueAttrs = lists:usort([ClojureAttr | Attrs]),

  Funs         = [X || {_, X} <- ets:tab2list(FunsTable)],

  [ModuleAttr, VarsAttr, ExportAttr | UniqueAttrs ++ Rest ++ Funs].

-spec add_vars(module() | clj_module(), ['clojerl.Var':type()]) -> clj_module().
add_vars(ModuleName, Vars) when is_atom(ModuleName)  ->
  add_vars(get(modules_table_id(), ModuleName), Vars);
add_vars(Module, Vars) ->
  AddFun = fun(V) ->
               K = clj_core:name(V),
               save(Module#module.vars, {K, V})
           end,
  lists:foreach(AddFun, Vars),
  Module.

-spec add_attributes(clj_module(), [erl_parse:abstract_form()]) -> clj_module().
add_attributes(ModuleName, Attrs) when is_atom(ModuleName)  ->
  add_attributes(get(modules_table_id(), ModuleName), Attrs);
add_attributes(Module, []) ->
  Module;
add_attributes(Module, Attrs) ->
  Module1 = Module#module{attrs = Attrs ++ Module#module.attrs},
  save(modules_table_id(), Module1).

-spec add_exports(clj_module(), [{atom(), non_neg_integer()}]) ->
  clj_module().
add_exports(ModuleName, Exports) when is_atom(ModuleName)  ->
  add_exports(get(modules_table_id(), ModuleName), Exports);
add_exports(Module, Exports) ->
  AddExport = fun(E) ->
                  save(Module#module.exports, {E})
              end,
  ok = lists:foreach(AddExport, Exports),
  Module.

-spec add_functions(module() | clj_module(), [erl_parse:abstract_form()]) ->
  clj_module().
add_functions(ModuleName, Funs) when is_atom(ModuleName)  ->
  add_functions(get(modules_table_id(), ModuleName), Funs);
add_functions(Module, Funs) ->
  SaveFun = fun(F) ->
                save(Module#module.funs, {function_id(F), F})
            end,
  lists:foreach(SaveFun, Funs),
  Module.

-spec is_clojure(module()) -> boolean().
is_clojure(Name) ->
  Key = {?MODULE, is_clojure, Name},
  case erlang:get(Key) of
    undefined ->
      Attrs = Name:module_info(attributes),
      IsClojure = lists:keymember(clojure, 1, Attrs),
      erlang:put(Key, IsClojure),
      IsClojure;
    Value ->
      Value
  end.

%%------------------------------------------------------------------------------
%% Helper Functions
%%------------------------------------------------------------------------------

-spec modules_table_id() -> ets:tid().
modules_table_id() ->
  erlang:get(?MODULE).

-spec get(atom(), module()) -> clj_module().
get(undefined, Id) -> %% If there is no table then nothing will be found.
  throw({notfound, Id});
get(Table, Id) ->
  case ets:lookup(Table, Id) of
    [] -> throw({notfound, Id});
    [Value] -> Value
  end.

-spec save(atom(), term()) -> term().
save(Table, Value) ->
  true = ets:insert(Table, Value),
  Value.

-spec new([erl_parse:abstract_form()]) -> ok | {error, term()}.
new(Forms) ->
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

  {ExportAttrs, Attrs} = lists:partition(is_attribute_fun(export), RestAttrs),
  Exports = flat_exports(ExportAttrs),

  TableOpts = [set, protected, {keypos, 1}],
  Module = #module{ name      = Name
                  , vars      = ets:new(var, TableOpts)
                  , funs      = ets:new(funs, TableOpts)
                  , fake_funs = ets:new(fake_funs, TableOpts)
                  , exports   = ets:new(exports, TableOpts)
                  , attrs     = Attrs
                  , rest      = Rest2
                  },

  Module1 = add_functions(Module, Funs),
  Module2 = add_vars(Module1, maps:values(Vars)),
  Module3 = add_exports(Module2, Exports),

  save(modules_table_id(), Module3).

-spec attribute_module(atom()) -> erl_parse:abstract_form().
attribute_module(Name) when is_atom(Name) ->
  {attribute, 0, module, Name}.

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
