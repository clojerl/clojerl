-module(clj_module).
-behavior(gen_server).
-compile({no_auto_import, [get/1]}).

-export([start_link/0]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , code_change/3
        , terminate/2
        ]).

-export([ all/0
        , load/1
        , is_loaded/1
        , to_forms/1
        , fun_for/3

        , add_vars/2
        , add_attributes/2
        , add_exports/2
        , add_functions/2

        , is_clojure/1
        ]).

-define(FAKE_FUNS, clj_module_fake_funs).

-type var_id() :: binary().

-type function_id() :: {atom(), integer()}.
-type export() :: {atom(), non_neg_integer()}.

-record(module, { name    :: atom(),
                  vars    :: #{var_id() => 'clojerl.Var':type()},
                  attrs   :: [erl_syntax:syntaxTree()],
                  exports :: #{export() => true},
                  funs    :: #{function_id() => erl_syntax:syntaxTree()},
                  rest    :: [erl_syntax:syntaxTree()]
                }).

-type clj_module() :: #module{}.

-export_type([clj_module/0]).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

-spec start_link() -> ok.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(term()) -> {ok, term()}.
init(_) ->
  ets:new(?MODULE, [set, public, named_table, {keypos, 2}]),
  ets:new(?FAKE_FUNS, [set, public, named_table, {keypos, 1}]),
  {ok, []}.

-spec handle_call(term(), term(), term()) -> term().
handle_call(_Request, _From, State) ->
  {reply, undefined, State}.

-spec handle_cast(term(), term()) -> term().
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(term(), term()) -> term().
handle_info(_Request, State) ->
  {noreply, State}.

-spec terminate(term(), term()) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(term(), term(), term()) -> term().
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------

-spec fun_for(module(), atom(), integer()) -> function().
fun_for(Name, Function, Arity) ->
  MFA = {Name, Function, Arity},
  case ets:lookup(?FAKE_FUNS, MFA) of
    [] ->
      %% io:format("fun_for ~p~n", [MFA]),
      Module = get(Name),
      case maps:get({Function, Arity}, Module#module.funs, undefined) of
        undefined -> throw({notfound, {Name, Function, Arity}});
        FunctionAst ->
          {function, _, _, _, Clauses} = replace_calls(FunctionAst, Name, Function),
          FunAst = {'named_fun', 0, Function, Clauses},
          {value, Fun, _} = erl_eval:expr(FunAst, []),
          Fun1 = check_var_val(Function, Arity, Fun),
          ets:insert(?FAKE_FUNS, {MFA, Fun1}),
          Fun1
      end;
    [{_, Fun}] ->
      Fun
  end.

-spec check_var_val(atom(), integer(), function()) -> function().
check_var_val(Function, 0, Fun) ->
  FunctionBin = atom_to_binary(Function, utf8),
  case clj_utils:ends_with(FunctionBin, <<"__val">>) of
    true ->
      Value = Fun(),
      case clj_core:'var?'(Value) of
        true  -> fun() -> 'clojerl.FakeVar':new(Value) end;
        false -> Fun
      end;
    false -> Fun
  end;
check_var_val(_Function, _Arity, Fun) ->
  Fun.

-spec replace_calls(erl_parse:abstract_form(), module(), atom()) ->
  erl_parse:abstract_form().
replace_calls( { call, Line
               , { remote, _
                 , {atom, _, Module}
                 , {atom, _, Function}
                 }
               , Args
               }
             , Module
             , TopFunction) ->
  Remote = {remote, Line,
            {atom, Line, ?MODULE},
            {atom, Line, fun_for}
           },

  Arity = length(Args),
  %% io:format("Changing call to ~p~n", [{Module, Function, Arity}]),
  FunCall = {call, Line, Remote, [
    {atom, Line, Module}, {atom, Line, Function}, {integer, Line, Arity}
  ]},
  Args1 = replace_calls(Args, Module, TopFunction),
  {call, Line, FunCall, Args1};
replace_calls( {call, Line, {atom, _, TopFunction}, Args}
             , Module
             , TopFunction) ->
  %% io:format("Changing recursive call to ~p~n", [TopFunction]),
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
      from_binary(Name);
    {error, _} ->
      new([attribute_module(Name)])
  end.

-spec is_loaded(module()) -> boolean().
is_loaded(Name) ->
  [] =/= ets:lookup(?MODULE, Name).

-spec all() -> [clj_module()].
all() -> ets:tab2list(?MODULE).

-spec to_forms(clj_module()) -> [erl_syntax:syntaxTree()].
to_forms(#module{name = Name} = Module) ->
  #module{ attrs   = Attrs
         , exports = Exports
         , vars    = Vars
         , funs    = Funs
         , rest    = Rest
         } = Module,

  ClojureAtom  = erl_syntax:atom(clojure),
  ClojureAttr  = erl_syntax:attribute(ClojureAtom, [erl_syntax:atom(true)]),
  ClojureAttr1 = erl_syntax:revert(ClojureAttr),

  UniqueAttrs = lists:usort([ClojureAttr1 | Attrs]),
  UniqueFuns  = maps:values(Funs),

  VarsAtom    = erl_syntax:atom(vars),
  VarsAttr    = erl_syntax:attribute(VarsAtom, [erl_syntax:abstract(Vars)]),
  VarsAttr1   = erl_syntax:revert(VarsAttr),

  ExportAttr = export_attribute(maps:keys(Exports)),
  ModuleAttr = attribute_module(Name),

  [ModuleAttr, VarsAttr1, ExportAttr | UniqueAttrs ++ Rest ++ UniqueFuns].

-spec add_vars(atom(), ['clojerl.Var':type()]) -> ok.
add_vars(Name, Vars) ->
  UpdateFun =
    fun(Module) ->
        AddByKeyFun = fun(V, Acc) ->
                          K = clj_core:name(V),
                          Acc#{K => V}
                      end,
        Vars1 = lists:foldl(AddByKeyFun, Module#module.vars, Vars),
        Module#module{vars = Vars1}
    end,
  update(Name, UpdateFun).

-spec add_attributes(clj_module(), [erl_syntax:syntaxTree()]) -> clj_module().
add_attributes(Name, Attrs) ->
  UpdateFun = fun(Module) ->
                  Attrs1 = erl_syntax:revert_forms(Attrs),
                  Module#module{attrs = Attrs1 ++ Module#module.attrs}
              end,
  update(Name, UpdateFun).

-spec add_exports(clj_module(), [{atom(), non_neg_integer()}]) ->
  clj_module().
add_exports(Name, Exports) ->
  UpdateFun =
    fun(Module) ->
        AddExport = fun(E, M) -> M#{E => true} end,
        Exports1  = lists:foldl(AddExport, Module#module.exports, Exports),
        Module#module{exports = Exports1}
    end,
  update(Name, UpdateFun).

-spec add_functions(clj_module(), [erl_syntax:syntaxTree()]) -> clj_module().
add_functions(Name, AddFuns) ->
  UpdateFun = fun(Module) ->
                  AddByKeyFun = fun(F, Acc) ->
                                    K = function_id(F),
                                    Acc#{K => erl_syntax:revert(F)}
                                end,
                  Funs1 = lists:foldl(AddByKeyFun, Module#module.funs, AddFuns),
                  Module#module{funs = Funs1}
              end,
  update(Name, UpdateFun).

-spec is_clojure(module()) -> boolean().
is_clojure(Name) ->
  Attrs = Name:module_info(attributes),
  lists:keymember(clojure, 1, Attrs).

%%------------------------------------------------------------------------------
%% Helper Functions
%%------------------------------------------------------------------------------

-spec get(module()) -> clj_module().
get(Name) ->
  case ets:lookup(?MODULE, Name) of
    [] -> throw({notfound, Name});
    [Module] -> Module
  end.

-spec save(clj_module()) -> ok.
save(Module) ->
  true = ets:insert(?MODULE, Module),
  ok.

-spec update(atom(), function()) -> ok.
update(Name, UpdateFun) ->
  Module = get(Name),
  Module1 = UpdateFun(Module),
  true = ets:insert(?MODULE, Module1),
  ok.

-spec new([erl_syntax:syntaxTree()]) -> ok | {error, term()}.
new(Forms) ->
  {[ModuleAttr], Rest} = lists:partition(is_attribute_fun(module), Forms),
  {AllAttrs, Rest1} = lists:partition(fun is_attribute/1, Rest),
  {Funs, Rest2} = lists:partition(fun is_function/1, Rest1),

  [ModuleAst] = erl_syntax:attribute_arguments(ModuleAttr),
  Name        = erl_syntax:concrete(ModuleAst),
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
             [V] = erl_syntax:attribute_arguments(VarsAttr),
             erl_syntax:concrete(V)
         end,

  {ExportAttrs, Attrs} = lists:partition(is_attribute_fun(export), RestAttrs),
  Exports    = concrete_export(ExportAttrs),
  ExportsMap = lists:foldl(fun(E, M) -> M#{E => true} end, #{}, Exports),

  IndexedFuns = index_functions(Funs),

  Module = #module{ name    = Name
                  , vars    = Vars
                  , attrs   = erl_syntax:revert_forms(Attrs)
                  , exports = ExportsMap
                  , funs    = IndexedFuns
                  , rest    = erl_syntax:revert_forms(Rest2)
                  },

  save(Module).

-spec from_binary(atom()) -> ok | {error, term()}.
from_binary(Name) when is_atom(Name) ->
  case code:get_object_code(Name) of
    {Name, Binary, _} ->
      case beam_lib:chunks(Binary, [abstract_code]) of
        {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
          new(Forms);
        Error ->
          Error
      end;
    _ ->
      clj_utils:throw([ <<"Could not load object code for namespace: ">>
                      , atom_to_binary(Name, utf8)
                      ]
                     )
  end.
-spec attribute_module(atom()) -> erl_syntax:syntaxTree().

attribute_module(Name) when is_atom(Name) ->
  ModuleAtom = erl_syntax:atom(module),
  NameAtom = erl_syntax:atom(Name),
  Attr = erl_syntax:attribute(ModuleAtom, [NameAtom]),
  erl_syntax:revert(Attr).

-spec is_attribute(erl_syntax:syntaxTree()) -> boolean.
is_attribute(Form) ->
  erl_syntax:type(Form) =:= attribute.

-spec is_attribute_fun(atom()) -> function().
is_attribute_fun(Name) ->
  fun(Form) ->
      is_attribute(Form) andalso
        erl_syntax:concrete(erl_syntax:attribute_name(Form)) =:= Name
  end.

-spec is_function(erl_syntax:syntaxTree()) -> boolean.
is_function(Form) ->
  erl_syntax:type(Form) =:= function.

-spec function_name(erl_syntax:syntaxTree()) -> atom().
function_name(Function) ->
  erl_syntax:atom_value(erl_syntax:function_name(Function)).

-spec function_arity(erl_syntax:syntaxTree()) -> integer().
function_arity(Function) ->
  erl_syntax:function_arity(Function).

-spec function_id(erl_syntax:syntaxTree()) -> function_id().
function_id(Function) ->
  {function_name(Function), function_arity(Function)}.

-spec index_functions([erl_syntax:syntaxTree()]) ->
  #{atom() => erl_syntax:syntaxTree()}.
index_functions(Funs) ->
  IndexFun = fun(F, M) ->
                 Key = function_id(F),
                 M#{Key => erl_syntax:revert(F)}
             end,
  lists:foldl(IndexFun, #{}, Funs).

-spec concrete_export(erl_syntax:syntaxTree()) ->
  [{atom(), non_neg_integer()}].
concrete_export(ExportAttrs) ->
  Exports  = lists:flatmap(fun erl_syntax:attribute_arguments/1, ExportAttrs),
  Exports1 = lists:flatmap(fun erl_syntax:concrete/1, Exports),
  lists:map(fun concrete_arity_qualifier/1, Exports1).

-spec concrete_arity_qualifier(erl_syntax:syntaxTree()) ->
  {atom(), non_neg_integer()}.
concrete_arity_qualifier({NameAst, ArityAst}) ->
  { erl_syntax:concrete(NameAst)
  , erl_syntax:concrete(ArityAst)
  }.

-spec export_attribute([{atom(), integer()}]) ->
  erl_syntax:syntaxTree().
export_attribute(FAs) when is_list(FAs) ->
  ExportAtom = erl_syntax:atom(export),
  Fun  = fun({Function, Arity}) -> arity_qualifier(Function, Arity) end,
  Asts = lists:usort(lists:map(Fun, FAs)),
  ListAst = erl_syntax:list(Asts),
  Attr = erl_syntax:attribute(ExportAtom, [ListAst]),
  erl_syntax:revert(Attr).

-spec arity_qualifier(atom(), integer()) -> erl_syntax:syntaxTree().
arity_qualifier(Function, Arity) when is_atom(Function),
                                      is_integer(Arity) ->
  FunctionAtomAst = erl_syntax:atom(Function),
  ArityIntegerAst = erl_syntax:integer(Arity),
  erl_syntax:arity_qualifier(FunctionAtomAst, ArityIntegerAst).
