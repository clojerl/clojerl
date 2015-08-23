-module(clj_compiler).

-export([
         is_special_symbol/1,
         compile_files/1,
         compile_file/1,
         compile/1
        ]).

-include("include/clj_types.hrl").

-type state() :: #{current_ns => symbol(),
                   local_bindings => #{symbol() => any()},
                   namespaces => []}.

-spec is_special_symbol(sexpr()) -> boolean().
is_special_symbol(s) ->
  lists:member(s, special_forms()).

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------

-spec compile_files([file:filename_all()]) -> ok.
compile_files(Files) ->
  State = lists:foldl(fun compile_file/2, clean_state(), Files),
  emit_code(State).

-spec compile_file(file:filename_all()) -> ok.
compile_file(File) ->
  State = compile_file(File, clean_state()),
  emit_code(State).

-spec compile_file(state(), file:filename_all()) -> ok.
compile_file(File, State) ->
  {ok, Src} = file:read_file(File),
  compile(State, Src).

-spec compile(binary()) -> state().
compile(Src) ->
  State = compile(Src, clean_state()),
  emit_code(State).

-spec compile(state(), binary()) -> state().
compile(Src, State) ->
  Forms = clj_reader:read_all(Src),
  _NewState = lists:foldl(fun analyze/2, State, Forms).

%%------------------------------------------------------------------------------
%% Analyze
%%------------------------------------------------------------------------------

-spec analyze(any(), state()) -> state().
analyze(nil, State) ->
  Expr = erl_syntax:abstract(undefined),
  add_expr(State, Expr);
analyze(Boolean, State) when is_boolean(Boolean) ->
  Expr = erl_syntax:abstract(Boolean),
  add_expr(State, Expr);
analyze(String, State) when is_binary(String) ->
  Expr = erl_syntax:abstract(String),
  add_expr(State, Expr);
analyze(Number, State) when is_number(Number) ->
  Expr = erl_syntax:abstract(Number),
  add_expr(State, Expr);
analyze(Form, State) ->
  case clj_core:type(Form) of
    'clojerl.Symbol' ->
      analyze_symbol(Form, State);
    'clojerl.Keyword' ->
      Expr = erl_syntax:abstract(Form),
      add_expr(State, Expr);
    'clojerl.List' ->
      Op = clj_core:first(Form),
      analyze_seq(Op, Form, State);
    'clojerl.Vector' ->
      Expr = erl_syntax:abstract(Form),
      add_expr(State, Expr);
    'clojerl.Map' ->
      Expr = erl_syntax:abstract(Form),
      add_expr(State, Expr);
    'clojerl.Set' ->
      Expr = erl_syntax:abstract(Form),
      add_expr(State, Expr);
    _ ->
      throw({invalid_form, Form, State})
  end.

-spec analyze_seq(any(), 'clojerl.List':type(), state()) -> state().
analyze_seq(undefined, _List, _State) ->
  throw(<<"Can't call nil">>);
analyze_seq(Op, List, State) ->
  Specials = special_forms(),
  case lists:member(Op, Specials) of
    true ->
      Name = clj_core:name(Op),
      analyze_special_form(Name, List, State);
    false ->
      analyze_invoke(List, State)
  end.

-spec analyze_special_form(any(), 'clojerl.List':type(), state()) -> state().
analyze_special_form('def', List, State) ->
  Docstring = case {clj_core:count(List), clj_core:third(List)} of
                {4, Str} when is_binary(Str) -> Str;
                _ -> undefined
              end,
  case clj_core:count(List) of
    C when C < 2 ->
      throw(<<"Too few arguments to def">>);
    C when C > 3, Docstring == undefined; C > 4, Docstring =/= undefined  ->
      throw(<<"Too many arguments to def">>);
    _ ->
      erlang:display({special, def}),
      State
  end.

special_forms() ->
  SymbolFun = fun clj_core:symbol/1,
  [SymbolFun('def'),
   SymbolFun('loop*'),
   SymbolFun('recur'),
   SymbolFun('if'),
   SymbolFun('case*'),
   SymbolFun('let*'),
   SymbolFun('letfn*'),
   SymbolFun('do'),
   SymbolFun('fn*'),
   SymbolFun('quote'),
   SymbolFun('var'),
   SymbolFun('import*'),
   SymbolFun('deftype*'),
   SymbolFun('reify*'),
   SymbolFun('try'),
   %% SymbolFun('monitor-enter') => fun analyze_def/2,
   %% SymbolFun('monitor-exit') => fun analyze_def/2,
   %% SymbolFun('new') => fun analyze_def/2,
   %% SymbolFun('&') => fun analyze_def/2
   SymbolFun('throw'),
   SymbolFun('catch'),
   SymbolFun('finally')
  ].

-spec analyze_invoke('clojerl.List':type(), state()) -> state().
analyze_invoke(_List, State) ->
  State.

-spec analyze_symbol(symbol(), state()) -> state().
analyze_symbol(_, State) ->
  erlang:display(analyze_symbol),
  State.

%%------------------------------------------------------------------------------
%% Code Emission
%%------------------------------------------------------------------------------

-spec emit_code(state()) -> ok.
emit_code(State = #{exprs := Exprs}) ->
  AbstractSyntaxForms = lists:map(fun erl_syntax:revert/1, Exprs),
  %%erlang:display(AbstractSyntaxForms),
  erlang:display(erl_eval:expr_list(AbstractSyntaxForms, [])),
  %%compile:forms(AbstractSyntaxForms),
  State.

%%------------------------------------------------------------------------------
%% State, Namespace & Helpers
%%------------------------------------------------------------------------------

-spec clean_state() -> state().
clean_state() ->
  UserSym = clj_core:symbol(user),
  UserNs = clj_namespace:new(UserSym),
  #{namespaces     => #{UserSym => UserNs},
    exprs          => [],
    current_ns     => clj_core:symbol(user),
    local_bindings => #{}}.

-spec current_ns(state()) -> clj_namespace:namespace().
current_ns(#{current_ns := CurrentNs,
             namespaces := Namespaces}) ->
  maps:get(CurrentNs, Namespaces).

-spec update_ns(state(), clj_namespace:namespace()) -> state().
update_ns(State = #{namespaces := Nss}, Ns = #{name := Name}) ->
  State#{namespaces => maps:put(Name, Ns, Nss)}.

-spec add_expr(state(), erl_syntax:syntaxTree()) -> state().
add_expr(State = #{exprs := Exprs}, Expr) ->
  State#{exprs => [Expr | Exprs]};
add_expr(State, Expr) ->
  State#{exprs => [Expr]}.

-spec add_form(clj_namespace:namespace(), erl_syntax:syntaxTree()) ->
  clj_namespace:namespace().
add_form(Ns = #{forms := Forms}, Form) ->
  Ns#{forms => [Form | Forms]}.
