%% Parts of this module are heavily inspired in erl_eval.erl
%% and some helper functions (e.g. for binary matching and eval'ing)
%% are copied directly from there.
-module(core_eval).

-export([exprs/1, expr/1, expr/2]).

-include("clojerl.hrl").
-include("clojerl_int.hrl").
-include_lib("compiler/src/core_parse.hrl").

-type value()    :: term().
-type bindings() :: #{}.

-define(STACKTRACE,
        element(2, erlang:process_info(self(), current_stacktrace))).

-spec expr(cerl:cerl()) -> value().
expr(Expr) ->
  expr(Expr, #{}).

-spec expr(cerl:cerl(), bindings()) -> value().
expr(Expr, Bindings) ->
  lint(Expr),
  expr_(Expr, Bindings).

-spec exprs([cerl:cerl()]) -> value().
exprs([]) ->
  erlang:error(no_expressions);
exprs([Expr]) ->
  expr(Expr);
exprs([Expr | Exprs]) ->
  expr(Expr),
  exprs(Exprs).

%% -----------------------------------------------------------------------------
%% Internal functions
%% -----------------------------------------------------------------------------

-spec lint(cerl:cerl()) -> ok | no_return().
lint(#c_module{} = Module) ->
  case core_lint:module(Module) of
    {ok, _} -> ok;
    {error, [{_, Errors}], _Warnings} ->
      raise(process_lint_errors(Errors))
  end;
lint(Expr) ->
  Name    = cerl:abstract(m),
  FName   = cerl:c_var({f, 0}),
  Exports = [FName],
  Fun     = cerl:c_fun([], Expr),
  Defs    = [{FName, Fun}],
  Module  = cerl:c_module(Name, Exports, Defs),
  lint(Module).

-spec process_lint_errors([term()]) -> term().
process_lint_errors(Errors0) ->
  Errors1 = [Error || {'none', _, Error} <- Errors0],
  case Errors1 of
    [Error] -> Error;
    _ -> Errors1
  end.

-spec expr_(cerl:cerl(), bindings()) -> value().
%% Alias -----------------------------------------------------------------------
%% Handled by core_lint
%% Apply -----------------------------------------------------------------------
expr_(#c_apply{op = Op, args = Args}, Bindings) ->
  Operator  = expr_(Op, Bindings),
  Arguments = expr_list(Args, Bindings),
  apply(Operator, Arguments);
%% Binary ----------------------------------------------------------------------
expr_(#c_binary{segments = Segments}, Bindings) ->
  list_to_bitstring(expr_list(Segments, Bindings));
%% Bitstring -------------------------------------------------------------------
expr_(#c_bitstr{ val   = Val
              , size  = Size
              , unit  = Unit
              , type  = Type
              , flags = Flags
              }
    , Bindings) ->
  expr_bitstr(Val, Size, Unit, Type, Flags, Bindings);
%% Call ------------------------------------------------------------------------
expr_(#c_call{module = M, name = F, args = Args}, Bindings) ->
  Module    = cerl:concrete(M),
  Function  = cerl:concrete(F),
  Arguments = expr_list(Args, Bindings),
  apply(Module, Function, Arguments);
%% Case ------------------------------------------------------------------------
expr_(#c_case{arg = Arg} = Expr, Bindings) ->
  Args   = check_c_values(Arg),
  Values = expr_list(Args, Bindings),
  case_clauses(Values, Expr, Bindings);
%% Catch -----------------------------------------------------------------------
expr_(#c_catch{body = Body}, Bindings) ->
  try expr_(Body, Bindings)
  catch
    throw:Error ->
      Error;
    exit:Error ->
      {'EXIT', Error};
    ?WITH_STACKTRACE(error, Error, Stacktrace)
      {'EXIT', {Error, Stacktrace}}
  end;
%% Clause ----------------------------------------------------------------------
%% Handled by core_lint
%% Cons ------------------------------------------------------------------------
expr_(#c_cons{hd = Head, tl = Tail}, Bindings) ->
  HeadVal = expr_(Head, Bindings),
  TailVal = expr_(Tail, Bindings),
  [HeadVal | TailVal];
%% Fun -------------------------------------------------------------------------
expr_(#c_fun{vars = Vars, body = Body}, Bindings) ->
  Names = [Name || #c_var{name = Name} <- Vars],
  create_fun(Names, Body, Bindings);
%% Let -------------------------------------------------------------------------
expr_(#c_let{vars = Vars, arg = Arg, body = Body}, Bindings0) ->
  Values    = expr_maybe_c_values(Arg, Bindings0),
  Pairs     = lists:zip(Vars, Values),
  Bindings1 = process_bindings(Pairs, Bindings0),
  expr_(Body, Bindings1);
%% Letrec ----------------------------------------------------------------------
expr_(#c_letrec{defs = Defs, body = Body}, Bindings) ->
  Fun = fun() -> letrec(Defs, Body, Bindings) end,
  with_proc_bindings(Fun);
%% Literal ---------------------------------------------------------------------
expr_(#c_literal{val = Value}, _Bindings) ->
  Value;
%% Map -------------------------------------------------------------------------
expr_(#c_map{is_pat = false, arg = Arg, es = Pairs}, Bindings) ->
  Value    = expr_(Arg, Bindings),
  MapPairs = expr_list(Pairs, Bindings),
  maps:merge(Value, maps:from_list(MapPairs));
%% Map Pair --------------------------------------------------------------------
expr_(#c_map_pair{key = K, val = V}, Bindings) ->
  {expr_(K, Bindings), expr_(V, Bindings)};
%% Module ----------------------------------------------------------------------
expr_(#c_module{}, _Bindings) ->
  erlang:error({invalid_expr, c_module});
%% Primitive Operation ---------------------------------------------------------
expr_(#c_primop{name = Name, args = Args} = Expr, Bindings) ->
  NameValue  = expr_(Name, Bindings),
  ArgsValues = expr_list(Args, Bindings),
  primitive_op(NameValue, ArgsValues, Expr);
%% Receive ---------------------------------------------------------------------
expr_( #c_receive{clauses = Clauses, timeout = Timeout, action = Action}
    , Bindings
    ) ->
  receive_clauses(Clauses, Timeout, Action, Bindings);
%% Sequence --------------------------------------------------------------------
expr_(#c_seq{arg = Argument, body = Body}, Bindings) ->
  expr_(Argument, Bindings),
  expr_(Body, Bindings);
%% Try -------------------------------------------------------------------------
expr_( #c_try{ arg = Arg
             , vars = Vars
             , body = Body
             , evars = EVars
             , handler = Handler
             }
    , Bindings0
    ) ->
  try
    Values = check_c_values(expr_(Arg, Bindings0)),
    {match, Bindings1} = match_list(Values, Vars, Bindings0),
    expr_(Body, Bindings1)
  catch Type:Error ->
      EVarsNames = [cerl:var_name(Evar) || Evar <- EVars],
      Bindings2 = add_bindings(EVarsNames, [Type, Error, ?NIL], Bindings0),
      expr_(Handler, Bindings2)
  end;
%% Tuple -----------------------------------------------------------------------
expr_(#c_tuple{es = Elements}, Bindings) ->
  Values = expr_list(Elements, Bindings),
  list_to_tuple(Values);
%% Values ----------------------------------------------------------------------
expr_(#c_values{es = Elements}, Bindings) ->
  Values = expr_list(Elements, Bindings),
  throw({values, Values});
%% Var -------------------------------------------------------------------------
expr_(#c_var{name = Name} = Expr, Bindings) ->
  case binding(Name, Bindings) of
    {value, Value} -> Value;
    unbound -> raise({unbound, Name}, Expr)
  end.

%% -----------------------------------------------------------------------------
%% Bindings
%% -----------------------------------------------------------------------------

-spec merge_bindings(bindings(), bindings()) -> bindings().
merge_bindings(Bindings1, Bindings2) ->
  Fold = fun({K, {value, V}}, Acc) ->
             case binding(K, Bindings2) of
               {value, V} -> Acc;
               {value, Current} -> raise({badmatch, Current});
               unbound -> add_binding(K, V, Acc)
             end
         end,
  lists:foldl(Fold, Bindings2, maps:to_list(Bindings1)).

-spec process_bindings([{cerl:cerl(), value()}], bindings()) -> bindings().
process_bindings(Pairs, Bindings) ->
  Fun = fun({Var, Value}, Acc) ->
            Name = cerl:var_name(Var),
            add_binding(Name, Value, Acc)
        end,
  lists:foldl(Fun, Bindings, Pairs).

-spec add_binding(term(), value(), bindings()) -> bindings().
add_binding(Name, Value, Bindings) ->
  maps:put(Name, {value, Value}, Bindings).

-spec add_bindings([cerl:var_name()], [value()], bindings()) -> bindings().
add_bindings([Name | Names], [Value | Values], Bindings0) ->
  Bindings1 = maps:put(Name, {value, Value}, Bindings0),
  add_bindings(Names, Values, Bindings1);
add_bindings([], [], Bindings) ->
  Bindings.

-spec remove_bindings([cerl:var_name()], bindings()) -> bindings().
remove_bindings(Names, Bindings0) ->
  maps:without(Names, Bindings0).

-spec binding(cerl:var_name(), bindings()) -> {value, value()} | unbound.
binding(Name, Bindings) ->
  case maps:get(Name, Bindings, unbound) of
    unbound -> proc_binding(Name);
    Found -> Found
  end.

%% -----------------------------------------------------------------------------
%% Process Bindings (used for letrec)
%% -----------------------------------------------------------------------------

-define(PROCESS_BINDINGS, '$__core_eval_process_bindings__$').

-spec with_proc_bindings(function()) -> value().
with_proc_bindings(Fun) ->
  Prev = erlang:get(?PROCESS_BINDINGS),
  try
    erlang:put(?PROCESS_BINDINGS, #{}),
    Fun()
  after
    (Prev =/= undefined) andalso erlang:put(?PROCESS_BINDINGS, Prev)
  end.

-spec add_proc_binding(cerl:var_name(), value()) -> ok.
add_proc_binding(Name, Value) ->
  Bindings = erlang:get(?PROCESS_BINDINGS),
  erlang:put(?PROCESS_BINDINGS, add_binding(Name, Value, Bindings)).

-spec proc_binding(cerl:var_name()) -> {value, value()} | unbound.
proc_binding(Name) ->
  case erlang:get(?PROCESS_BINDINGS) of
    undefined -> unbound;
    Bindings -> maps:get(Name, Bindings, unbound)
  end.

%% -----------------------------------------------------------------------------
%% Letrec
%% -----------------------------------------------------------------------------

letrec(Defs, Body, Bindings0) ->
  Names     = [Name || {#c_var{name = Name}, _} <- Defs],
  Bindings1 = remove_bindings(Names, Bindings0),
  [ begin
      VarNames = [VarName || #c_var{name = VarName} <- Vars],
      Fun = create_named_fun(Name, VarNames, FunBody, Bindings1),
      add_proc_binding(Name, Fun)
    end
    || {#c_var{name = Name}, #c_fun{vars = Vars, body = FunBody}} <- Defs
  ],
  expr_(Body, Bindings1).

%% -----------------------------------------------------------------------------
%% Case clauses
%% -----------------------------------------------------------------------------

-spec case_clauses([value()], cerl:cerl(), bindings()) -> value().
case_clauses(Values, #c_case{clauses = Clauses} = Expr, Bindings0) ->
  case match_clause(Values, Clauses, Bindings0) of
    {match, Body, Bindings1} ->
      expr_(Body, Bindings1);
    nomatch ->
      case Values of
        [V] -> raise({case_clause, V}, Expr);
        _   -> raise({case_clause, Values}, Expr)
      end
  end.

-spec match_clause(value(), [cerl:cerl()], bindings()) ->
  {match, cerl:cerl(), bindings()} | nomatch.
match_clause(Values, [Clause | Clauses], Bindings0) ->
  #c_clause{pats = Patterns, guard = Guard, body = Body} = Clause,
  case match_list(Values, Patterns, Bindings0) of
    {match, Bindings1} ->
      case guard(Guard, Bindings1) of
        true  -> {match, Body, Bindings1};
        false -> match_clause(Values, Clauses, Bindings0)
      end;
    nomatch ->
      match_clause(Values, Clauses, Bindings0)
  end;
match_clause(_Value, [], _Bindings) ->
  nomatch.

%% -----------------------------------------------------------------------------
%% Pattern Matching
%% -----------------------------------------------------------------------------

-spec check_c_values(cerl:cerl()) -> [cerl:cerl()].
check_c_values(#c_values{es = Elements}) ->
  Elements;
check_c_values(X) ->
  [X].

-spec expr_maybe_c_values(cerl:cerl(), bindings()) -> [cerl:cerl()].
expr_maybe_c_values(Expr, Bindings) ->
  try [expr_(Expr, Bindings)]
  catch throw:{values, Values} ->
      Values
  end.

-spec match_list([value()], [cerl:cerl()], bindings()) ->
  {match, bindings()} | nomatch.
match_list([Value | Values], [Pattern | Patterns], Bindings) ->
  %% TODO: we need to use merge_bindings here
  case match(Value, Pattern, Bindings) of
    {match, Bindings1} -> match_list(Values, Patterns, Bindings1);
    nomatch -> nomatch
  end;
match_list([], [], Bindings) ->
    {match, Bindings};
match_list(_, _, _Bindings) ->
    nomatch.

-spec match_map_pairs(map(), [cerl:c_map_pair()], bindings()) ->
  {match, bindings()} | nomatch.
match_map_pairs(Map, Pairs, Bindings) ->
  Fold = fun(#c_map_pair{op=#c_literal{val=exact}, key = K, val = V}, Acc0) ->
             Key = expr_(K, Bindings),
             Value = case maps:find(Key, Map) of
                       {ok, Value_} -> Value_;
                       error -> throw(nomatch)
                     end,
             {match, Acc1} = do_match(Value, V, Bindings),
             merge_bindings(Acc1, Acc0)
         end,
  Bindings1 = lists:foldl(Fold, Bindings, Pairs),
  {match, Bindings1}.

-spec match_map_literal(map(), map(), bindings()) ->
  {match, bindings()} | nomatch.
match_map_literal(#{} = Map, MapPattern, Bindings) ->
  Fun = fun(K, V) ->
            case maps:find(K, Map) of
              error -> throw(nomatch);
              {ok, _} -> V
            end
        end,
  maps:map(Fun, MapPattern),
  {match, Bindings}.

-spec match(value(), cerl:cerl(), bindings()) -> {match, bindings()} | nomatch.
match(Value, Pattern, Bindings) ->
  case catch do_match(Value, Pattern, Bindings) of
    invalid -> raise({illegal_pattern, Pattern}, Pattern);
    Other   -> Other
  end.

-spec do_match(value(), cerl:cerl(), bindings()) ->
  {match, bindings()} | nomatch.
%% Literal ---------------------------------------------------------------------
do_match(Value, #c_literal{val = Value}, Bindings) ->
  {match, Bindings};
do_match(_Value, #c_literal{}, _Bindings) ->
  throw(nomatch);
%% Var -------------------------------------------------------------------------
do_match(Value, #c_var{name = Name}, Bindings) ->
  %% Variables in patterns do not have to match the current
  %% bindings in Core Erlang
  {match, add_binding(Name, Value, Bindings)};
%% Cons ------------------------------------------------------------------------
do_match([Value | Values], #c_cons{hd = Head, tl = Tail}, Bindings0) ->
  {match, Bindings1} = do_match(Value, Head, Bindings0),
  do_match(Values, Tail, Bindings1);
do_match(_, #c_cons{}, _Bindings) ->
  throw(nomatch);
%% Map -------------------------------------------------------------------------
do_match( Value
        , #c_map{arg = #c_literal{val = Map}, es = Pairs}
        , Bindings0
        ) when is_map(Value) ->
  {match, Bindings1} = match_map_literal(Value, Map, Bindings0),
  match_map_pairs(Value, Pairs, Bindings1);
do_match(_, #c_map{is_pat = true}, _Bindings) ->
  throw(nomatch);
%% Tuple -----------------------------------------------------------------------
do_match(Tuple, #c_tuple{es = Elements}, Bindings) when is_tuple(Tuple) ->
  match_list(tuple_to_list(Tuple), Elements, Bindings);
do_match(_, #c_tuple{}, _Bindings) ->
  throw(nomatch);
%% Binary ----------------------------------------------------------------------
do_match(<<_/binary>> = Value, #c_binary{segments = Segments}, Bindings) ->
  match_bitstrings(Value, Segments, Bindings);
do_match(_, #c_binary{}, _Bindings) ->
  throw(nomatch);
do_match(Value, #c_alias{var = Var, pat = Pattern}, Bindings0) ->
  {match, Bindings1} = do_match(Value, Pattern, Bindings0),
  {match, add_binding(cerl:var_name(Var), Value, Bindings1)};
do_match(_, _, _) ->
  throw(invalid).

-spec match_bitstrings(value(), [cerl:cerl()], bindings()) ->
  {match, bindings()} | nomatch.
match_bitstrings(Value, Segments, Bindings0) ->
  Fold = fun(Segment, {Acc0, Bin}) ->
             {Acc1, Rest} = match_binary_segment(Bin, Segment, Bindings0),
             {merge_bindings(Acc1, Acc0), Rest}
         end,
  {Bindings1, _} = lists:foldl(Fold, {Bindings0, Value}, Segments),
  {match, Bindings1}.

-spec match_binary_segment(value(), cerl:cerl(), bindings()) ->
  {bindings(), value()}.
match_binary_segment( Value
                    , #c_bitstr{ val   = ValExpr
                               , size  = SizeExpr
                               , unit  = UnitExpr
                               , type  = TypeExpr
                               , flags = FlagsExpr
                               }
                    , Bindings0
                    ) ->
  Size   = expr_(SizeExpr, Bindings0),
  Unit   = expr_(UnitExpr, Bindings0),
  Type   = expr_(TypeExpr, Bindings0),
  Flags  = expr_(FlagsExpr, Bindings0),

  Endian = extract_endian(Flags),
  Sign   = extract_sign(Flags),

  {V, Rest} = get_value(Value, Type, Size, Unit, Sign, Endian),
  {match, Bindings1} = do_match(V, ValExpr, Bindings0),
  {Bindings1, Rest}.

get_value(Bin, integer, Size, Unit, Sign, Endian) ->
    get_integer(Bin, Size * Unit, Sign, Endian);
get_value(Bin, float, Size, Unit, _Sign, Endian) ->
    get_float(Bin, Size * Unit, Endian);
get_value(Bin, utf8, undefined, _Unit, _Sign, _Endian) ->
    <<I/utf8,Rest/bits>> = Bin,
    {I,Rest};
get_value(Bin, utf16, undefined, _Unit, _Sign, big) ->
    <<I/big-utf16,Rest/bits>> = Bin,
    {I,Rest};
get_value(Bin, utf16, undefined, _Unit, _Sign, little) ->
    <<I/little-utf16,Rest/bits>> = Bin,
    {I,Rest};
get_value(Bin, utf32, undefined, _Unit, _Sign, big) ->
    <<Val/big-utf32,Rest/bits>> = Bin,
    {Val,Rest};
get_value(Bin, utf32, undefined, _Unit, _Sign, little) ->
    <<Val/little-utf32,Rest/bits>> = Bin,
    {Val,Rest};
get_value(Bin, binary, all, Unit, _Sign, _Endian) ->
    0 = (bit_size(Bin) rem Unit),
    {Bin,<<>>};
get_value(Bin, binary, Size, Unit, _Sign, _Endian) ->
    TotSize = Size*Unit,
    <<Val:TotSize/bitstring,Rest/bits>> = Bin,
    {Val,Rest}.

get_integer(Bin, Size, signed, little) ->
    <<Val:Size/little-signed,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_integer(Bin, Size, unsigned, little) ->
    <<Val:Size/little,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_integer(Bin, Size, signed, native) ->
    <<Val:Size/native-signed,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_integer(Bin, Size, unsigned, native) ->
    <<Val:Size/native,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_integer(Bin, Size, signed, big) ->
    <<Val:Size/signed,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_integer(Bin, Size, unsigned, big) ->
    <<Val:Size,Rest/binary-unit:1>> = Bin,
    {Val,Rest}.

get_float(Bin, Size, little) ->
    <<Val:Size/float-little,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_float(Bin, Size, native) ->
    <<Val:Size/float-native,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_float(Bin, Size, big) ->
    <<Val:Size/float,Rest/binary-unit:1>> = Bin,
    {Val,Rest}.

-spec guard(cerl:cerl(), bindings()) -> value().
guard(Guard, Bindings) ->
  expr_(Guard, Bindings).

%% -----------------------------------------------------------------------------
%% List of expression
%% -----------------------------------------------------------------------------

-spec expr_list([cerl:cerl()], bindings()) -> [value()].
expr_list(Exprs, Bindings) ->
  Fun = fun(Expr, Values) ->
            Value = expr_(Expr, Bindings),
            [Value | Values]
        end,
  Values = lists:foldl(Fun, [], Exprs),
  lists:reverse(Values).

%% -----------------------------------------------------------------------------
%% BitStrings
%% -----------------------------------------------------------------------------

expr_bitstr(Val, Size, Unit, Type, Flags, Bindings) ->
  Value      = expr_(Val, Bindings),
  SizeValue  = expr_(Size, Bindings),
  UnitValue  = expr_(Unit, Bindings),
  TypeValue  = expr_(Type, Bindings),
  FlagsValue = expr_(Flags, Bindings),

  Endian     = extract_endian(FlagsValue),
  Sign       = extract_sign(FlagsValue),

  eval_exp_field(Value, SizeValue, UnitValue, TypeValue, Endian, Sign).

-spec extract_endian([atom()]) -> atom().
extract_endian([Endian | _Flags])
  when Endian == big; Endian == native; Endian == little ->
  Endian;
extract_endian([_Endian | Flags]) ->
  extract_endian(Flags);
extract_endian([]) ->
  big.

-spec extract_sign([atom()]) -> atom().
extract_sign([Sign | _Flags])
  when Sign == unsigned; Sign == signed ->
  Sign;
extract_sign([_ | Flags]) ->
  extract_sign(Flags);
extract_sign([]) ->
  signed.

eval_exp_field(Val, Size, Unit, integer, little, signed) ->
    <<Val:(Size*Unit)/little-signed>>;
eval_exp_field(Val, Size, Unit, integer, little, unsigned) ->
    <<Val:(Size*Unit)/little>>;
eval_exp_field(Val, Size, Unit, integer, native, signed) ->
    <<Val:(Size*Unit)/native-signed>>;
eval_exp_field(Val, Size, Unit, integer, native, unsigned) ->
    <<Val:(Size*Unit)/native>>;
eval_exp_field(Val, Size, Unit, integer, big, signed) ->
    <<Val:(Size*Unit)/signed>>;
eval_exp_field(Val, Size, Unit, integer, big, unsigned) ->
    <<Val:(Size*Unit)>>;
eval_exp_field(Val, _Size, _Unit, utf8, _, _) ->
    <<Val/utf8>>;
eval_exp_field(Val, _Size, _Unit, utf16, big, _) ->
    <<Val/big-utf16>>;
eval_exp_field(Val, _Size, _Unit, utf16, little, _) ->
    <<Val/little-utf16>>;
eval_exp_field(Val, _Size, _Unit, utf32, big, _) ->
    <<Val/big-utf32>>;
eval_exp_field(Val, _Size, _Unit, utf32, little, _) ->
    <<Val/little-utf32>>;
eval_exp_field(Val, Size, Unit, float, little, _) ->
    <<Val:(Size*Unit)/float-little>>;
eval_exp_field(Val, Size, Unit, float, native, _) ->
    <<Val:(Size*Unit)/float-native>>;
eval_exp_field(Val, Size, Unit, float, big, _) ->
    <<Val:(Size*Unit)/float>>;
eval_exp_field(Val, all, Unit, binary, _, _) ->
    case bit_size(Val) of
	Size when Size rem Unit =:= 0 ->
	    <<Val:Size/binary-unit:1>>;
	_ ->
	    raise(badarg)
    end;
eval_exp_field(Val, Size, Unit, binary, _, _) ->
    <<Val:(Size*Unit)/binary-unit:1>>.

%% -----------------------------------------------------------------------------
%% Fun
%% -----------------------------------------------------------------------------

-spec create_named_fun( cerl:var_name()
                      , [cerl:var_name()]
                      , cerl:cerl()
                      , bindings()
                      ) -> function().
create_named_fun(Name, Vars0, Body, Bindings) ->
  Vars1     = [Name | Vars0],
  case length(Vars0) of
    0  -> fun NF() -> eval_fun(Vars1, [NF], Body, Bindings) end;
    1  -> fun NF(A) -> eval_fun(Vars1, [NF, A], Body, Bindings) end;
    2  -> fun NF(A, B) -> eval_fun(Vars1, [NF, A, B], Body, Bindings) end;
    3  -> fun NF(A, B, C) -> eval_fun(Vars1, [NF, A, B, C], Body, Bindings) end;
    4  -> fun NF(A, B, C, D) ->
              eval_fun(Vars1, [NF, A, B, C, D], Body, Bindings)
          end;
    5  -> fun NF(A, B, C, D, E) ->
              eval_fun(Vars1, [NF, A, B, C, D, E], Body, Bindings)
          end;
    6  -> fun NF(A, B, C, D, E, F) ->
              eval_fun(Vars1, [NF, A, B, C, D, E, F], Body, Bindings)
          end;
    7  -> fun NF(A, B, C, D, E, F, G) ->
              eval_fun(Vars1, [NF, A, B, C, D, E, F, G], Body, Bindings)
          end;
    8  -> fun NF(A, B, C, D, E, F, G, H) ->
              eval_fun(Vars1, [NF, A, B, C, D, E, F, G, H], Body, Bindings)
          end;
    9  -> fun NF(A, B, C, D, E, F, G, H, I) ->
              eval_fun(Vars1, [NF, A, B, C, D, E, F, G, H, I], Body, Bindings)
          end;
    10 -> fun NF(A, B, C, D, E, F, G, H, I, J) ->
              Args = [NF, A, B, C, D, E, F, G, H, I, J],
              eval_fun(Vars1, Args, Body, Bindings)
          end;
    11 -> fun NF(A, B, C, D, E, F, G, H, I, J, K) ->
              Args = [NF, A, B, C, D, E, F, G, H, I, J, K],
              eval_fun(Vars1, Args, Body, Bindings)
          end;
    12 -> fun NF(A, B, C, D, E, F, G, H, I, J, K, L) ->
              Args = [NF, A, B, C, D, E, F, G, H, I, J, K, L],
              eval_fun(Vars1, Args, Body, Bindings)
          end;
    13 -> fun NF(A, B, C, D, E, F, G, H, I, J, K, L, M) ->
              Args = [NF, A, B, C, D, E, F, G, H, I, J, K, L, M],
              eval_fun(Vars1, Args, Body, Bindings)
          end;
    14 -> fun NF(A, B, C, D, E, F, G, H, I, J, K, L, M, N) ->
              Args = [NF, A, B, C, D, E, F, G, H, I, J, K, L, M, N],
              eval_fun(Vars1, Args, Body, Bindings)
          end;
    15 -> fun NF(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) ->
              Args = [NF, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O],
              eval_fun(Vars1, Args, Body, Bindings)
          end;
    16 -> fun NF(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) ->
              Args = [NF, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P],
              eval_fun(Vars1, Args, Body, Bindings)
          end;
    17 -> fun NF(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) ->
              Args = [NF, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q],
              eval_fun(Vars1, Args, Body, Bindings)
          end;
    18 -> fun NF(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) ->
              Args = [ NF, A, B, C, D, E, F, G, H, I
                     , J, K, L, M, N, O, P, Q, R
                     ],
              eval_fun(Vars1, Args, Body, Bindings)
          end;
    19 -> fun NF(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) ->
              Args = [ NF, A, B, C, D, E, F, G, H, I
                     , J, K, L, M, N, O, P, Q, R, S
                     ],
              eval_fun(Vars1, Args, Body, Bindings)
          end;
    20 -> fun NF(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) ->
              Args = [ NF, A, B, C, D, E, F, G, H, I
                     , J, K, L, M, N, O, P, Q, R, S, T
                     ],
              eval_fun(Vars1, Args, Body, Bindings)
          end
  end.

-spec create_fun([cerl:var_name()], cerl:cerl(), bindings()) -> function().
create_fun(Vars, Body, Bindings) ->
  case length(Vars) of
    0  -> fun() -> eval_fun(Vars, [], Body, Bindings) end;
    1  -> fun(A) -> eval_fun(Vars, [A], Body, Bindings) end;
    2  -> fun(A, B) -> eval_fun(Vars, [A, B], Body, Bindings) end;
    3  -> fun(A, B, C) -> eval_fun(Vars, [A, B, C], Body, Bindings) end;
    4  -> fun(A, B, C, D) ->
              eval_fun(Vars, [A, B, C, D], Body, Bindings)
          end;
    5  -> fun(A, B, C, D, E) ->
              eval_fun(Vars, [A, B, C, D, E], Body, Bindings)
          end;
    6  -> fun(A, B, C, D, E, F) ->
              eval_fun(Vars, [A, B, C, D, E, F], Body, Bindings)
          end;
    7  -> fun(A, B, C, D, E, F, G) ->
              eval_fun(Vars, [A, B, C, D, E, F, G], Body, Bindings)
          end;
    8  -> fun(A, B, C, D, E, F, G, H) ->
              eval_fun(Vars, [A, B, C, D, E, F, G, H], Body, Bindings)
          end;
    9  -> fun(A, B, C, D, E, F, G, H, I) ->
              eval_fun(Vars, [A, B, C, D, E, F, G, H, I], Body, Bindings)
          end;
    10 -> fun(A, B, C, D, E, F, G, H, I, J) ->
              Args = [A, B, C, D, E, F, G, H, I, J],
              eval_fun(Vars, Args, Body, Bindings)
          end;
    11 -> fun(A, B, C, D, E, F, G, H, I, J, K) ->
              Args = [A, B, C, D, E, F, G, H, I, J, K],
              eval_fun(Vars, Args, Body, Bindings)
          end;
    12 -> fun(A, B, C, D, E, F, G, H, I, J, K, L) ->
              Args = [A, B, C, D, E, F, G, H, I, J, K, L],
              eval_fun(Vars, Args, Body, Bindings)
          end;
    13 -> fun(A, B, C, D, E, F, G, H, I, J, K, L, M) ->
              Args = [A, B, C, D, E, F, G, H, I, J, K, L, M],
              eval_fun(Vars, Args, Body, Bindings)
          end;
    14 -> fun(A, B, C, D, E, F, G, H, I, J, K, L, M, N) ->
              Args = [A, B, C, D, E, F, G, H, I, J, K, L, M, N],
              eval_fun(Vars, Args, Body, Bindings)
          end;
    15 -> fun(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) ->
              Args = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O],
              eval_fun(Vars, Args, Body, Bindings)
          end;
    16 -> fun(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) ->
              Args = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P],
              eval_fun(Vars, Args, Body, Bindings)
          end;
    17 -> fun(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) ->
              Args = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q],
              eval_fun(Vars, Args, Body, Bindings)
          end;
    18 -> fun(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) ->
              Args = [ A, B, C, D, E, F, G, H, I
                     , J, K, L, M, N, O, P, Q, R
                     ],
              eval_fun(Vars, Args, Body, Bindings)
          end;
    19 -> fun(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) ->
              Args = [ A, B, C, D, E, F, G, H, I
                     , J, K, L, M, N, O, P, Q, R, S
                     ],
              eval_fun(Vars, Args, Body, Bindings)
          end;
    20 -> fun(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) ->
              Args = [ A, B, C, D, E, F, G, H, I, J
                     , K, L, M, N, O, P, Q, R, S, T
                     ],
              eval_fun(Vars, Args, Body, Bindings)
          end
  end.

-spec eval_fun([cerl:var_name()], [value()], cerl:cerl(), bindings()) ->
  value().
eval_fun(Vars, Values, Body, Bindings0) ->
  Bindings1 = add_bindings(Vars, Values, Bindings0),
  expr_(Body, Bindings1).

%% -----------------------------------------------------------------------------
%% Primitive Operation
%% -----------------------------------------------------------------------------

-spec primitive_op(atom(), [term()], cerl:cerl()) -> term().
primitive_op(raise, [X, Y], Expr) ->
  raise(Y, X, Expr);
primitive_op(match_fail, [Error], Expr) ->
  raise(error, Error, Expr);
primitive_op(Name, _, _Expr) ->
  raise({not_implemented, Name}).

%% -----------------------------------------------------------------------------
%% Primitive Operation
%% -----------------------------------------------------------------------------

-spec receive_clauses([cerl:cerl()], cerl:cerl(), cerl:cerl(), bindings()) ->
  value().
receive_clauses(Clauses, TimeoutExpr, Action, Bindings0) ->
  Timeout = expr_(TimeoutExpr, Bindings0),
  F = fun (Message) -> match_clause([Message], Clauses, Bindings0) end,
  case prim_eval:'receive'(F, Timeout) of
    {match, Body, Bindings1} -> expr_(Body, Bindings1);
    timeout -> expr_(Action, Bindings0)
  end.

%% -----------------------------------------------------------------------------
%% Error
%% -----------------------------------------------------------------------------

-spec raise(term()) -> no_return().
raise(Error) ->
  raise(Error, cerl:abstract(?NIL)).

-spec raise(term(), cerl:cerl()) -> no_return().
raise(Error, Expr) ->
  raise(error, Error, Expr).

-spec raise(atom(), term(), cerl:cerl()) -> no_return().
raise(Type, Error, Expr) ->
  Ann = cerl:get_ann(Expr),
  Stacktrace = [location(Ann)| ?STACKTRACE],
  erlang:raise(Type, Error, Stacktrace).

-spec location(list()) -> tuple().
location(Ann) ->
  Line = line(Ann),
  File = file(Ann),
  {core_eval, expr, 1, [{line, Line}, {file, File}]}.

line([]) ->
  ?NIL;
line([Line | _Rest]) when is_integer(Line) ->
  Line;
line([_ | Rest]) ->
  line(Rest).

file([]) ->
  "?";
file([{file, File} | _Rest]) ->
  File;
file([_ | Rest]) ->
  file(Rest).
