-module(clj_utils).

-export([
         char_type/1,
         char_type/2,
         parse_number/1,
         parse_symbol/1,
         desugar_meta/1
        ]).

-define(INT_PATTERN,
        "^([-+]?)"
        "(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|"
        "([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?$").
-define(FLOAT_PATTERN, "^(([-+]?[0-9]+)(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?$").
-define(RATIO_PATTERN, "^([-+]?[0-9]+)/([0-9]+)$").

-type char_type() :: whitespace | number | string
                   | keyword | comment | quote
                   | deref | meta | syntax_quote
                   | unquote | list | vector
                   | map | unmatched_delim | char
                   | unmatched_delim | char
                   | arg | dispatch | symbol.

%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------

-spec parse_number(binary()) -> integer() | float() | ratio().
parse_number(Number) ->
  Result = case number_type(Number) of
             int       -> parse_int(Number);
             float     -> parse_float(Number);
             ratio     -> parse_ratio(Number);
             undefined -> undefined
           end,

  case Result of
    undefined ->
      throw(<<"Invalid number format [", Number/binary, "]">>);
    _ ->
      Result
  end.

-spec parse_symbol(binary()) ->
  {Ns :: 'clojerl.Symbol':type(), Name :: 'clojerl.Symbol':type()}.
parse_symbol(<<>>) ->
  undefined;
parse_symbol(<<"::", _/binary>>) ->
  undefined;
parse_symbol(<<"/">>) ->
  {undefined, <<"/">>};
parse_symbol(Str) ->
  case binary:last(Str) of
    $: -> undefined;
    _ ->
      case binary:split(Str, <<"/">>) of
        [_Namespace, <<>>] ->
          undefined;
        [Namespace, <<"/">>] ->
          {Namespace, <<"/">>};
        [Namespace, Name] ->
          verify_symbol_name({Namespace, Name});
        [Name] ->
          verify_symbol_name({undefined, Name})
      end
  end.

verify_symbol_name({_, Name} = Result) ->
  NotNumeric = fun(<<C, _/binary>>) -> char_type(C) =/= number end,
  NoEndColon = fun(X) -> binary:last(X) =/= $: end,
  NoSlash = fun(X) -> binary:match(X, <<"/">>) == nomatch end,
  ApplyPred = fun(Fun) -> Fun(Name) end,
  case lists:all(ApplyPred, [NotNumeric, NoEndColon, NoSlash]) of
    true -> Result;
    false -> undefined
  end.

-spec char_type(non_neg_integer()) -> char_type().
char_type(X) -> char_type(X, <<>>).

-spec char_type(non_neg_integer(), binary()) -> char_type().
char_type(X, _)
  when X == $\n; X == $\t; X == $\r; X == $ ; X == $,->
  whitespace;
char_type(X, _)
  when X >= $0, X =< $9 ->
  number;
char_type(X, <<Y, _/binary>>)
  when (X == $+ orelse X == $-),
       Y >= $0, Y =< $9 ->
  number;
char_type($", _) -> string;
char_type($:, _) -> keyword;
char_type($;, _) -> comment;
char_type($', _) -> quote;
char_type($@, _) -> deref;
char_type($^, _) -> meta;
char_type($`, _) -> syntax_quote;
char_type($~, _) -> unquote;
char_type($(, _) -> list;
char_type($[, _) -> vector;
char_type(${, _) -> map;
char_type(X, _)
  when X == $); X == $]; X == $} ->
  unmatched_delim;
char_type($\\, _) -> char;
char_type($%, _) -> arg;
char_type($#, _) -> dispatch;
char_type(_, _) -> symbol.

-spec desugar_meta('clojerl.Map':type() |
                   'clojerl.Keyword':type() |
                   'clojerl.Symbol':type() |
                   string()) -> map().
desugar_meta(Meta) ->
  case 'clojerl.Keyword':is(Meta) of
    true ->
      maps:put(Meta, true, #{});
    false ->
      case 'clojerl.Symbol':is(Meta) orelse is_binary(Meta) of
        true ->
          Tag = 'clojerl.Keyword':new(<<"tag">>),
          maps:put(Tag, Meta, #{});
        false ->
          Meta
      end
  end.

%%------------------------------------------------------------------------------
%% Internal helper functions
%%------------------------------------------------------------------------------

%% @doc Valid integers can be either in decimal, octal, hexadecimal or any
%%      base specified (e.g. `2R010` is binary for `2`).
-spec parse_int(binary()) -> integer() | undefined.
parse_int(IntBin) ->
  {match, [_ | Groups]} = re:run(IntBin, ?INT_PATTERN, [{capture, all, list}]),
  case int_properties(Groups) of
    {zero, _Arbitrary, _Negate} ->
      0;
    {{Base, Value}, _Arbitrary, Negate} ->
      list_to_integer(Value, Base) * Negate;
    _ ->
      undefined
  end.

-spec int_properties([string()]) -> {zero | undefined | {integer(), string()},
                                     boolean(),
                                     boolean()}.
int_properties(Groups) ->
  Props = lists:map(fun(X) -> X =/= "" end, Groups),
  Result =
    case Props of
      [_, true | _] -> zero;
      [_, _, true | _]-> {10, nth(3, Groups)};
      [_, _, _, true | _]-> {16, nth(4, Groups)};
      [_, _, _, _, true | _]-> {8, nth(5, Groups)};
      [_, _, _, _, _, _, true | _]->
        Base = list_to_integer(lists:nth(6, Groups)),
        {Base, lists:nth(7, Groups)};
      _ ->
        undefined
    end,

  Arbitrary = nth(8, Props, false),
  Negate = nth(1, Props),
  {Result, Arbitrary, case Negate of true -> -1; false -> 1 end}.

-spec parse_float(binary()) -> float().
parse_float(FloatBin) ->
  {match, [_ | Groups]} =
    re:run(FloatBin, ?FLOAT_PATTERN, [{capture, all, list}]),
  Decimal = nth(3, Groups, "") =/= "",
  case Decimal of
    true ->
      FloatStr = nth(1, Groups),
      list_to_float(FloatStr);
    false ->
      %% When there is no decimal part we add it so we can use
      %% list_to_float/1.
      FloatStr = nth(2, Groups) ++ ".0" ++ nth(4, Groups),
      list_to_float(FloatStr)
  end.

-type ratio() :: #{type => ratio,
                   denom => integer(),
                   enum => integer()}.

-spec parse_ratio(binary()) -> ratio().
parse_ratio(RatioBin) ->
  {match, [_ | Groups]} =
    re:run(RatioBin, ?RATIO_PATTERN, [{capture, all, list}]),
  Numerator = nth(1, Groups),
  Denominator = nth(2, Groups),
  {ratio,
   list_to_integer(Numerator),
   list_to_integer(Denominator)}.

number_type(Number) ->
  Regex = #{int   => ?INT_PATTERN,
            float => ?FLOAT_PATTERN,
            ratio => ?RATIO_PATTERN},
  Fun = fun(Type, RE, Acc) ->
            case re:run(Number, RE) of
              nomatch -> Acc;
              _ -> [Type | Acc]
            end
        end,
  case maps:fold(Fun, [], Regex) of
    [] -> undefined;
    [T | _] -> T
  end.

%% @doc Like lists:nth/2 but returns `undefined` if `Index` is
%%      larger than the amount of elements in `List`.
nth(Index, List) ->
  nth(Index, List, undefined).

nth(Index, List, Default) ->
  case Index =< length(List) of
    true -> lists:nth(Index, List);
    false -> Default
  end.
