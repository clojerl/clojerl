-module(clj_utils).

-export([
         parse_number/1
        ]).

-define(INT_PATTERN,
        "^([-+]?)"
        "(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|"
        "([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?$").
-define(FLOAT_PATTERN, "^(([-+]?[0-9]+)(\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?$").
-define(RATIO_PATTERN, "^([-+]?[0-9]+)/([0-9]+)$").

-type ratio() :: {ratio, integer(), integer()}.

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
      [_, _, _, _, _ ,_, true | _]->
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
  {match, [_ | Groups]} = re:run(FloatBin, ?FLOAT_PATTERN, [{capture, all, list}]),
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

-spec parse_ratio(binary()) -> ratio().
parse_ratio(RatioBin) ->
  {match, [_ | Groups]} = re:run(RatioBin, ?RATIO_PATTERN, [{capture, all, list}]),
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
