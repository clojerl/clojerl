-module(clj_utils).

-compile({no_auto_import, [throw/1, error/1]}).

-export([ char_type/1
        , char_type/2
        , parse_number/1
        , parse_symbol/1
        , desugar_meta/1
        , binary_append/2
        , binary_join/2
        , ends_with/2
        , error/1
        , throw/1
        , throw/2
        , throw_when/2
        , throw_when/3
        , warn_when/2
        , warn_when/3
        , group_by/2
        , trace_while/2
        , time/1
        , time/2
        , time/3
        , bench/3
        , bench/4
        , code_from_binary/1
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
parse_symbol(<<"::"/utf8, _/binary>>) ->
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
  NotNumeric = fun(<<C/utf8, _/binary>>) -> char_type(C) =/= number end,
  NoEndColon = fun(X) -> binary:last(X) =/= $: end,
  NoDoubleSlash = fun(X) -> re:run(X, <<"/.*?/">>) == nomatch end,
  ApplyPred = fun(Fun) -> Fun(Name) end,
  case lists:all(ApplyPred, [NotNumeric, NoEndColon, NoDoubleSlash]) of
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
  case clj_core:type(Meta) of
    'clojerl.Keyword' ->
      clj_core:hash_map([Meta, true]);
    'clojerl.Map' ->
      Meta;
    Type when Type == 'clojerl.Symbol'
              orelse Type == 'clojerl.String' ->
      Tag = clj_core:keyword(<<"tag">>),
      clj_core:hash_map([Tag, Meta]);
    _ ->
      throw(<<"Metadata must be Symbol, Keyword, String or Map">>)
  end.

-spec binary_append([binary()], binary()) -> binary().
binary_append(X, Y) when is_binary(X), is_binary(Y) ->
  <<X/binary, Y/binary>>.

-spec binary_join([binary()], binary()) -> binary().
binary_join([], _) ->
  <<>>;
binary_join([S], _) when is_binary(S) ->
  S;
binary_join([H | T], Sep) ->
  B = << <<Sep/binary, X/binary>> || X <- T >>,
  <<H/binary, B/binary>>.

-spec ends_with(binary(), binary()) -> ok.
ends_with(Str, Ends) when size(Ends) > size(Str)->
  false;
ends_with(Str, Ends) ->
  StrSize = byte_size(Str),
  EndsSize = byte_size(Ends),
  Ends == binary:part(Str, {StrSize, - EndsSize}).

-spec error(any()) -> no_return().
error(List) when is_list(List) ->
  Reason = erlang:iolist_to_binary(lists:map(fun clj_core:str/1, List)),
  error(Reason);
error(Reason) when is_binary(Reason) ->
  erlang:error(Reason).

-spec throw(any()) -> no_return().
throw(Reason) ->
  throw(Reason, undefined).

-spec throw(boolean(), undefined | clj_reader:location()) -> no_return().
throw(List, Location) when is_list(List) ->
  Reason = erlang:iolist_to_binary(lists:map(fun clj_core:str/1, List)),
  throw(Reason, Location);
throw(Reason, Location) when is_binary(Reason) ->
  LocationBin = location_to_binary(Location),
  erlang:throw(<<LocationBin/binary, Reason/binary>>);
throw(Reason, Location) ->
  erlang:throw({Location, Reason}).

-spec throw_when(boolean(), any()) -> ok | no_return().
throw_when(Throw, Reason) ->
  throw_when(Throw, Reason, undefined).

-spec throw_when(boolean(), any(), clj_reader:location()) -> ok | no_return().
throw_when(true, List, Location) when is_list(List) ->
  Reason = erlang:iolist_to_binary(lists:map(fun clj_core:str/1, List)),
  throw_when(true, Reason, Location);
throw_when(true, Reason, Location) ->
  throw(Reason, Location);
throw_when(false, _, _) ->
  ok.

-spec warn_when(boolean(), any()) -> ok | no_return().
warn_when(Warn, Reason) ->
  warn_when(Warn, Reason, undefined).

-spec warn_when(boolean(), any(), clj_reader:location()) -> ok | no_return().
warn_when(true, List, Location) when is_list(List) ->
  Reason = erlang:iolist_to_binary(lists:map(fun clj_core:str/1, List)),
  warn_when(true, Reason, Location);
warn_when(true, Reason, Location) when is_binary(Reason) ->
  LocationBin = location_to_binary(Location),
  error_logger:warning_msg(<<LocationBin/binary, Reason/binary>>);
warn_when(true, Reason, Location) ->
  error_logger:warning_msg({Location, Reason});
warn_when(false, _, _) ->
  ok.

-spec group_by(fun((any()) -> any()), list()) -> map().
group_by(GroupBy, List) ->
  Group = fun(Item, Acc) ->
              Key = GroupBy(Item),
              Items = maps:get(Key, Acc, []),
              Acc#{Key => [Item | Items]}
          end,
  Map = lists:foldl(Group, #{}, List),
  ReverseValue = fun(_, V) -> lists:reverse(V) end,
  maps:map(ReverseValue, Map).

-spec trace_while(string(), function()) -> ok.
trace_while(Filename, Fun) ->
  Self = self(),
  F = fun() ->
          Self ! start,
          Fun(),
          Self ! stop
      end,
  spawn(F),

  receive start -> ok
  after 1000 -> throw(<<"Fun never started">>)
  end,

  eep:start_file_tracing(Filename),

  receive stop -> ok
  after 5000 -> ok
  end,

  eep:stop_tracing(),
  eep:convert_tracing(Filename).

-spec time(function()) -> ok.
time(Fun) when is_function(Fun) ->
  time("Time", Fun).

-spec time(string(), function()) -> ok.
time(Label, Fun) when is_function(Fun) ->
  time(Label, Fun, []).

-spec time(string(), function(), list()) -> ok.
time(Label, Fun, Args) ->
  {T, V} = timer:tc(fun() -> apply(Fun, Args) end),
  io:format("~s: ~p ms~n", [Label, T / 1000]),
  V.

bench(Name, Fun, Trials) ->
  bench(Name, Fun, [], Trials).

bench(Name, Fun, Args, Trials) ->
    print_result(Name, repeat_tc(Fun, Args, Trials)).

repeat_tc(Fun, Args, Trials) ->
  Repeat = fun
             R(0) -> ok;
             R(N) -> apply(Fun, Args), R(N - 1)
           end,

    {Time, _} = timer:tc(fun() -> Repeat(Trials) end),
    {Time, Trials}.


print_result(Name, {Time, Trials}) ->
    io:format("~s: ~.3f ms (~.2f per second)~n",
              [Name, (Time / 1000) / Trials, Trials / (Time / 1000000)]).

-spec code_from_binary(atom()) -> [erl_parse:abstract_form()] | {error, term()}.
code_from_binary(Name) when is_atom(Name) ->
  case code:get_object_code(Name) of
    {Name, Binary, _} ->
      case beam_lib:chunks(Binary, [abstract_code]) of
        {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
          Forms;
        Error ->
          Error
      end;
    _ ->
      clj_utils:throw([ <<"Could not load object code for namespace: ">>
                      , atom_to_binary(Name, utf8)
                      ])
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

-spec location_to_binary(undefined | clj_reader:location()) -> binary().
location_to_binary(#{loc := {Line, Col}, file := Filename}) ->
  LineBin     = integer_to_binary(Line),
  ColBin      = integer_to_binary(Col),
  FilenameBin = case Filename of
                  undefined -> <<"?">>;
                  _ -> Filename
                end,
  <<FilenameBin/binary, ":", LineBin/binary, ":", ColBin/binary, ": ">>;
location_to_binary(#{loc := {_Line, _Col}} = Location) ->
  location_to_binary(Location#{file => undefined});
location_to_binary(_) ->
  <<"?:?:?: ">>.
