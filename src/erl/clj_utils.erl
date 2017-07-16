-module(clj_utils).

-include("clojerl.hrl").

-dialyzer([ {nowarn_function, throw/2}
          , {nowarn_function, throw_when/2}
          , {nowarn_function, throw_when/3}
          , {nowarn_function, error/1}
          , {nowarn_function, error/2}
          , {nowarn_function, error_when/2}
          , {nowarn_function, error_when/3}
          ]).

-compile({no_auto_import, [throw/1, error/1]}).

-export([ char_type/1
        , char_type/2
        , parse_number/1
        , parse_symbol/1
        , desugar_meta/1

        , compare/2

        , throw/1
        , throw/2
        , throw_when/2
        , throw_when/3
        , error/1
        , error/2
        , error_when/2
        , error_when/3
        , warn_when/2
        , warn_when/3

        , group_by/2
        , nth/2
        , nth/3

        , time/1
        , time/2
        , time/3

        , store_binary/2
        , add_core_to_binary/2
        , code_from_binary/1

        , 'rem'/2
        , floor/1
        , ceil/1
        , signum/1

        , ets_get/2
        , ets_save/2
        ]).

-define(CORE_CHUNK, "Core").

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
             ?NIL -> ?NIL
           end,

  case Result of
    ?NIL ->
      throw(<<"Invalid number format [", Number/binary, "]">>);
    _ ->
      Result
  end.

-spec parse_symbol(binary()) ->
  {Ns :: 'clojerl.Symbol':type(), Name :: 'clojerl.Symbol':type()} | ?NIL.
parse_symbol(<<>>) ->
  ?NIL;
parse_symbol(<<"::"/utf8, _/binary>>) ->
  ?NIL;
parse_symbol(<<"/">>) ->
  {?NIL, <<"/">>};
parse_symbol(Str) ->
  case binary:last(Str) of
    $: -> ?NIL;
    _ ->
      case binary:split(Str, <<"/">>) of
        [_Namespace, <<>>] ->
          ?NIL;
        [Namespace, <<"/">>] ->
          {Namespace, <<"/">>};
        [Namespace, Name] ->
          verify_symbol_name({Namespace, Name});
        [Name] ->
          verify_symbol_name({?NIL, Name})
      end
  end.

verify_symbol_name({_, Name} = Result) ->
  NotNumeric = fun(<<C/utf8, _/binary>>) -> char_type(C) =/= number end,
  NoEndColon = fun(X) -> binary:last(X) =/= $: end,
  NoDoubleSlash = fun(X) -> re:run(X, <<"/.*?/">>) == nomatch end,
  ApplyPred = fun(Fun) -> Fun(Name) end,
  case lists:all(ApplyPred, [NotNumeric, NoEndColon, NoDoubleSlash]) of
    true -> Result;
    false -> ?NIL
  end.

-spec char_type(non_neg_integer()) -> char_type().
char_type(X) -> char_type(X, ?NIL).

-spec char_type(non_neg_integer(), integer() | ?NIL) -> char_type().
char_type(X, _)
  when X == $\n; X == $\t; X == $\r; X == $ ; X == $,->
  whitespace;
char_type(X, _)
  when X >= $0, X =< $9 ->
  number;
char_type(X, Y)
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
  case clj_rt:type_module(Meta) of
    'clojerl.Keyword' ->
      clj_rt:hash_map([Meta, true]);
    'clojerl.Map' ->
      Meta;
    Type when Type == 'clojerl.Symbol' orelse
              Type == 'clojerl.String' ->
      Tag = clj_rt:keyword(<<"tag">>),
      clj_rt:hash_map([Tag, Meta]);
    _ ->
      throw(<<"Metadata must be Symbol, Keyword, String or Map">>)
  end.

-spec compare(any(), any()) -> integer().
compare(X, Y) ->
  if
    X <  Y -> -1;
    X == Y -> 0;
    X >  Y -> 1
  end.

-spec throw(any()) -> no_return().
throw(Reason) ->
  throw(Reason, ?NIL).

-spec throw(any(), ?NIL | clj_reader:location()) -> no_return().
throw(List, Location) ->
  throw_when(true, List, Location).

-spec throw_when(true,  any()) -> no_return();
                (false, any()) -> ok.
throw_when(Throw, Reason) ->
  throw_when(Throw, Reason, ?NIL).

-spec throw_when(true,  any(), clj_reader:location() | ?NIL) -> no_return();
                (false, any(), clj_reader:location() | ?NIL) -> ok.
throw_when(false, _, _) ->
  ok;
throw_when(true, List, Location) when is_list(List) ->
  Reason = error_msg_to_binary(List),
  throw_when(true, Reason, Location);
throw_when(true, Reason, Location) when is_binary(Reason) ->
  LocationBin = location_to_binary(Location),
  erlang:throw(<<LocationBin/binary, Reason/binary>>);
throw_when(true, Reason, Location) ->
  erlang:throw({Location, Reason}).

-spec error(any()) -> no_return().
error(List) ->
  error_when(true, List, ?NIL).

-spec error(any(), clj_reader:location() | ?NIL) -> no_return().
error(List, Location) ->
  error_when(true, List, Location).

-spec error_when(true,  any()) -> no_return();
                (false, any()) -> ok.
error_when(Throw, Reason) ->
  error_when(Throw, Reason, ?NIL).

-spec error_when(true,  any(), clj_reader:location() | ?NIL) -> no_return();
                (false, any(), clj_reader:location() | ?NIL) -> ok.
error_when(false, _, _) ->
  ok;
error_when(true, List, Location) when is_list(List) ->
  Reason = error_msg_to_binary(List),
  error_when(true, Reason, Location);
error_when(true, Reason, Location) when is_binary(Reason) ->
  LocationBin = location_to_binary(Location),
  erlang:error(<<LocationBin/binary, Reason/binary>>);
error_when(true, Reason, Location) ->
  erlang:error({Location, Reason}).

-spec warn_when(boolean(), any()) -> ok.
warn_when(Warn, Reason) ->
  warn_when(Warn, Reason, ?NIL).

-spec warn_when(boolean(), any(), clj_reader:location() | ?NIL) -> ok.
warn_when(false, _, _) ->
  ok;
warn_when(true, List, Location) when is_list(List) ->
  Reason = error_msg_to_binary(List),
  warn_when(true, Reason, Location);
warn_when(true, Reason, Location) when is_binary(Reason) ->
  LocationBin = location_to_binary(Location),
  'erlang.io.IWriter':write( 'clojure.core':'*err*__val'()
                           , <<LocationBin/binary, Reason/binary, "\n">>
                           ),
  ok;
warn_when(true, Reason, Location) ->
  'erlang.io.IWriter':write( 'clojure.core':'*err*__val'()
                           , <<"~p~n">>
                           , [{Location, Reason}]
                           ),
  ok.

-spec error_msg_to_binary(any()) -> binary().
error_msg_to_binary(Message) when is_binary(Message) ->
  Message;
error_msg_to_binary(Message) when is_list(Message) ->
  erlang:iolist_to_binary(lists:map(fun error_msg_to_binary/1, Message));
error_msg_to_binary(Message) ->
  clj_rt:str(Message).

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

-spec store_binary(module(), binary()) -> ok.
store_binary(Name, Binary) ->
  clj_cache:put({beam, Name}, Binary).

-spec add_core_to_binary(binary(), cerl:cerl()) -> binary().
add_core_to_binary(BeamBinary, CoreModule) ->
  CoreAbstract        = erlang:term_to_binary(CoreModule, [compressed]),
  CoreAbstractChunk   = {?CORE_CHUNK, CoreAbstract},
  {ok, _, OldChunks}  = beam_lib:all_chunks(BeamBinary),
  {ok, NewBeamBinary} = beam_lib:build_module(OldChunks ++ [CoreAbstractChunk]),
  NewBeamBinary.

-spec code_from_binary(atom()) -> cerl:cerl() | {error, term()}.
code_from_binary(Name) when is_atom(Name) ->
  case code:get_object_code(Name) of
    {Name, Binary, _} ->
      core_from_binary(Binary);
    _ ->
      case clj_cache:get({beam, Name}) of
        {ok, Binary} -> core_from_binary(Binary);
        _ ->
          error([ <<"Could not load object code for namespace: ">>
                , atom_to_binary(Name, utf8)
                ])
      end
  end.

-spec core_from_binary(binary()) ->
  cerl:cerl() | {error, missing_abstract_code}.
core_from_binary(Binary) ->
  ChunkNames = [?CORE_CHUNK, abstract_code],
  ChunkOpts  = [allow_missing_chunks],
  {ok, {_, Chunks}} = beam_lib:chunks(Binary, ChunkNames, ChunkOpts),
  case proplists:get_value(?CORE_CHUNK, Chunks) of
    missing_chunk ->
      case proplists:get_value(abstract_code, Chunks) of
        %% This case is only for bootstrapping clojure.core since it
        %% is written in Erlang it has erlang abstract syntax forms.
        {raw_abstract_v1, Code0} ->
          %% Since OTP 20 there is no sys_pre_expand module.
          {Code1, Opts1} = case code:ensure_loaded(sys_pre_expand) of
                           {module, _} ->
                             { Mod
                             , Exp
                             , Forms
                             , Opts0
                             } = sys_pre_expand:module(Code0, []),
                             {{Mod, Exp, Forms}, Opts0};
                           {error, _} ->
                             {Code0, []}
                         end,
          {ok, CoreModule, _} = v3_core:module(Code1, Opts1),
          CoreModule;
        missing_chunk ->
          {error, missing_abstract_code}
        end;
    CoreModule ->
      erlang:binary_to_term(CoreModule)
  end.

%%------------------------------------------------------------------------------
%% Internal helper functions
%%------------------------------------------------------------------------------

%% @doc Valid integers can be either in decimal, octal, hexadecimal or any
%%      base specified (e.g. `2R010` is binary for `2`).
-spec parse_int(binary()) -> integer() | ?NIL.
parse_int(IntBin) ->
  {match, [_ | Groups]} = re:run(IntBin, ?INT_PATTERN, [{capture, all, list}]),
  case int_properties(Groups) of
    {zero, _Arbitrary, _Negate} ->
      0;
    {{Base, Value}, _Arbitrary, Negate} ->
      list_to_integer(Value, Base) * Negate;
    _ ->
      ?NIL
  end.

-spec int_properties([string()]) ->
  {zero | ?NIL | {integer(), string()}, integer(), -1 | 1}.
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
        ?NIL
    end,

  Arbitrary = nth(8, Props, false),
  Negate = nth(1, Props),
  {Result, Arbitrary, case Negate of true -> -1; false -> 1 end}.

-spec parse_float(binary()) -> float().
parse_float(FloatBin) ->
  {match, [_ | Groups]} =
    re:run(FloatBin, ?FLOAT_PATTERN, [{capture, all, list}]),

  %% When there is no decimal part we add it so we can use
  %% list_to_float/1.
  FloatStr = case nth(3, Groups, "") of
               ""  -> nth(2, Groups) ++ ".0" ++ nth(4, Groups, "");
               "." -> nth(2, Groups) ++ ".0" ++ nth(4, Groups, "");
               _   -> nth(1, Groups)
             end,

  list_to_float(FloatStr).

-type ratio() :: {ratio, integer(), integer()}.

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
    [] -> ?NIL;
    [T | _] -> T
  end.

%% @doc Like lists:nth/2 but returns nil if `Index` is
%%      larger than the amount of elements in `List`.
nth(Index, List) ->
  nth(Index, List, ?NIL).

nth(Index, List, Default) ->
  case Index =< length(List) of
    true -> lists:nth(Index, List);
    false -> Default
  end.

-spec location_to_binary(?NIL | clj_reader:location()) -> binary().
location_to_binary(#{line := Line, column := Col, file := Filename})
  when is_integer(Line) andalso is_integer(Col) ->
  LineBin     = integer_to_binary(Line),
  ColBin      = integer_to_binary(Col),
  FilenameBin = case Filename of
                  ?NIL -> <<?NO_SOURCE>>;
                  _ -> Filename
                end,
  <<FilenameBin/binary, ":", LineBin/binary, ":", ColBin/binary, ": ">>;
location_to_binary(#{line := Line, column := Col} = Location)
  when is_integer(Line) andalso is_integer(Col) ->
  location_to_binary(Location#{file => ?NIL});
location_to_binary(#{file := Filename})
  when is_binary(Filename) ->
  <<Filename/binary, ":?:?: ">>;
location_to_binary(_) ->
  <<?NO_SOURCE, ":?:?: ">>.

-spec 'rem'(number(), number()) -> number().
'rem'(X, Y) when is_integer(X), is_integer(Y) ->
  X rem Y;
'rem'(X, Y) ->
  Q = trunc(X / Y),
  X - Q * Y.

-spec floor(number()) -> number().
floor(X) when X < 0 ->
  T = trunc(X),
  case X - T == 0 of
    true  -> T;
    false -> T - 1
  end;
floor(X) ->
  trunc(X).

-spec ceil(number()) -> number().
ceil(X) when X < 0 ->
  trunc(X);
ceil(X) ->
  T = trunc(X),
  case X - T == 0 of
    true  -> T;
    false -> T + 1
  end.

-spec signum(number()) -> number().
signum(X) when X < 0 -> -1;
signum(X) when X >= 0 -> 1.

-spec ets_get(atom() | ets:tid(), term()) -> term().
ets_get(Table, Id) ->
  case ets:lookup(Table, Id) of
    [] -> ?NIL;
    [Value] -> Value
  end.

-spec ets_save(ets:tid() | atom(), term()) -> term().
ets_save(Table, Value) ->
  true = ets:insert(Table, Value),
  Value.
