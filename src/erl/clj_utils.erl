-module(clj_utils).

-include("clojerl.hrl").
-include("clojerl_int.hrl").
-include("clojerl_expr.hrl").

-compile({no_auto_import, [throw/1, error/1]}).

-export([ char_type/1
        , char_type/2
        , parse_number/2
        , parse_symbol/1
        , check_erl_fun/1
        , parse_erl_fun/1
        , desugar_meta/1

        , compare/2

        , env_vars/0

        , format_error/2
        , error_str/2

        , group_by/2
        , nth/2
        , nth/3

        , time/1
        , time/2
        , time/3

        , store_binary/2
        , add_core_to_binary/2
        , add_compile_info_to_binary/2
        , code_from_binary/1
        , core_chunk/0

        , 'rem'/2
        , quotient/2
        , floor/1
        , ceil/1
        , signum/1
        , bnand/2

        , ets_get/2
        , ets_get/3
        , ets_save/2

        , format_stacktrace/1
        , format_stacktrace/2

        , resource_to_ns/1
        , ns_to_resource/1

        , record_hash/1
        ]).

-define(CORE_CHUNK, "Core").
-define(COMPILE_INFO_CHUNK, "CInf").

-type char_type() :: whitespace | number | string
                   | keyword | comment | quote
                   | deref | meta | syntax_quote
                   | unquote | list | vector
                   | map | unmatched_delim | char
                   | unmatched_delim | char
                   | arg | dispatch | symbol.

-type number_type() :: int | float.

%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------

-spec parse_number(binary(), [{number_type(), re:mp()}]) -> number().
parse_number(Number, Types) ->
  Result = case number_type(Number, Types) of
             {int, Groups}   -> parse_int(Groups);
             {float, Groups} -> parse_float(Groups);
             ?NIL -> ?NIL
           end,

  ?ERROR_WHEN( Result =:= ?NIL
             , <<"Invalid number format [", Number/binary, "]">>
             ),

  Result.

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
  NoEndColon = fun(X) -> binary:last(X) =/= $: end,
  NoDoubleSlash = fun(X) -> re:run(X, <<"/.*?/">>) == nomatch end,
  ApplyPred = fun(Fun) -> Fun(Name) end,
  case lists:all(ApplyPred, [NoEndColon, NoDoubleSlash]) of
    true -> Result;
    false -> ?NIL
  end.

-spec check_erl_fun(erl_fun_expr()) -> ok.
check_erl_fun(Expr) ->
  #{ op       := erl_fun
   , env      := Env
   , form     := Symbol
   , module   := Module
   , function := Function
   , arity    := Arity
   } = Expr,

  IsSymbol         = clj_rt:'symbol?'(Symbol),
  FunctionExported = erlang:function_exported(Module, Function, Arity),
  NoWarnErlFun     = clj_compiler:no_warn_symbol_as_erl_fun(Env),
  ?WARN_WHEN( IsSymbol
              andalso not FunctionExported
              andalso not NoWarnErlFun
            , [ <<"'">>, Symbol, <<"'">>
              , <<" will be considered an Erlang function,">>
              , <<" but we couldn't find it at compile-time.">>
              , <<" Either make sure the module is loaded or use">>
              , <<" '#erl ">>, Symbol, <<"' to remove this warning.">>
              ]
            , clj_env:location(Env)
            ).

-spec parse_erl_fun('clojerl.Symbol':type()) ->
  {binary() | ?NIL, binary(), integer() | ?NIL}.
parse_erl_fun(Symbol) ->
  NsName        = 'clojerl.Symbol':namespace(Symbol),
  {Name, Arity} = erl_fun_arity('clojerl.Symbol':name(Symbol)),
  {NsName, Name, Arity}.

-spec erl_fun_arity(binary()) -> {binary(), ?NIL | integer()}.
erl_fun_arity(Name) ->
  case binary:split(Name, <<".">>, [global]) of
    [_] -> {Name, ?NIL};
    Parts ->
      Last = lists:last(Parts),
      case re:run(Last, <<"\\d+">>, [{capture, none}]) of
        nomatch ->
          {Name, ?NIL};
        _ ->
          NameParts = 'clojerl.String':join(lists:droplast(Parts), <<".">>),
          Arity = binary_to_integer(Last),
          {iolist_to_binary(NameParts), Arity}
      end
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
      ?THROW(<<"Metadata must be Symbol, Keyword, String or Map">>)
  end.

-spec compare(any(), any()) -> integer().
compare(X, Y) ->
  if
    X <  Y -> -1;
    X == Y -> 0;
    X >  Y -> 1
  end.

-spec env_vars() -> #{binary() => binary()}.
env_vars() ->
  Pairs = [ begin
              EntryBin = unicode:characters_to_binary(Entry),
              [K, V]   = binary:split(EntryBin, <<"=">>),
              {K, V}
            end
            || Entry <- os:getenv()
          ],
  maps:from_list(Pairs).

-spec format_error(any(), clj_reader:location() | ?NIL) -> binary().
format_error(List, Location) when is_list(List) ->
  Reason = error_msg_to_binary(List),
  format_error(Reason, Location);
format_error(Reason, Location) when is_binary(Reason) ->
  LocationBin = location_to_binary(Location),
  <<LocationBin/binary, Reason/binary>>;
format_error(Reason, Location) ->
  iolist_to_binary(io_lib:format("~p", [{Location, Reason}])).

-spec error_str(module(), binary()) -> binary().
error_str(Module, Message) ->
  ModuleBin = atom_to_binary(Module, utf8),
  <<ModuleBin/binary, ": ", Message/binary>>.

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

-spec time(function()) -> any().
time(Fun) when is_function(Fun) ->
  time("Time", Fun).

-spec time(string(), function()) -> any().
time(Label, Fun) when is_function(Fun) ->
  time(Label, Fun, []).

-spec time(string(), function(), list()) -> any().
time(Label, Fun, Args) ->
  {T, V} = timer:tc(fun() -> apply(Fun, Args) end),
  io:format("~s: ~p ms~n", [Label, erlang:trunc(T / 1000)]),
  V.

-spec store_binary(module(), binary()) -> ok.
store_binary(Name, Binary) ->
  clj_cache:put({beam, Name}, Binary).

-spec add_core_to_binary(binary(), cerl:cerl()) -> binary().
add_core_to_binary(BeamBinary, CoreModule) ->
  CoreAbstract        = erlang:term_to_binary(CoreModule, [compressed]),
  CoreAbstractChunk   = {?CORE_CHUNK, CoreAbstract},
  {ok, _, OldChunks0} = beam_lib:all_chunks(BeamBinary),
  OldChunks1          = proplists:delete(?CORE_CHUNK, OldChunks0),
  {ok, NewBeamBinary} = beam_lib:build_module([CoreAbstractChunk | OldChunks1]),
  NewBeamBinary.

-spec add_compile_info_to_binary(binary(), any()) -> binary().
add_compile_info_to_binary(BeamBinary, CompileInfo) ->
  CompileInfoChunk    = { ?COMPILE_INFO_CHUNK
                        , erlang:term_to_binary(CompileInfo)
                        },
  {ok, _, OldChunks0} = beam_lib:all_chunks(BeamBinary),
  OldChunks1          = proplists:delete(?COMPILE_INFO_CHUNK, OldChunks0),
  {ok, NewBeamBinary} = beam_lib:build_module([CompileInfoChunk | OldChunks1]),
  NewBeamBinary.

-spec code_from_binary(atom()) -> cerl:cerl() | {error, term()}.
code_from_binary(Name) when is_atom(Name) ->
  %% First try to fetch the binary from memory
  case clj_cache:get({beam, Name}) of
    {ok, Binary} -> core_from_binary(Binary);
    _ ->
      %% Then try to fetch it from the beam file
      case code:get_object_code(Name) of
        {Name, Binary, _} -> core_from_binary(Binary);
        _ ->
          ?ERROR([ <<"Could not load object code for namespace: ">>
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
          {Code1, Opts1} = maybe_sys_pre_expand(Code0),
          {ok, CoreModule, _} = v3_core:module(Code1, Opts1),
          CoreModule;
        missing_chunk ->
          {error, missing_abstract_code}
        end;
    CoreModule ->
      erlang:binary_to_term(CoreModule)
  end.

-spec core_chunk() -> string().
core_chunk() -> ?CORE_CHUNK.

%% Since OTP 20 there is no sys_pre_expand module.
-spec maybe_sys_pre_expand(any()) -> {any(), [any()]}.
maybe_sys_pre_expand(Code) ->
  case code:ensure_loaded(sys_pre_expand) of
    {module, _} ->
      { Mod, Exp, Forms, Opts0} = sys_pre_expand:module(Code, []),
      {{Mod, Exp, Forms}, Opts0};
    {error, _} ->
      {Code, []}
  end.

%%------------------------------------------------------------------------------
%% Internal helper functions
%%------------------------------------------------------------------------------

%% @doc Valid integers can be either in decimal, octal, hexadecimal or any
%%      base specified (e.g. `2R010' is binary for `2').
-spec parse_int([string()]) -> integer() | ?NIL.
parse_int(Groups) ->
  case int_properties(Groups) of
    {zero, _Negate} ->
      0;
    {{Base, Value}, Negate} ->
      list_to_integer(Value, Base) * Negate;
    _ ->
      ?NIL
  end.

-spec int_properties([string()]) ->
  {zero | ?NIL | {integer(), string()}, -1 | 1}.
int_properties(Groups) ->
  Result = case Groups of
             [_, X | _] when X =/= "" -> zero;
             [_, _, X | _] when X =/= "" -> {10, X};
             [_, _, _, X | _] when X =/= "" -> {16, X};
             [_, _, _, _, X | _] when X =/= "" -> {8, X};
             [_, _, _, _, _, Y, X | _] when X =/= "" ->
               Base = list_to_integer(Y),
               {Base, X};
             _ ->
               ?NIL
           end,

  Negate = hd(Groups) == "-",
  {Result, case Negate of true -> -1; false -> 1 end}.

-spec parse_float([string()]) -> float().
parse_float(Groups) ->
  %% When there is no decimal part we add it so we can use
  %% list_to_float/1.
  FloatStr = case Groups of
               [_, G2, ""  | Rest] -> [G2, ".0", nth(1, Rest, "")];
               [_, G2, "." | Rest] -> [G2, ".0", nth(1, Rest, "")];
               [G1         | _   ] -> G1
             end,
  FloatBin = iolist_to_binary(FloatStr),
  binary_to_float(FloatBin).

-spec number_type(binary(), [{number_type(), re:mp()}]) ->
  {number_type(), [string()]} | ?NIL.
number_type(Number, Types) ->
  do_number_type(Number, Types, ?NIL).

-spec do_number_type( binary()
                    , [{number_type(), re:mp()}]
                    , number_type() | ?NIL
                    ) ->
  {number_type(), [string()]} | ?NIL.
do_number_type(_Number, [], Result) ->
  Result;
do_number_type(Number, [{Type, RE} | Rest], ?NIL) ->
  case re:run(Number, RE, [{capture, all, list}]) of
    nomatch -> do_number_type(Number, Rest, ?NIL);
    {match, [_ | Groups]} -> {Type, Groups}
  end;
do_number_type(_, _, Result) ->
  Result.

%% @doc Like lists:nth/2 but returns nil if `Index' is
%%      larger than the amount of elements in `List'.
nth(Index, List) ->
  case Index =< length(List) of
    true  -> lists:nth(Index, List);
    false -> ?ERROR(<<"Index out of bounds">>)
  end.

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

-spec quotient(number(), number()) -> number().
quotient(X, Y) when is_integer(X), is_integer(Y) ->
  X div Y;
quotient(X, Y) ->
  quotient(trunc(X), trunc(Y)).

-spec floor(number()) -> integer().
floor(X) when X < 0 ->
  T = trunc(X),
  case X - T == 0 of
    true  -> T;
    false -> T - 1
  end;
floor(X) ->
  trunc(X).

-spec ceil(number()) -> integer().
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

-spec bnand(number(), number()) -> number().
bnand(X, Y) -> bnot(X band Y).

-spec ets_get(atom() | ets:tid(), term()) -> term().
ets_get(Table, Id) ->
  ets_get(Table, Id, ?NIL).

-spec ets_get(atom() | ets:tid(), term(), term()) -> term().
ets_get(Table, Id, Default) ->
  case ets:lookup(Table, Id) of
    [] -> Default;
    [Value] -> Value
  end.

-spec ets_save(ets:tid() | atom(), term()) -> term().
ets_save(Table, Value) ->
  true = ets:insert(Table, Value),
  Value.

-spec resource_to_ns(binary()) -> binary().
resource_to_ns(Resource) ->
  Ns = binary:replace(Resource, <<"/">>, <<".">>, [global]),
  binary:replace(Ns, <<"_">>, <<"-">>, [global]).

-spec ns_to_resource(binary()) -> binary().
ns_to_resource(NsName) ->
  Resource = binary:replace(NsName, <<".">>, <<"/">>, [global]),
  binary:replace(Resource, <<"-">>, <<"_">>, [global]).

-spec record_hash(map()) -> integer().
record_hash(#{?TYPE := _, '__extmap' := ExternalMap} = Record) ->
  MapFields = maps:without([?TYPE, '__extmap', '__meta'], Record),
  AllFields = case ExternalMap of
                ?NIL -> MapFields;
                _ -> maps:merge(MapFields, ExternalMap)
              end,
  clj_rt:hash(AllFields).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The MIT License (MIT)

%% Copyright (c) 2014 Adam Lindberg

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec format_stacktrace([tuple()]) -> iolist().
format_stacktrace(Stacktrace) ->
  format_stacktrace(Stacktrace, [{indent, 4}]).

-spec format_stacktrace([tuple()], [{indent, integer()}]) -> iolist().
format_stacktrace(Stacktrace, Options) ->
  Indent = lists:duplicate(proplists:get_value(indent, Options), <<" ">>),
  stacktrace_pretty(Indent, Stacktrace).

-spec stacktrace_pretty(iolist(), [tuple()]) -> iolist().
stacktrace_pretty(_Indent, []) ->
  [];
stacktrace_pretty(Indent, [Entry | Stacktrace]) ->
  {Mod, Func, ArityOrArgs, Attrs} = Entry,
  Arity = case is_integer(ArityOrArgs) of
            true  -> ArityOrArgs;
            false -> length(ArityOrArgs)
          end,
  File = proplists:get_value(file, Attrs, "?"),
  Line = case proplists:get_value(line, Attrs, undefined) of
           undefined -> "?";
           LineInt   -> integer_to_binary(LineInt)
         end,
  Output = [ Indent
           , atom_to_list(Mod), <<"/">>, atom_to_list(Func)
           , <<".">>, integer_to_binary(Arity)
           , <<" (">>, File, <<":">>, Line, <<")">>
           , io_lib:nl()
           ],
  [Output | stacktrace_pretty(Indent, Stacktrace)].
