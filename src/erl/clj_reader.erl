%% @doc Clojerl reader.
%%
%% Reads an input string (or {@link 'erlang.io.IPushbackReader'}) and
%% returns a Clojerl form.
-module(clj_reader).

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-export([ read_fold/4
        , read/1
        , read/2
        , location_meta/1
        , remove_location/1
        ]).

-type location() :: #{ line   => non_neg_integer()
                     , column => non_neg_integer()
                     , file   => binary() | ?NIL
                     }.

-type opts() :: #{ %% When the value is `allow' then reader conditional will be
                   %% processed. If it is `preserve' then a ReaderConditional
                   %% value will be returned.
                   ?OPT_READ_COND => allow | preserve
                   %% Set of features available when processing a reader
                   %% conditional.
                 , ?OPT_FEATURES  => 'clojerl.Set':type()
                   %% When 'eofthrow' then throw, otherwise return value.
                 , ?OPT_EOF       => ?EOFTHROW | ok
                   %% Source file being read.
                 , file           => file:filename_all()
                   %% Keep track of the time spent in the reader
                 , time           => boolean()
                   %% IReader that should be used when there are no more
                   %% characters to be read from the binary.
                 , ?OPT_IO_READER => 'erlang.io.IPushbackReader':type()
                 }.

-export_type([location/0, opts/0]).

-type state() :: #{ %% A binary the represents Clojure source code
                    src           => binary()
                    %% The options map supplied to the reader.
                  , opts          => opts()
                    %% List of forms read (in reverse order).
                  , forms         => [any()]
                    %% Pending forms to be processed. Used by reader cond.
                  , pending_forms => [any()]
                    %% The current Clojure environment.
                  , env           => clj_env:env()
                    %% Current line and column location.
                  , loc           => {non_neg_integer(), non_neg_integer()}
                    %% Current bindings.
                  , bindings      => clj_scope:scope()
                    %% Used when reading a string
                  , return_on     => char() | ?NIL
                    %% Compiled regexes for numbers
                  , current       => binary() | ?NIL
                    %% Pre-compiled regular expressions for numbers
                  , number_types  => [{clj_utils:number_type(), re:mp()}]
                  }.

-type read_fold_fun() :: fun((any(), any()) -> any()).

-define(ARG_ENV, arg_env).
-define(GENSYM_ENV, gensym_env).
-define(SUPPRESS_READ, suppress_read).

%% @doc Extracts the location information from `X''s metadata.
-spec location_meta(any()) -> location() | ?NIL.
location_meta(X) ->
  case clj_rt:'meta?'(X) andalso clj_rt:meta(X) of
    false -> ?NIL;
    ?NIL  -> ?NIL;
    Meta  ->
      Meta1 = add_location_field(Meta, line, #{}),
      Meta2 = add_location_field(Meta, column, Meta1),
      Meta3 = add_location_field(Meta, file, Meta2),
      case Meta3 =:= #{} of
        true  -> ?NIL;
        false -> Meta3
      end
  end.

%% @doc Removes the location information from `Meta'.
-spec remove_location(any()) -> any().
remove_location(?NIL) ->
  ?NIL;
remove_location(Meta) ->
  Meta1 = clj_rt:dissoc(Meta, file),
  Meta2 = clj_rt:dissoc(Meta1, line),
  Meta3 = clj_rt:dissoc(Meta2, column),
  case clj_rt:'empty?'(Meta3) of
    true  -> ?NIL;
    false -> Meta3
  end.

%% @doc Read a form from the provided binary `Src'.
-spec read(binary()) -> any().
read(Src) ->
  read(Src, #{}).

%% @doc Reads the next form from the input. Returns the form
%%      or throws if there is no form to read.
-spec read(binary(), opts()) -> any().
read(Src, Opts) ->
  ok = clj_rt:reset_id(),
  State = new_state(Src, clj_env:default(), Opts),
  try
    ensure_read(State)
  catch
    throw:{eof, Value, _} -> Value
  end.

%% @private
%% @doc Folds over the forms read using `Env' as the accumulator.
%%
%% Used by {@link clj_compiler} to process all forms from the input.
-spec read_fold(read_fold_fun(), binary(), opts(), clj_env:env()) ->
  clj_env:env().
read_fold(Fun, Src, Opts0, Env) ->
  %% Since we want to consume all of the source, we don't want to
  %% throw when eof is reached.
  Opts    = Opts0#{?OPT_EOF => ok},
  State   = new_state(Src, Env, Opts),
  ReadFun = read_fun(Opts),
  ok = clj_rt:reset_id(),
  read_fold_loop(Fun, ReadFun, State).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec add_location_field(any(), any(), map()) -> map().
add_location_field(Meta, Name, Location) ->
  case clj_rt:get(Meta, Name) of
    ?NIL -> Location;
    Value -> maps:put(Name, Value, Location)
  end.

-spec read_fun(opts()) -> function().
read_fun(#{time := true}) ->
  fun time_read_one_fold/1;
read_fun(_) ->
  fun read_one_fold/1.

-spec time_read_one_fold(state()) -> state().
time_read_one_fold(State0) ->
  {Delta, State1} = timer:tc(fun () -> read_one_fold(State0) end),
  #{env := Env0} = State1,
  Env1 = clj_env:time("Reader", Delta, Env0),
  State1#{env => Env1}.

-spec read_one_fold(state()) -> state().
read_one_fold(State) ->
  try read_one(State)
  catch throw:{eof, ok, State_} -> State_
  end.

-spec read_fold_loop(read_fold_fun(), fun(), state()) -> clj_env:env().
read_fold_loop(Fun, ReadFun, State0) ->
  case ReadFun(State0) of
    %% Only finish when there is no more source to consume
    #{src := <<>>, forms := [], env := Env0} ->
      Env0;
    State1 = #{forms := []} ->
      read_fold_loop(Fun, ReadFun, State1);
    State1 = #{forms := [Form], env := Env0} ->
      Env1 = Fun(Form, Env0),
      read_fold_loop(Fun, ReadFun, State1#{env => Env1, forms => []})
  end.

-spec platform_features(opts() | ?NIL) -> opts().
platform_features(#{?OPT_FEATURES := Features} = Opts) ->
  NewFeatures = lists:foldl( fun(Feature, Acc) ->
                                 clj_rt:conj(Acc, Feature)
                             end
                           , Features
                           , ?PLATFORM_FEATURES
                           ),
  Opts#{?OPT_FEATURES => NewFeatures};
platform_features(Opts) ->
  Opts#{?OPT_FEATURES => clj_rt:hash_set(?PLATFORM_FEATURES)}.

%% @private
-spec new_state(binary(), clj_env:env(), opts()) -> state().
new_state(Src, Env, Opts) ->
  {ok, IntRegex}   = re:compile(?INT_PATTERN),
  {ok, FloatRegex} = re:compile(?FLOAT_PATTERN),
  #{ src           => Src
   , opts          => platform_features(Opts)
   , forms         => []
   , pending_forms => []
   , env           => Env
   , loc           => {1, 1}
   , bindings      => clj_scope:new()
   , return_on     => ?NIL
   , current       => ?NIL
   , number_types  => [{int, IntRegex}, {float, FloatRegex}]
   }.

%% @doc Makes sure a single form is read unless we reach
%%      the enf of file. It handles the case were an ignore
%%      reader is used.
%% @private
-spec ensure_read(state()) -> any().
ensure_read(#{forms := [Form]}) ->
  Form;
ensure_read(State) ->
  ensure_read(read_one(State)).

-spec read_one(state()) -> state().
read_one(#{pending_forms := [Form | PendingForms]} = State) ->
  push_form(Form, State#{pending_forms => PendingForms});
read_one(#{src := <<>>, opts := Opts} = State) ->
  Eof = maps:get(?OPT_EOF, Opts, ?EOFTHROW),
  %% If we got here it's because we were expecting something
  %% and it wasn't there.
  case check_reader(State) of
    {ok, NewState} ->
      read_one(NewState);
    eof when Eof =:= ?EOFTHROW ->
      ?ERROR(<<"EOF">>, location(State));
    eof ->
      throw({eof, Eof, State})
  end;
read_one(#{src := <<Ch/utf8, _/binary>>, return_on := Ch} = State) ->
  throw({return_on, consume_char(State)});
read_one(#{src := <<First/utf8, Rest/binary>>} = State) ->
  Second = peek_src(State#{src := Rest}),
  case clj_utils:char_type(First, Second) of
    whitespace      -> read_one(consume_char(State));
    number          -> read_number(State);
    string          -> read_string(State);
    keyword         -> read_keyword(State);
    comment         -> read_comment(State);
    quote           -> read_quote(State);
    deref           -> read_deref(State);
    meta            -> read_meta(State);
    syntax_quote    -> read_syntax_quote(State);
    unquote         -> read_unquote(State);
    list            -> read_list(State);
    vector          -> read_vector(State);
    map             -> read_map(State);
    unmatched_delim -> read_unmatched_delim(State);
    char            -> read_char(State);
    arg             -> read_arg(State);
    dispatch        -> read_dispatch(State);
    symbol          -> read_symbol(State)
  end.

%%------------------------------------------------------------------------------
%% Numbers
%%------------------------------------------------------------------------------

-spec read_number(state()) -> state().
read_number(State) ->
  {Current, State1} = consume(State, [number, symbol]),
  Types  = maps:get(number_types, State),
  Number = clj_utils:parse_number(Current, Types),
  push_form(Number, State1).

%%------------------------------------------------------------------------------
%% String
%%------------------------------------------------------------------------------

-spec read_string(state()) -> state().
%% Start consuming string
read_string(#{ src := <<"\"", _/binary>>
             , loc := Loc
             , current := ?NIL
             } = State
           ) ->
  State1 = add_scope(consume_char(State)),
  read_string(location_started(State1#{current => <<>>}, Loc));
%% Found closing double quotes
read_string(#{ src     := <<"\"", _/binary>>
             , current := String
             } = State0
           ) ->
  State = consume_char(State0#{current => ?NIL}),
  push_form(String, remove_scope(State));
%% Process escaped character
read_string(#{ src     := <<"\\", _/binary>>
             , current := String
             } = State0
           ) ->
  {EscapedChar, State00} = escape_char(consume_char(State0)),
  State = State00#{current => <<String/binary, EscapedChar/binary>>},

  read_string(State);
%% Append character to current string
read_string(#{ src     := <<Char/utf8, _/binary>>
             , current := String} = State0) ->
  State = State0#{current => <<String/binary, Char/utf8>>},
  read_string(consume_char(State));
%% Didn't find closing double quotes
read_string(#{src := <<>>} = State) ->
  case check_reader(State) of
    {ok, NewState}  ->
      read_string(NewState);
    eof ->
      {Line, Col} = scope_get(loc_started, State),
      ?ERROR( [ <<"Started reading at (">>
              , Line, <<":">>, Col
              , <<") but found EOF while reading string">>
              ]
            , location(State)
            )
  end.

-spec escape_char(state()) -> {binary(), state()}.
escape_char(State = #{src := <<>>}) ->
  case check_reader(State) of
    eof -> ?ERROR(<<"EOF while escaping char">>, location(State));
    {ok, NewState} -> escape_char(NewState)
  end;
escape_char(State = #{src := <<Char/utf8, _/binary>>}) ->
  CharType = clj_utils:char_type(Char, peek_src(State)),
  case Char of
    $t  -> {<<"\t">>, consume_char(State)};
    $r  -> {<<"\r">>, consume_char(State)};
    $n  -> {<<"\n">>, consume_char(State)};
    $\\ -> {<<"\\">>, consume_char(State)};
    $"  -> {<<"\"">>, consume_char(State)};
    $b  -> {<<"\b">>, consume_char(State)};
    $f  -> {<<"\f">>, consume_char(State)};
    $u  ->
      %% Hexa unicode
      State1   = consume_char(State),
      NextChar = peek_src(State1),
      ?ERROR_WHEN( NextChar =/= eof
                   andalso not is_char_valid(NextChar, 16)
                 , [ <<"Invalid unicode escape: \\u">>
                   , NextChar =/= eof andalso <<NextChar/utf8>>
                   ]
                 , location(State1)
                 ),
      {CodePoint, State2} = unicode_char(State1, 16, 4, true),
      {unicode:characters_to_binary([CodePoint], utf8), State2};
    _ when CharType == number ->
      %% Octal unicode
      {CodePoint, State1} = unicode_char(State, 8, 3, false),
      ?ERROR_WHEN( CodePoint > 8#377
                 , <<"Octal escape sequence must be in "
                     "range [0, 377]">>
                 , location(State1)
                 ),
      {unicode:characters_to_binary([CodePoint], utf8), State1};
    _ ->
      ?ERROR( <<"Unsupported escape character: \\", Char>>
            , location(State)
            )
  end.

-spec unicode_char(state(), integer(), integer(), Exact :: boolean()) ->
  {integer(), state()}.
unicode_char(State, Base, Length, IsExact) ->
  {Number, State1} = read_token(State, Length),
  Size = case IsExact of
           true -> size(Number);
           false -> Length
         end,

  ?ERROR_WHEN( Size =/= Length
             , [ <<"Invalid character length: ">>, Size
               , <<", should be: ">>, Length
               ]
             , location(State)
             ),
  Invalid      = fun(C) -> not is_char_valid(C, Base) end,
  Chars        = unicode:characters_to_list(Number),
  InvalidChars = lists:filter(Invalid, Chars),
  InvalidChar  = case InvalidChars of
                   [] -> ?NIL;
                   [C | _] -> <<C/utf8>>
                 end,
  ?ERROR_WHEN( InvalidChars =/= []
             , [ <<"Invalid digit: ">>, InvalidChar]
             , location(State)
             ),

  NumberInt = binary_to_integer(Number, Base),
  ?ERROR_WHEN(  16#D800 =< NumberInt andalso NumberInt =< 16#DFFF
             , [ <<"Invalid UTF-8 character number: \\u">>, Number]
             , location(State)
             ),

  {NumberInt, State1}.

-spec is_char_valid(integer(), integer()) -> boolean().
is_char_valid(C, Base) when C >= $0, C =< $9 -> C - $0 + 1  =< Base;
is_char_valid(C, Base) when C >= $a, C =< $z -> C - $a + 11 =< Base;
is_char_valid(C, Base) when C >= $A, C =< $Z -> C - $A + 11 =< Base;
is_char_valid(_, _) -> false.


%%------------------------------------------------------------------------------
%% Keyword
%%------------------------------------------------------------------------------

-spec read_keyword(state()) -> state().
read_keyword(#{src := <<":", _/binary>>} = State0) ->
  {Token, State} = read_token(consume_char(State0)),
  Keyword =
    case clj_utils:parse_symbol(Token) of
      {?NIL, <<":", Name/binary>>} ->
        Ns    = 'clojerl.Namespace':current(),
        NsSym = 'clojerl.Namespace':name(Ns),
        Namespace = 'clojerl.Symbol':name(NsSym),
        clj_rt:keyword(Namespace, Name);
      {?NIL, Name} ->
        clj_rt:keyword(Name);
      {<<":", Namespace/binary>>, Name} ->
        AliasSym  = clj_rt:symbol(Namespace),
        CurrentNs = 'clojerl.Namespace':current(),
        case 'clojerl.Namespace':alias(CurrentNs, AliasSym) of
          ?NIL -> ?ERROR(<<"Invalid token: :", Token/binary>>, location(State));
          Ns ->
            NsSym = 'clojerl.Namespace':name(Ns),
            clj_rt:keyword('clojerl.Symbol':name(NsSym), Name)
        end;
      {Namespace, Name} ->
        clj_rt:keyword(Namespace, Name);
      _ ->
        ?ERROR(<<"Invalid token: :", Token/binary>>, location(State))
    end,
  push_form(Keyword, State).

%%------------------------------------------------------------------------------
%% Symbol
%%------------------------------------------------------------------------------

-spec read_symbol(state()) -> state().
read_symbol(State) ->
  {Token, State1} = read_token(State),
  Symbol = case clj_utils:parse_symbol(Token) of
             {?NIL, <<"nil">>}   -> ?NIL;
             {?NIL, <<"true">>}  -> true;
             {?NIL, <<"false">>} -> false;
             {?NIL, Name} ->
               clj_rt:symbol(Name);
             {Ns, Name} ->
               clj_rt:symbol(Ns, Name);
             ?NIL ->
               ?ERROR(<<"Invalid token: ", Token/binary>>, location(State))
           end,

  push_form(Symbol, State1).

%%------------------------------------------------------------------------------
%% Comment
%%------------------------------------------------------------------------------

-spec read_comment(state()) -> state().
read_comment(State0) ->
  read_one(skip_line(State0)).

%%------------------------------------------------------------------------------
%% Quote
%%------------------------------------------------------------------------------

-spec read_quote(state()) -> state().
read_quote(#{src := <<"'"/utf8, _/binary>>} = State) ->
  Quote = clj_rt:symbol(<<"quote">>),
  wrapped_read(Quote, consume_char(State)).

%%------------------------------------------------------------------------------
%% Deref
%%------------------------------------------------------------------------------

-spec read_deref(state()) -> state().
read_deref(#{src := <<"@"/utf8, _/binary>>} = State) ->
  Deref = clj_rt:symbol(<<"clojure.core">>, <<"deref">>),
  wrapped_read(Deref, consume_char(State)).

%%------------------------------------------------------------------------------
%% Meta
%%------------------------------------------------------------------------------

-spec read_meta(state()) -> state().
read_meta(#{src := <<"^"/utf8, Src/binary>>} = State) ->
  {SugaredMeta, State1} = read_pop_one(State#{src => Src}),
  Meta = clj_utils:desugar_meta(SugaredMeta),

  {Form, State2} = read_pop_one(State1),
  FormMeta = clj_rt:merge([clj_rt:meta(Form), Meta]),
  NewMeta  = case clj_rt:'seq?'(Form) of
              true  -> clj_rt:merge([FormMeta, file_location_meta(State)]);
              false -> FormMeta
            end,

  NewForm = clj_rt:with_meta(Form, NewMeta),

  push_form(NewForm, State2).

%%------------------------------------------------------------------------------
%% Syntax quote
%%------------------------------------------------------------------------------

-spec read_syntax_quote(state()) -> state().
read_syntax_quote(#{src := <<"`"/utf8, _/binary>>} = State) ->
  {Form, NewState} = read_pop_one(consume_char(State)),

  try
    %% TODO: using process dictionary here might be a code smell
    erlang:put(?GENSYM_ENV, #{}),
    QuotedForm      = syntax_quote(Form),
    NewFormWithMeta = add_meta(Form, QuotedForm),

    push_form(NewFormWithMeta, NewState)
  catch ?WITH_STACKTRACE(error, Reason, Stack)
      ?ERROR(Reason, location(NewState), Stack)
  after
    erlang:erase(?GENSYM_ENV)
  end.

-spec syntax_quote(any()) -> any().
syntax_quote(Form) ->
  IsSpecial    = clj_analyzer:is_special(Form),
  IsSymbol     = clj_rt:'symbol?'(Form),
  IsUnquote    = is_unquote(Form),
  IsUnquoteSpl = is_unquote_splicing(Form),
  IsColl       = clj_rt:'coll?'(Form),
  IsLiteral    = is_literal(Form),
  IsErlMap     = is_map(Form) andalso not maps:is_key(?TYPE, Form),

  QuoteSymbol = clj_rt:symbol(<<"quote">>),
  if
    IsSpecial    -> clj_rt:list([QuoteSymbol, Form]);
    IsSymbol     ->
      Symbol = syntax_quote_symbol(Form),
      clj_rt:list([QuoteSymbol, Symbol]);
    IsUnquote    -> clj_rt:second(Form);
    IsUnquoteSpl -> throw(<<"unquote-splice not in list">>);
    %% Erlang collections
    is_list(Form) ->
      ErlListSymbol = clj_rt:symbol(<<"clojure.core">>, <<"erl-list">>),
      syntax_quote_coll(Form, ErlListSymbol);
    is_tuple(Form) ->
      TupleSymbol = clj_rt:symbol(<<"clojure.core">>, <<"tuple">>),
      syntax_quote_coll(tuple_to_list(Form), TupleSymbol);
    IsErlMap ->
      Flatten = fun(K, V, Acc) -> [K, V | Acc] end,
      ErlMapSymbol = clj_rt:symbol(<<"clojure.core">>, <<"erl-map">>),
      syntax_quote_coll(maps:fold(Flatten, [], Form), ErlMapSymbol);
    %% Clojure collections
    IsColl ->
      IsMap = clj_rt:'map?'(Form),
      IsVector = clj_rt:'vector?'(Form),
      IsSet = clj_rt:'set?'(Form),
      IsList = clj_rt:'list?'(Form),
      if
        IsMap ->
          HashMapSymbol = clj_rt:symbol(<<"clojure.core">>, <<"hash-map">>),
          syntax_quote_coll(flatten_map(Form), HashMapSymbol);
        IsVector ->
          VectorSymbol = clj_rt:symbol(<<"clojure.core">>, <<"vector">>),
          syntax_quote_coll(Form, VectorSymbol);
        IsSet ->
          HashSetSymbol = clj_rt:symbol(<<"clojure.core">>, <<"hash-set">>),
          syntax_quote_coll(Form, HashSetSymbol);
        IsList ->
          ListSymbol = clj_rt:symbol(<<"clojure.core">>, <<"list">>),
          syntax_quote_coll(Form, ListSymbol);
        true ->
          syntax_quote_coll(Form, ?NIL)
      end;
    IsLiteral -> Form;
    true      -> clj_rt:list([QuoteSymbol, Form])
  end.

-spec flatten_map(any()) -> any().
flatten_map(Map) ->
  MapSeq = clj_rt:seq(Map),
  flatten_map(MapSeq, clj_rt:vector([])).

-spec flatten_map(any() | ?NIL, 'clojerl.Vector':type()) -> any().
flatten_map(?NIL, Vector) ->
  clj_rt:seq(Vector);
flatten_map(MapSeq, Vector) ->
  First = clj_rt:first(MapSeq),
  Vector1 = clj_rt:conj(Vector, clj_rt:first(First)),
  Vector2 = clj_rt:conj(Vector1, clj_rt:second(First)),
  flatten_map(clj_rt:next(MapSeq), Vector2).

-spec syntax_quote_symbol(any()) -> any().
syntax_quote_symbol(Symbol) ->
  NamespaceStr = 'clojerl.Symbol':namespace(Symbol),
  NameStr      = 'clojerl.Symbol':name(Symbol),
  IsGenSym     = 'clojerl.String':ends_with(NameStr, <<"#">>),
  case {NamespaceStr, IsGenSym} of
    {?NIL, true} ->
      register_gensym(Symbol);
    _ ->
      resolve_symbol(Symbol)
  end.

-spec register_gensym(any()) -> any().
register_gensym(Symbol) ->
  GensymEnv = case erlang:get(?GENSYM_ENV) of
                undefined -> throw(<<"Gensym literal not in syntax-quote">>);
                X -> X
              end,
  case maps:get('clojerl.Symbol':name(Symbol), GensymEnv, ?NIL) of
    ?NIL ->
      NameStr    = 'clojerl.Symbol':name(Symbol),
      NameStr2   = binary:part(NameStr, 0, byte_size(NameStr) - 1),
      Parts      = [NameStr2, <<"__">>, clj_rt:next_id(), <<"__auto__">>],
      PartsStr   = lists:map(fun clj_rt:str/1, Parts),
      GenSym0    = clj_rt:symbol(erlang:iolist_to_binary(PartsStr)),

      Meta       = 'clojerl.Symbol':meta(Symbol),
      GenSym1    = 'clojerl.Symbol':with_meta(GenSym0, Meta),

      SymbolName = 'clojerl.Symbol':name(Symbol),
      erlang:put(?GENSYM_ENV, GensymEnv#{SymbolName => GenSym1}),
      GenSym1;
    GenSym ->
      GenSym
  end.

-spec resolve_symbol(any()) -> any().
resolve_symbol(Symbol) ->
  HasDot = binary:match('clojerl.Symbol':str(Symbol), <<"\.">>) =/= nomatch,
  case HasDot orelse 'clojerl.Namespace':find_var(Symbol) of
    true -> Symbol;
    ?NIL ->
      case 'clojerl.Symbol':namespace(Symbol) of
        ?NIL ->
          CurrentNs = 'clojerl.Namespace':current(),
          NameSym   = 'clojerl.Namespace':name(CurrentNs),
          Namespace = 'clojerl.Symbol':name(NameSym),
          Name      = 'clojerl.Symbol':name(Symbol),
          Meta      = 'clojerl.Symbol':meta(Symbol),
          clj_rt:with_meta(clj_rt:symbol(Namespace, Name), Meta);
        _ ->
          Symbol
      end;
    Var ->
      Namespace = 'clojerl.Var':namespace(Var),
      Name      = 'clojerl.Var':name(Var),
      Meta      = clj_rt:meta(Symbol),
      clj_rt:with_meta(clj_rt:symbol(Namespace, Name), Meta)
  end.

-spec syntax_quote_coll(any(), 'clojerl.Symbol':type()) -> any().
syntax_quote_coll(List, ?NIL) ->
  syntax_quote_coll(List);
syntax_quote_coll(List, FunSymbol) ->
  ExpandedList = syntax_quote_coll(List),
  ApplySymbol  = clj_rt:symbol(<<"clojure.core">>, <<"apply">>),
  clj_rt:list([ApplySymbol, FunSymbol, ExpandedList]).

-spec syntax_quote_coll(any()) -> any().
syntax_quote_coll(List) ->
  case clj_rt:'empty?'(List) of
    true ->
      ListSymbol = clj_rt:symbol(<<"clojure.core">>, <<"list">>),
      clj_rt:list([ListSymbol]);
    false ->
      ReversedExpandedItems = expand_list(List, []),
      ExpandedItems = lists:reverse(ReversedExpandedItems),
      ConcatSymbol = clj_rt:symbol(<<"clojure.core">>, <<"concat">>),
      clj_rt:list([ConcatSymbol | ExpandedItems])
  end.

-spec expand_list(any(), any()) -> any().
expand_list(?NIL, Result) ->
  Result;
expand_list(List, Result) ->
  Item       = clj_rt:first(List),
  ListSymbol = clj_rt:symbol(<<"clojure.core">>, <<"list">>),
  NewItem    = case {is_unquote(Item), is_unquote_splicing(Item)} of
                 {true, _} ->
                   clj_rt:list([ListSymbol, clj_rt:second(Item)]);
                 {_, true} ->
                   clj_rt:second(Item);
                 _ ->
                   QuotedForm = syntax_quote(Item),
                   clj_rt:list([ListSymbol, QuotedForm])
               end,
  expand_list(clj_rt:next(List), [NewItem | Result]).

-spec add_meta(any(), any()) -> any().
add_meta(Form, Result) ->
  case clj_rt:'meta?'(Form) andalso clj_rt:meta(Form) of
    false  -> Result;
    ?NIL   -> Result;
    Meta0  ->
      case remove_location(Meta0) of
        ?NIL ->
          Result;
        _ ->
          WithMetaSym = clj_rt:symbol(<<"clojure.core">>, <<"with-meta">>),
          clj_rt:list([WithMetaSym, Result, syntax_quote(Meta0)])
      end
  end.

is_unquote(Form) ->
  clj_rt:'seq?'(Form) andalso
    clj_rt:first(Form) == clj_rt:symbol(<<"clojure.core">>, <<"unquote">>).

is_unquote_splicing(Form) ->
  clj_rt:'seq?'(Form) andalso
    clj_rt:first(Form) == clj_rt:symbol( <<"clojure.core">>
                                       , <<"unquote-splicing">>
                                       ).

is_literal(Form) ->
  clj_rt:'keyword?'(Form)
    orelse clj_rt:'number?'(Form)
    orelse clj_rt:'string?'(Form)
    orelse clj_rt:'nil?'(Form)
    orelse clj_rt:'boolean?'(Form)
    orelse clj_rt:'regex?'(Form).

%%------------------------------------------------------------------------------
%% Unquote
%%------------------------------------------------------------------------------

-spec read_unquote(state()) -> state().
read_unquote(#{src := <<"\~"/utf8, _/binary>>} = State0) ->
  State = consume_char(State0),
  case peek_src(State) of
    $@ ->
      UnquoteSplicing = clj_rt:symbol(<<"clojure.core">>,
                                        <<"unquote-splicing">>),
      wrapped_read(UnquoteSplicing, consume_char(State));
    _ ->
      Unquote = clj_rt:symbol(<<"clojure.core">>, <<"unquote">>),
      wrapped_read(Unquote, State)
  end.

%%------------------------------------------------------------------------------
%% List
%%------------------------------------------------------------------------------

-spec read_list(state()) -> state().
read_list(#{ src   := <<"("/utf8, _/binary>>
           , forms := Forms
           , loc   := Loc
           } = State0
         ) ->
  State  = add_scope(consume_char(State0)),
  State1 = read_until($), location_started(State#{forms => []}, Loc)),
  State2 = remove_scope(State1),
  #{forms := ReversedItems} = State2,

  Items = lists:reverse(ReversedItems),
  List = 'clojerl.List':with_meta( clj_rt:list(Items)
                                 , file_location_meta(State0)
                                 ),

  State2#{forms => [List | Forms]}.

%%------------------------------------------------------------------------------
%% Vector
%%------------------------------------------------------------------------------

-spec read_vector(state()) -> state().
read_vector(#{ src   := <<"["/utf8, _/binary>>
             , forms := Forms
             , loc   := Loc
             } = State0
           ) ->
  State  = add_scope(consume_char(State0)),
  State1 = read_until($], location_started(State#{forms => []}, Loc)),
  State2 = remove_scope(State1),
  #{forms := ReversedItems} = State2,

  Items = lists:reverse(ReversedItems),
  Vector = 'clojerl.Vector':with_meta( clj_rt:vector(Items)
                                     , file_location_meta(State0)
                                     ),

  State2#{forms => [Vector | Forms]}.

%%------------------------------------------------------------------------------
%% Map
%%------------------------------------------------------------------------------

-spec read_map(state()) -> state().
read_map(#{src := <<"{"/utf8, _/binary>>} = State0) ->
  {Items, State1} = read_map_items(State0),

  ?ERROR_WHEN( length(Items) rem 2 =/= 0
             , <<"Map literal must contain an even number of forms">>
             , location(State0)
             ),

  Map = 'clojerl.Map':with_meta( 'clojerl.Map':?CONSTRUCTOR(Items, true)
                               , file_location_meta(State0)
                               ),
  push_form(Map, State1).

-spec read_map_items(state()) -> {[any()], state()}.
read_map_items(#{forms := Forms, loc := Loc} = State0) ->
  State  = add_scope(consume_char(State0)),
  State1 = read_until($}, location_started(State#{forms => []}, Loc)),
  State2 = remove_scope(State1),
  #{forms := ReversedItems} = State2,
  {lists:reverse(ReversedItems), State2#{forms => Forms}}.

%%------------------------------------------------------------------------------
%% Unmatched delimiter
%%------------------------------------------------------------------------------

-spec read_unmatched_delim(state()) -> no_return().
read_unmatched_delim(#{src := <<Delim/utf8, _/binary>>} = State) ->
  ?ERROR(<<"Umatched delimiter ", Delim/utf8>>, location(State)).

%%------------------------------------------------------------------------------
%% Character
%%------------------------------------------------------------------------------

-spec read_char(state()) -> state().
read_char(#{src := <<"\\"/utf8, _/binary>>} = State0) ->
  State    = consume_char(State0),
  NextChar = case peek_src(State) of
               eof -> ?ERROR( <<"EOF while reading character">>
                            , location(State)
                            );
               NextCh  -> NextCh
             end,
  {Token, State1} =
    case is_macro_terminating(NextChar) orelse is_whitespace(NextChar) of
      true -> {<<NextChar/utf8>>, consume_char(State)};
      false -> read_token(State)
    end,
  Char =
    case Token of
      <<_/utf8>> -> Token;
      <<"newline">> -> $\n;
      <<"space">> -> $ ;
      <<"tab">> -> $\t;
      <<"backspace">> -> $\b;
      <<"formfeed">> -> $\f;
      <<"return">> -> $\r;
      <<"u", RestToken/binary>> ->
        TokenLength = 'clojerl.String':count(RestToken),
        ?ERROR_WHEN( TokenLength =/= 4
                   , [ <<"Invalid unicode character: \\u">>
                     , RestToken
                     ]
                   , location(State1)
                   ),
        {Ch, _} = unicode_char(State1#{src => RestToken}, 16, 4, true),
        Ch;
      <<"o", RestToken/binary>> ->
        read_octal_char(State1#{src => RestToken});
      Ch -> ?ERROR( <<"Unsupported character: \\", Ch/binary>>
                  , location(State)
                  )
    end,

  CharBin = unicode:characters_to_binary([Char]),
  push_form(CharBin, State1).

-spec read_octal_char(state()) -> char().
read_octal_char(#{src := RestToken} = State) when size(RestToken) > 3 ->
  Size    = size(RestToken),
  SizeBin = integer_to_binary(Size),
  ?ERROR( <<"Invalid octal escape sequence length: ", SizeBin/binary>>
        , location(State)
        );
read_octal_char(#{src := RestToken} = State) ->
  Size = size(RestToken),
  case unicode_char(State, 8, Size, true) of
    {Ch, _} when Ch > 8#377 ->
      ?ERROR( <<"Octal escape sequence must be in range [0, 377]">>
            , location(State)
            );
    {Ch, _} -> Ch
  end.

%%------------------------------------------------------------------------------
%% Argument
%%------------------------------------------------------------------------------

-spec read_arg(state()) -> state().
read_arg(#{src := <<"%"/utf8, _/binary>>} = State) ->
  case erlang:get(?ARG_ENV) of
    undefined ->
      read_symbol(State);
    ArgEnv ->
      case arg_type(consume_char(State)) of
        {register_arg_1, State1} ->
          ArgSym = register_arg(1, ArgEnv),
          push_form(ArgSym, State1);
        {register_arg_multi, State1} ->
          ArgSym = register_arg(-1, ArgEnv),
          push_form(ArgSym, consume_chars(1, State1));
        {register_arg_n, State1} ->
          {N, State2} = read_pop_one(State1),
          ?ERROR_WHEN( not is_integer(N)
                     , <<"Arg literal must be %, %& or %integer">>
                     , location(State1)
                     ),
          ArgSym = register_arg(N, ArgEnv),
          push_form(ArgSym, State2)
      end
  end.

-spec arg_type(state()) ->
  {register_arg_1 | register_arg_multi | register_arg_n, state()}.
arg_type(State = #{src := <<>>}) ->
  case check_reader(State) of
    eof -> {register_arg_1, State};
    {ok, NewState} -> arg_type(NewState)
  end;
arg_type(State = #{src := <<Char/utf8, _/binary>>}) ->
  IsWhitespace = clj_utils:char_type(Char) =:= whitespace,
  IsMacroTerminating = is_macro_terminating(Char),
  Type = if
           IsWhitespace orelse IsMacroTerminating -> register_arg_1;
           Char =:= $& -> register_arg_multi;
           true -> register_arg_n
         end,
  {Type, State}.

-spec register_arg(integer(), map()) -> 'clojerl.Symbol':type().
register_arg(N, ArgEnv) ->
  case maps:get(N, ArgEnv, ?NIL) of
    ?NIL ->
      ArgSymbol = gen_arg_sym(N),
      NewArgEnv = maps:put(N, ArgSymbol, ArgEnv),
      erlang:put(?ARG_ENV, NewArgEnv),
      ArgSymbol;
    ArgSymbol -> ArgSymbol
  end.

-spec gen_arg_sym(integer()) -> 'clojerl.Symbol':type().
gen_arg_sym(N) ->
  Param = case N of
            -1 -> <<"rest">>;
            N ->
              NBin = integer_to_binary(N),
              <<"p", NBin/binary>>
          end,
  NextId = clj_rt:next_id(),
  Parts = lists:map(fun clj_rt:str/1, [Param, <<"__">>, NextId, <<"#">>]),
  Name = erlang:iolist_to_binary(Parts),
  clj_rt:symbol(Name).

%%------------------------------------------------------------------------------
%% Reader dispatch
%%------------------------------------------------------------------------------

-spec read_dispatch(state()) -> state().
read_dispatch(#{src := <<"#">>} = State) ->
  case check_reader(State#{src := <<>>}) of
    {ok, NewState = #{src := NewSrc}}  ->
      read_dispatch(NewState#{src := <<"#", NewSrc/binary>>});
    eof ->
      ?ERROR(<<"EOF while reading dispatch">>, location(State))
  end;
read_dispatch(#{src := <<"#"/utf8, Src/binary>>} = State) ->
  <<Ch/utf8, _/binary>> = Src,
  NewState = consume_chars(2, State),
  case Ch of
    $^ -> read_meta(consume_char(State)); %% deprecated
    $' -> read_var(consume_char(State));
    $( -> read_fn(consume_char(State));
    $= -> read_eval(NewState);
    ${ -> read_set(NewState);
    $" -> read_regex(NewState);
    $! -> read_comment(NewState);
    $_ -> read_discard(NewState);
    $? -> read_cond(NewState);
    $: -> read_namespaced_map(NewState);
    $< -> ?ERROR(<<"Unreadable form">>, location(State));
    _  -> read_tagged(consume_char(State))
  end.

%%------------------------------------------------------------------------------
%% #' var
%%------------------------------------------------------------------------------

-spec read_var(state()) -> state().
read_var(#{src := <<"'", _/binary>>} = State) ->
  VarSymbol = clj_rt:symbol(<<"var">>),
  wrapped_read(VarSymbol, consume_char(State)).

%%------------------------------------------------------------------------------
%% #() fn
%%------------------------------------------------------------------------------

-spec read_fn(state()) -> state().
read_fn(State) ->
  ?ERROR_WHEN( erlang:get(?ARG_ENV) =/= undefined
             , <<"Nested #()s are not allowed">>
             , location(State)
             ),

  {{Form, NewState}, ArgEnv} =
    try
      erlang:put(?ARG_ENV, #{}),
      {read_pop_one(State), erlang:get(?ARG_ENV)}
    after
      %% Make sure the process dictionary entry gets removed
      erlang:erase(?ARG_ENV)
    end,

  MaxArg = lists:max([0 | maps:keys(ArgEnv)]),
  MapFun = fun(N) ->
               maps:get(N, ArgEnv, gen_arg_sym(N))
           end,
  ArgsSyms = lists:map(MapFun, lists:seq(1, MaxArg)),
  ArgsSyms2 = case maps:get(-1, ArgEnv, ?NIL) of
                ?NIL -> ArgsSyms;
                RestArgSym  ->
                  AmpSym = clj_rt:symbol(<<"&">>),
                  ArgsSyms ++ [AmpSym, RestArgSym]
              end,
  ArgsVector = clj_rt:vector(ArgsSyms2),

  FnSymbol = clj_rt:symbol(<<"fn*">>),
  FnForm   = clj_rt:list([FnSymbol, ArgsVector, Form]),
  FnFormWithMeta = 'clojerl.List':with_meta(FnForm, file_location_meta(State)),

  push_form(FnFormWithMeta, NewState).

%%------------------------------------------------------------------------------
%% #= eval
%%------------------------------------------------------------------------------

-spec read_eval(state()) -> state().
read_eval(#{env := Env} = State) ->
  {Form, NewState} = read_pop_one(State),
  {Value, Env1} = clj_compiler:eval(Form, #{}, Env),
  push_form(Value, NewState#{env => Env1}).

%%------------------------------------------------------------------------------
%% #{} set
%%------------------------------------------------------------------------------

-spec read_set(state()) -> state().
read_set(#{forms := Forms, loc := Loc} = State0) ->
  State  = add_scope(State0),
  State1 = read_until($}, location_started(State#{forms => []}, Loc)),
  State2 = remove_scope(State1),
  #{forms := ReversedItems} = State2,

  Items       = lists:reverse(ReversedItems),
  Set         = 'clojerl.Set':?CONSTRUCTOR(Items, true),
  SetWithMeta = 'clojerl.Set':with_meta(Set, file_location_meta(State0)),

  State2#{forms => [SetWithMeta | Forms]}.

%%------------------------------------------------------------------------------
%% #"" regex
%%------------------------------------------------------------------------------

-spec read_regex(state()) -> state().
read_regex(#{src := <<"\""/utf8, _/binary>>, current := ?NIL} = State) ->
  read_regex(State#{current => <<>>});
read_regex(#{src := <<Ch/utf8, _/binary>>, current := ?NIL} = State) ->
  NewState = State#{current => <<Ch/utf8>>},
  read_regex(consume_char(NewState));
read_regex(#{src := <<"\\"/utf8>>, current := Current} = State) ->
  case check_reader(consume_char(State)) of
    eof -> ?ERROR(<<"EOF while escaping char in regex">>, location(State));
    {ok, NewState = #{src := <<Ch/utf8>>}} ->
      NewCurrent = <<Current/binary, "\\"/utf8, Ch/utf8>>,
      read_regex(consume_char(NewState#{current => NewCurrent}))
  end;
read_regex(#{ src     := <<"\\"/utf8, Ch/utf8, _/binary>>
            , current := Current
            } = State
          ) ->
  NewState = State#{current => <<Current/binary, "\\", Ch/utf8>>},
  read_regex(consume_chars(2, NewState));
read_regex(#{src := <<"\""/utf8, _/binary>>, current := Current} = State) ->
  Regex = 'erlang.util.Regex':?CONSTRUCTOR(Current),
  push_form(Regex, consume_char(State#{current => ?NIL}));
read_regex(#{src := <<Ch/utf8, _/binary>>, current := Current} = State) ->
  NewState = State#{current => <<Current/binary, Ch/utf8>>},
  read_regex(consume_char(NewState));
read_regex(#{src := <<>>} = State) ->
  case check_reader(State) of
    {ok, NewState}  ->
      read_regex(NewState);
    eof ->
      ?ERROR(<<"EOF while reading regex">>, location(State))
  end.

%%------------------------------------------------------------------------------
%% #_ discard
%%------------------------------------------------------------------------------

-spec read_discard(state()) -> state().
read_discard(#{forms := Forms, return_on := ReturnOn} = State0) ->
  %% Remove forms and return_on to avoid discarding something unintentionally.
  State1 = State0#{forms := [], return_on := ?NIL},
  {_, State2} = read_pop_one(State1),
  %% Can't call read_one here because is might not be a top level form.
  State2#{forms := Forms, return_on := ReturnOn}.

%%------------------------------------------------------------------------------
%% #? cond
%%------------------------------------------------------------------------------

-spec read_cond(state()) -> state().
read_cond(#{src := <<>>} = State) ->
  case check_reader(State) of
    {ok, NewState} ->
      read_cond(NewState);
    eof ->
      ?ERROR(<<"EOF while reading cond">>, location(State))
  end;
read_cond(#{opts := Opts} = State0) ->
  ReadCondOpt = maps:get(?OPT_READ_COND, Opts, ?NIL),
  ?ERROR_WHEN(not lists:member(ReadCondOpt, [allow, preserve])
             , <<"Conditional read not allowed">>
             , location(State0)
             ),

  {_, State} = consume(State0, [whitespace]),
  IsSplicing = peek_src(State) =:= $@,

  State1 = case IsSplicing of
             true  -> consume_char(State);
             false -> State
           end,

  {_, State2} = consume(State1, [whitespace]),

  ?ERROR_WHEN( peek_src(State2) =/= $(
             , <<"read-cond body must be a list">>
             , location(State1)
             ),

  OldSupressRead = case erlang:get(?SUPPRESS_READ) of
                     undefined -> false;
                     SR -> SR
                   end,
  SupressRead    = OldSupressRead orelse ReadCondOpt == preserve,
  erlang:put(?SUPPRESS_READ, SupressRead),

  try
    case SupressRead of
      true ->
        {ListForm, State3} = read_pop_one(State2),
        ReaderCondForm     = reader_conditional(ListForm, IsSplicing),
        push_form(ReaderCondForm, State3);
      false ->
        read_cond_delimited(IsSplicing, consume_char(State2))
    end
  after
    erlang:put(?SUPPRESS_READ, OldSupressRead)
  end.

reader_conditional(List, IsSplicing) ->
  'clojerl.reader.ReaderConditional':?CONSTRUCTOR(List, IsSplicing).

read_cond_delimited(IsSplicing, State) ->
  case match_feature(State) of
    %% When there is no match at all there is nothing to read
    {finished, State1} ->
      State1;
    {nomatch,  State1} ->
      read_cond_delimited(IsSplicing, State1);
    {match, Form, State1} when IsSplicing ->
      ?ERROR_WHEN( not clj_rt:'sequential?'(Form)
                 , <<"Spliced form list in "
                     "read-cond-splicing must "
                     "extend clojerl.ISequential">>
                 , location(State1)
                 ),

      ReadDelim  = clj_rt:boolean(scope_get(read_delim, State)),
      ?ERROR_WHEN( IsSplicing andalso not ReadDelim
                 , <<"Reader conditional splicing "
                     "not allowed at the top level">>
                 , location(State)
                 ),

      State2 = read_until($), fun read_skip_suppress/1, State1),
      Items = lists:reverse(clj_rt:to_list(Form)),
      lists:foldl(fun push_pending_form/2, State2, Items);
    {match, Form, State1} ->
      State2 = read_until($), fun read_skip_suppress/1, State1),
      push_form(Form, State2)
  end.

-spec match_feature(state()) ->
  {match, any(), state()} | {nomatch | finished, state()}.
match_feature(State = #{return_on := ReturnOn, opts := Opts}) ->
  Features = maps:get(?OPT_FEATURES, Opts, clj_rt:hash_set([])),
  try
    %% Change the return_on value so that we only read until the next
    %% ')' char.
    {Feature, State1} = read_pop_one(State#{return_on => $)}),
    ?ERROR_WHEN( not clj_rt:'keyword?'(Feature)
               , <<"Feature should be a keyword">>
               , location(State)
               ),

    ?ERROR_WHEN( lists:member(Feature, ?RESERVED_FEATURES)
               , [<<"Feature name ">>, Feature, <<" is reserved.">>]
               , location(State)
               ),

    try
      case
        Feature =:= ?DEFAULT_FEATURE orelse
        clj_rt:'contains?'(Features, Feature)
      of
        true  ->
          {Form, State2} = read_pop_one(State1),
          {match, Form, State2#{return_on => ReturnOn}};
        false ->
          State2 = read_skip_suppress(State1),
          {nomatch, State2#{return_on => ReturnOn}}
      end
    catch
      throw:{return_on, State3} ->
        ?ERROR( <<"read-cond requires an even number of forms">>
              , location(State3)
              )
    end
  catch
    throw:{return_on, State4} ->
      {finished, State4#{return_on => ReturnOn}}
  end.

-spec read_skip_suppress(state()) -> state().
read_skip_suppress(State) ->
  OldSupressRead = case erlang:get(?SUPPRESS_READ) of
                     undefined -> false;
                     SR -> SR
                   end,
  erlang:put(?SUPPRESS_READ, true),
  try
    {_, NewState} = read_pop_one(State),
    NewState
  after
    erlang:put(?SUPPRESS_READ, OldSupressRead)
  end.

%%------------------------------------------------------------------------------
%% #: namespaced map
%%------------------------------------------------------------------------------

-spec read_namespaced_map(state()) -> state().
read_namespaced_map(State0) ->
  {IsAuto, Symbol, State1} = is_auto_namespaced_map(peek_src(State0), State0),

  %% Make sure to consume all whitespace
  {_, State2} = consume(State1, [whitespace]),

  ?ERROR_WHEN( peek_src(State2) =/= ${
             , <<"Namespaced map must specify a map">>
             , location(State2)
             ),

  {Items0, State3} = read_map_items(State2),

  ?ERROR_WHEN( length(Items0) rem 2 =/= 0
             , <<"Namespaced map literal must contain an even number of forms">>
             , location(State2)
             ),

  Ns     = resolve_namespaced_map(IsAuto, Symbol, State3),
  Items1 = process_namespaced_key_values(Ns, Items0, []),
  Map    = 'clojerl.Map':with_meta( 'clojerl.Map':?CONSTRUCTOR(Items1, true)
                                  , file_location_meta(State2)
                                  ),

  push_form(Map, State3).

-spec is_auto_namespaced_map(char(), state()) ->
  {boolean(), ?NIL | 'clojerl.Symbol':type(), state()}.
is_auto_namespaced_map($:, State0) ->
  %% #:: { }, #::foo { } or an error
  State1 = consume_char(State0),
  NextChar  = peek_src(State1),
  {_, State2} = consume(State1, [whitespace]),
  ?ERROR_WHEN( is_whitespace(NextChar)
               andalso peek_src(State2) =/= ${
             , <<"Namespaced map must specify a namespace">>
             , location(State2)
             ),

  case peek_src(State2) of
    ${ -> %% #:: { }
      {true, ?NIL, State2};
    _ -> %% #::foo { }
      {Symbol, State3} = read_pop_one(State2),
      {true, Symbol, State3}
  end;
is_auto_namespaced_map(Char, State0) ->
  %% #:foo { } or an error
  ?ERROR_WHEN( is_whitespace(Char)
             , <<"Namespaced map must specify a namespace">>
             , location(State0)
             ),
  {Symbol, State1} = read_pop_one(State0),
  {false, Symbol, State1}.

-spec resolve_namespaced_map(boolean(), 'clojerl.Symbol':type(), state()) ->
  'clojerl.Symbol':type().
resolve_namespaced_map(true, ?NIL, _State) ->
  Ns   = 'clojerl.Namespace':current(),
  Name = 'clojerl.Namespace':name(Ns),
  clj_rt:name(Name);
resolve_namespaced_map(true, Symbol, State) ->
  validate_namespaced_name(Symbol, State),
  Ns = 'clojerl.Namespace':current(),
  case 'clojerl.Namespace':alias(Ns, Symbol) of
    ?NIL ->
      ?ERROR(<<"Unknown auto-resolved namespace alias">>, location(State));
    ResolvedNs ->
      Name = 'clojerl.Namespace':name(ResolvedNs),
      clj_rt:name(Name)
  end;
resolve_namespaced_map(false, Symbol, State) ->
  validate_namespaced_name(Symbol, State),
  clj_rt:name(Symbol).

-spec validate_namespaced_name('clojerl.Symbol':type(), state()) -> ok.
validate_namespaced_name(Symbol, State) ->
  ?ERROR_WHEN( not clj_rt:'symbol?'(Symbol)
               orelse clj_rt:namespace(Symbol) =/= ?NIL
             , [ <<"Namespaced map must specify a valid namespace: ">>
               , Symbol
               ]
             , location(State)
             ).

-spec namespaced_key(binary(), 'clojerl.Keyword':type()) ->
  'clojerl.Keyword':type().
namespaced_key(Ns, Key) ->
  KeyNs = clj_rt:namespace(Key),
  case clj_rt:'keyword?'(Key) of
    true ->
      case KeyNs of
        ?NIL    -> clj_rt:keyword(Ns, clj_rt:name(Key));
        <<"_">> -> clj_rt:keyword(clj_rt:name(Key));
        _       -> Key
      end;
    false ->
      case KeyNs of
        ?NIL    -> clj_rt:symbol(Ns, clj_rt:name(Key));
        <<"_">> -> clj_rt:symbol(clj_rt:name(Key));
        _       -> Key
      end
  end.

-spec process_namespaced_key_values(binary(), [any()], [any()]) -> [any()].
process_namespaced_key_values(_Ns, [], Acc) ->
  Acc;
process_namespaced_key_values(Ns, [K0, V | KVs], Acc) ->
  K1 = case clj_rt:'keyword?'(K0) orelse clj_rt:'symbol?'(K0) of
         true  -> namespaced_key(Ns, K0);
         false -> K0
       end,
  process_namespaced_key_values(Ns, KVs, [K1, V | Acc]).

%%------------------------------------------------------------------------------
%% # reader tag
%%------------------------------------------------------------------------------

-spec read_tagged(state()) -> state().
read_tagged(State) ->
  {Symbol, State1} = read_pop_one(State),

  ?ERROR_WHEN( not clj_rt:'symbol?'(Symbol)
             , <<"Reader tag must be a symbol">>
             , location(State)
             ),

  read_tagged_symbol(Symbol, location(State), State1).

-spec read_tagged_symbol('clojerl.Symbol':type(), any(), state()) -> state().
read_tagged_symbol(Tag, Location, State0) ->
  {Form, State1} = read_pop_one(State0),
  SupressRead = erlang:get(?SUPPRESS_READ),
  case 'clojerl.Symbol':str(Tag) of
    <<"erl">> -> erlang_literal(Form, State1);
    <<"bin">> -> erlang_binary(Form, State1);
    <<"as">>  -> erlang_alias(Form, State1);
    _ when SupressRead =:= true ->
      push_form(tagged_literal(Tag, Form), State1);
    _ ->
      case 'clojerl.String':contains('clojerl.Symbol':name(Tag), <<".">>) of
        true  -> read_record(Tag, Form, State1);
        false -> read_tagged(Tag, Form, Location, State1)
      end
  end.

-spec erlang_literal(any(), state()) -> state().
erlang_literal(Form, State) ->
  IsList   = clj_rt:'list?'(Form),
  IsMap    = clj_rt:'map?'(Form),
  IsVector = clj_rt:'vector?'(Form),
  IsString = clj_rt:'string?'(Form),
  IsSymbol = clj_rt:'symbol?'(Form),

  Value    =
    if
      IsList   ->
        ErlListSym = clj_rt:symbol(<<"erl-list*">>),
        List       = clj_rt:to_list(Form),
        clj_rt:list([ErlListSym | List]);
      IsMap    -> clj_rt:'clj->erl'(Form, false);
      IsVector -> list_to_tuple('clojerl.Vector':to_list(Form));
      IsString -> clj_rt:list( [ clj_rt:symbol(<<"erl-list*">>)
                               | unicode:characters_to_list(Form)
                               ]
                               );
      IsSymbol ->
        {NsStr, NameStr, Arity} = clj_utils:parse_erl_fun(Form),
        ErlFunSym = clj_rt:symbol(<<"erl-fun*">>),
        Args = case {NsStr, Arity} of
                 {?NIL, ?NIL} ->
                   [binary_to_atom(NameStr, utf8)];
                 {?NIL, _} ->
                   [binary_to_atom(NameStr, utf8), Arity];
                 {_, ?NIL} ->
                   [ binary_to_atom(NsStr, utf8)
                   , binary_to_atom(NameStr, utf8)
                   ];
                 {_, _} ->
                   [ binary_to_atom(NsStr, utf8)
                   , binary_to_atom(NameStr, utf8)
                   , Arity
                   ]
               end,
        clj_rt:list([ErlFunSym | Args]);
      true     -> ?ERROR( [ <<"Can only have list, map, tuple ">>
                          , <<"or Erlang string literals, had: ">>
                          , clj_rt:type(Form)
                          ]
                        , location(State)
                        )
    end,

  push_form(Value, State).

-spec erlang_binary(any(), state()) -> state().
erlang_binary(Form, State) ->
  ?ERROR_WHEN( not clj_rt:'vector?'(Form)
             , <<"Binary expressions should be enclosed by a vector">>
             , location(State)
             ),

  ErlBinarySym = clj_rt:symbol(<<"erl-binary*">>),
  List         = clj_rt:list([ErlBinarySym | clj_rt:to_list(Form)]),
  push_form(List, State).

-spec erlang_alias(any(), state()) -> state().
erlang_alias(Form, State) ->
  ErlAliasSym = clj_rt:symbol(<<"erl-alias*">>),
  List        = clj_rt:list([ErlAliasSym | clj_rt:to_list(Form)]),
  push_form(List, State).

-spec read_record('clojerl.Symbol':type(), any(), state()) -> state().
read_record(Symbol, Form, State) ->
  ReadEvalVar = 'clojerl.Var':?CONSTRUCTOR( <<"clojure.core">>
                                          , <<"*read-eval*">>
                                          ),

  ?ERROR_WHEN( not clj_rt:boolean('clojerl.Var':deref(ReadEvalVar))
             , <<"Record construction syntax can only be used "
                 "when *read-eval* == true">>
             , location(State)
             ),

  ?ERROR_WHEN( not clj_rt:'vector?'(Form)
               andalso not clj_rt:'map?'(Form)
             , [ <<"Unreadable constructor form starting with \"#">>
               , Symbol
               , <<"\"">>
               ]
             , location(State)
             ),

  Type = try erlang:binary_to_existing_atom('clojerl.Symbol':str(Symbol), utf8)
         catch throw:badarg ->
             ?ERROR( [Symbol, <<" is not loaded or doesn't exist">>]
                   , location(State)
                   )
         end,

  Record =
    case clj_rt:'vector?'(Form) of
      true  ->
        ArgCount = clj_rt:count(Form),
        Exported = erlang:function_exported(Type, ?CONSTRUCTOR, ArgCount),

        ?ERROR_WHEN( not Exported
                   , [ <<"Unexpected number of constructor ">>
                     , <<"arguments to ">>, Symbol
                     , <<": got ">>, ArgCount
                     ]
                   , location(State)
                   ),

        erlang:apply(Type, ?CONSTRUCTOR, clj_rt:to_list(Form));
      false ->
        Keys        = clj_rt:to_list(clj_rt:keys(Form)),
        NotKwFun    = fun(X) -> not clj_rt:'keyword?'(X) end,
        NonKeywords = lists:filter(NotKwFun, Keys),

        ?ERROR_WHEN( NonKeywords =/= []
                   , [ <<"Unreadable defrecord form: key must ">>
                              , <<"be of type clojerl.Keyword, got ">>
                     , clj_rt:first(NonKeywords)
                     ]
                   , location(State)
                   ),

        erlang:apply(Type, create, [Form])
      end,

  push_form(Record, State).

-spec read_tagged('clojerl.Symbol':type(), any(), any(), state()) ->
  state().
read_tagged(Symbol, Form, Location, State) ->
  DataReadersVar        = 'clojerl.Var':?CONSTRUCTOR( <<"clojure.core">>
                                                    , <<"*data-readers*">>
                                                    ),
  DefaultDataReadersVar = 'clojerl.Var':?CONSTRUCTOR( <<"clojure.core">>
                                                    , <<"default-data-readers">>
                                                    ),
  DefaultReaderFunVar   =
    'clojerl.Var':?CONSTRUCTOR( <<"clojure.core">>
                              , <<"*default-data-reader-fn*">>
                              ),

  DataReaders = 'clojerl.Var':deref(DataReadersVar),
  Reader0     = clj_rt:get(DataReaders, Symbol, ?NIL),

  DefaultDataReaders = 'clojerl.Var':deref(DefaultDataReadersVar),
  Reader1 = case
              Reader0 =/= ?NIL
              orelse clj_rt:get(DefaultDataReaders, Symbol, ?NIL)
            of
              true -> Reader0;
              DefaultDataReader -> DefaultDataReader
            end,

  {IsDefault, Reader2} =
    case Reader1 =/= ?NIL of
      true  -> {false, Reader1};
      false -> {true, 'clojerl.Var':deref(DefaultReaderFunVar)}
    end,

  ?ERROR_WHEN( Reader2 =:= ?NIL
             , [<<"No reader function for tag ">>, Symbol]
             , Location
             ),

  ReadForm = case IsDefault of
               true  -> clj_rt:apply(Reader2, [Symbol, Form]);
               false -> clj_rt:apply(Reader2, [Form])
             end,
  push_form(ReadForm, State).

-spec tagged_literal(any(), any()) -> 'clojerl.reader.Taggedliteral':type().
tagged_literal(Tag, Form) ->
  'clojerl.reader.TaggedLiteral':?CONSTRUCTOR(Tag, Form).

%%------------------------------------------------------------------------------
%% Utility functions
%%------------------------------------------------------------------------------

-spec location(state()) -> location().
location(#{opts := Opts} = State) ->
  {Line, Col} = case maps:get(loc, State, ?NIL) of
                  ?NIL -> {?NIL, ?NIL};
                  Loc  -> Loc
                end,
  #{ line   => Line
   , column => Col
   , file   => maps:get(file, Opts, ?NIL)
   }.

-spec location_started(state(), {non_neg_integer(), non_neg_integer()}) ->
  state().
location_started(State, Loc) ->
  scope_put(loc_started, Loc, State).

-spec consume_char(state()) -> state().
consume_char(#{src := <<"\n"/utf8, Src/binary>>, loc := {Line, _}} = State) ->
  State#{src => Src, loc => {Line + 1, 1}};
consume_char(#{src := <<_/utf8, Src/binary>>, loc := {Line, Col}} = State) ->
  State#{src => Src, loc => {Line, Col + 1}};
consume_char(State) ->
  case check_reader(State) of
    {ok, NewState} -> consume_char(NewState);
    eof -> State
  end.

-spec consume_chars(non_neg_integer(), state()) -> state().
consume_chars(0, State) ->
  State;
consume_chars(N, State) when N > 0 ->
  consume_chars(N - 1, consume_char(State)).

-spec consume(state(), [clj_utils:char_type()] | fun()) ->
  {binary(), state()}.
consume(State, TypesOrPred) ->
  consume(State, TypesOrPred, -1).

-spec consume(state(), [clj_utils:char_type()] | fun(), integer()) ->
  {binary(), state()}.
consume(State, TypesOrPred, Length) ->
  do_consume(State, <<>>, TypesOrPred, Length).

-spec do_consume( state()
                , binary()
                , [clj_utils:char_type()] | fun()
                , integer()
                ) ->
  {binary(), state()}.
do_consume(State, Acc, _TypesOrPred, 0) ->
  {Acc, State};
do_consume(State = #{src := <<>>}, Acc, TypesOrPred, Length) ->
  case check_reader(State) of
    {ok, NewState} -> do_consume(NewState, Acc, TypesOrPred, Length);
    eof -> {Acc, State}
  end;
do_consume( State = #{src := <<X/utf8, _/binary>>}
          , Acc
          , Pred
          , Length
          ) when is_function(Pred) ->
  case Pred(X) of
    true  ->
      State1 = consume_char(State),
      do_consume(State1, <<Acc/binary, X/utf8>>, Pred, Length - 1);
    false ->
      {Acc, unread_char(State)}
  end;
do_consume( State = #{src := <<X/utf8, Rest/binary>>}
          , Acc
          , Types
          , Length
          ) ->
  Next = case Rest of
           <<N/utf8, _>> -> N;
           _             -> -1
         end,
  Type = clj_utils:char_type(X, Next),
  case lists:member(Type, Types) of
    true ->
      State1 = consume_char(State),
      do_consume(State1, <<Acc/binary, X/utf8>>, Types, Length - 1);
    false ->
      {Acc, unread_char(State)}
  end.

-spec read_token(state()) -> {binary(), state()}.
read_token(State) ->
  read_token(State, -1).

-spec read_token(state(), integer()) -> {binary(), state()}.
read_token(State, Length) ->
  Fun = fun(Char) ->
            (not is_whitespace(Char))
              andalso (not is_macro_terminating(Char))
        end,
  consume(State, Fun, Length).


-spec read_until(char(), state()) -> state().
read_until(Delim, State) ->
  read_until(Delim, fun read_one/1, State).

-spec read_until(char(), function(), state()) -> state().
read_until(Delim, ReadFun, #{src := <<>>} = State) ->
  case check_reader(State) of
    {ok, NewState} ->
      read_until(Delim, ReadFun, NewState);
    eof ->
      {Line, Col} = scope_get(loc_started, State),
      ?ERROR( [ <<"Started reading at (">>
              , Line, <<":">>, Col
              , <<") but found EOF while expecting '">>
              , <<Delim/utf8>>
              , <<"'">>
              ]
            , location(State)
            )
  end;
read_until(Delim, ReadFun, State) ->
  State1 = scope_put(read_delim, true, State#{return_on => Delim}),
  try
    read_until(Delim, ReadFun, ReadFun(State1))
  catch throw:{return_on, State2} ->
      finish_read_until(State2)
  end.

-spec finish_read_until(state()) -> state().
finish_read_until(State) ->
  #{ forms         := Forms
   , pending_forms := PendingForms
   } = State,
  State1 = State#{ forms         := lists:reverse(PendingForms) ++ Forms
                 , pending_forms := []
                 , return_on     := ?NIL
                 },
  scope_put(read_delim, false, State1).

-spec is_macro_terminating(char()) -> boolean().
is_macro_terminating(Char) ->
  lists:member(Char,
               [$", $;, $@, $^, $`, $~, $(,
                $), $[, $], ${, $}, $\\ ]).

-spec is_whitespace(char()) -> boolean().
is_whitespace(Char) ->
  clj_utils:char_type(Char) =:= whitespace.

-spec skip_line(state()) -> state().
skip_line(State) ->
  NotNewline = fun(C) -> C =/= $\n andalso C =/= $\r end,
  {_, State1} = consume(State, NotNewline),
  State1.

-spec wrapped_read('clojerl.Symbol':type(), state()) -> state().
wrapped_read(Symbol, State) ->
  {Form, NewState} = read_pop_one(State),
  List = clj_rt:list([Symbol, Form]),
  push_form(List, NewState).

-spec read_pop_one(state()) -> {any(), state()}.
read_pop_one(State0) ->
  case read_one(State0) of
    #{forms := [Form | Forms]} = State1 ->
      {Form, State1#{forms => Forms}};
    State1 ->
      read_pop_one(State1)
  end.

-spec push_form(any(), state()) -> state().
push_form(Form, #{forms := Forms} = State) ->
  State#{forms => [Form | Forms]}.

-spec push_pending_form(any(), state()) -> state().
push_pending_form(Form, #{pending_forms := Forms} = State) ->
  State#{pending_forms => [Form | Forms]}.

-spec scope_get(atom(), state()) -> any().
scope_get(Name, #{bindings := Bindings} = _State) ->
  clj_scope:get(Name, Bindings).

-spec scope_put(atom(), any(), state()) -> state().
scope_put(Name, Value, #{bindings := Bindings} = State) ->
  State#{bindings => clj_scope:put(Name, Value, Bindings)}.

-spec add_scope(state()) -> state().
add_scope(#{bindings := Bindings} = State) ->
  State#{bindings => clj_scope:new(Bindings)}.

-spec remove_scope(state()) -> state().
remove_scope(#{bindings := Bindings} = State) ->
  State#{bindings => clj_scope:parent(Bindings)}.

-spec file_location_meta(state()) -> map().
file_location_meta(State) ->
  {Line, Col} = maps:get(loc, State),
  Opts = maps:get(opts, State),

  case maps:is_key(file, Opts) of
    true  -> #{ line   => Line
              , column => Col
              , file   => maps:get(file, Opts)
              };
    false -> #{ line   => Line
              , column => Col
              }
  end.

-spec peek_src(state()) -> integer() | eof.
peek_src(#{src := <<First/utf8, _/binary>>}) ->
  First;
peek_src(#{src := <<>>, opts := #{?OPT_IO_READER := Reader}}) ->
  case 'erlang.io.IReader':read(Reader) of
    eof -> eof;
    <<Ch/utf8>> = ChBin ->
      'erlang.io.IPushbackReader':unread(Reader, ChBin),
      Ch
  end;
peek_src(_State) ->
  eof.

-spec check_reader(state()) -> {ok, state()} | eof.
check_reader(#{src := <<>>, opts := #{?OPT_IO_READER := Reader}} = State)
  when Reader =/= ?NIL ->
  case 'erlang.io.IReader':read(Reader) of
    eof -> eof;
    Ch  -> {ok, State#{src := Ch}}
  end;
check_reader(#{src := <<>>}) ->
  eof.

-spec unread_char(state()) -> state().
unread_char(#{src := <<>>} = State) ->
  State;
unread_char( #{ src := <<Ch/utf8, Rest/binary>>
              , opts := #{?OPT_IO_READER := Reader}
              } = State
           ) ->
  'erlang.io.IPushbackReader':unread(Reader, <<Ch/utf8>>),
  State#{src => Rest};
unread_char(State) ->
  State.
