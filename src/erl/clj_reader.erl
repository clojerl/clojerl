-module(clj_reader).

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-export([ read_fold/4
        , read/1, read/2
        , location_meta/1
        , remove_location/1
        ]).

-type location() :: #{ line   => non_neg_integer()
                     , column => non_neg_integer()
                     , file   => binary() | ?NIL
                     }.

-type opts() :: #{ ?OPT_READ_COND => allow | preserve
                   %% When the value is `allow' then reader conditional will be
                   %% processed. If it is `preserve' then a ReaderConditional
                   %% value will be returned.
                 , ?OPT_FEATURES  => 'clojerl.Set':type()
                   %% Set of features available when processing a reader
                   %% conditional.
                 , ?OPT_EOF       => ?EOFTHROW | ok
                   %% When 'eofthrow' then throw, otherwise return value.
                 , file           => file:filename_all()
                   %% Source file being read.
                 , ?OPT_IO_READER => 'erlang.io.IPushbackReader':type()
                   %% IReader that should be used when there are no more
                   %% characters to be read from the binary.
                 }.

-export_type([location/0, opts/0]).

-type state() :: #{ src           => binary()
                    %% A binary the represents Clojure source code
                  , opts          => opts()
                    %% The options map supplied to the reader.
                  , forms         => [any()]
                    %% List of forms read (in reverse order).
                  , pending_forms => [any()]
                    %% Pending forms to be processed. Used by reader cond.
                  , env           => clj_env:env()
                    %% The current Clojure environment.
                  , loc           => {non_neg_integer(), non_neg_integer()}
                    %% Current line and column location.
                  , bindings      => clj_scope:scope()
                    %% Current bindings.
                  , return_on     => char() | ?NIL
                    %% Used when reading a string
                  , current       => binary() | ?NIL
                  }.

-type read_fold_fun() :: fun((any(), any()) -> any()).

-spec read_fold(read_fold_fun(), binary(), opts(), clj_env:env()) ->
  clj_env:env().
read_fold(Fun, Src, Opts0, Env) ->
  %% Since we want to consume all of the source, we don't want to
  %% throw when eof is reached.
  Opts  = Opts0#{?OPT_EOF => ok},
  State = new_state(Src, Env, Opts),
  read_fold_loop(Fun, State).

-spec read_fold_loop(read_fold_fun(), state()) -> clj_env:env().
read_fold_loop(Fun, State) ->
  NewState = try
               read_one(State)
             catch
               throw:{eof, ok, StateTmp} -> StateTmp
             end,
  case NewState of
    %% Only finish when there is no more source to consume
    #{src := <<>>, forms := [], env := Env} ->
      Env;
    NewState = #{forms := []} ->
      read_fold_loop(Fun, NewState);
    NewState = #{forms := [Form], env := Env} ->
      NewEnv = Fun(Form, Env),
      read_fold_loop(Fun, NewState#{env => NewEnv, forms => []})
  end.

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

-spec add_location_field(any(), any(), map()) -> map().
add_location_field(Meta, Name, Location) ->
  case clj_rt:get(Meta, Name) of
    ?NIL -> Location;
    Value -> maps:put(Name, Value, Location)
  end.

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

-spec read(binary()) -> any().
read(Src) ->
  read(Src, #{}).

%% @doc Reads the next form from the input. Returns the form
%%      or throws if there is no form to read.
-spec read(binary(), opts()) -> any().
read(Src, Opts) ->
  State = new_state(Src, clj_env:default(), Opts),
  try
    ensure_read(State)
  catch
    throw:{eof, Value, _} -> Value
  end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

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
  #{ src           => Src
   , opts          => platform_features(Opts)
   , forms         => []
   , pending_forms => []
   , env           => Env
   , loc           => {1, 1}
   , bindings      => clj_scope:new()
   , return_on     => ?NIL
   , current       => ?NIL
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
      clj_utils:error(<<"EOF">>, location(State));
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
  Number = clj_utils:parse_number(Current),
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
      clj_utils:error( [ <<"Started reading at (">>
                       , Line, <<":">>, Col
                       , <<") but found EOF while reading string">>
                       ]
                     , location(State)
                     )
  end.

-spec escape_char(state()) -> {binary(), state()}.
escape_char(State = #{src := <<>>}) ->
  case check_reader(State) of
    eof -> clj_utils:error(<<"EOF while escaping char">>, location(State));
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
      clj_utils:error_when( NextChar =/= eof
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
      clj_utils:error_when( CodePoint > 8#377
                          , <<"Octal escape sequence must be in "
                              "range [0, 377]">>
                          , location(State1)
                          ),
      {unicode:characters_to_binary([CodePoint], utf8), State1};
    _ ->
      clj_utils:error( <<"Unsupported escape character: \\", Char>>
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

  clj_utils:error_when( Size =/= Length
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
  clj_utils:error_when( InvalidChars =/= []
                      , [ <<"Invalid digit: ">>, InvalidChar]
                      , location(State)
                      ),

  NumberInt = binary_to_integer(Number, Base),
  clj_utils:error_when(  16#D800 =< NumberInt andalso NumberInt =< 16#DFFF
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
  Keyword = case clj_utils:parse_symbol(Token) of
              {?NIL, <<":", Name/binary>>} ->
                Ns    = 'clojerl.Namespace':current(),
                NsSym = 'clojerl.Namespace':name(Ns),
                Namespace = 'clojerl.Symbol':name(NsSym),
                clj_rt:keyword(Namespace, Name);
              {?NIL, Name} ->
                clj_rt:keyword(Name);
              {<<Ch/utf8, _/binary>> = Namespace, Name} when Ch =/= $: ->
                clj_rt:keyword(Namespace, Name);
              _ ->
                clj_utils:error( <<"Invalid token: :", Token/binary>>
                               , location(State)
                               )
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
               clj_utils:error( <<"Invalid token: ", Token/binary>>
                              , location(State)
                              )
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
    erlang:put(gensym_env, #{}),
    QuotedForm      = syntax_quote(Form),
    NewFormWithMeta = add_meta(Form, QuotedForm),

    push_form(NewFormWithMeta, NewState)
  catch error:Reason ->
      clj_utils:error(Reason, location(NewState))
  after
    erlang:erase(gensym_env)
  end.

-spec syntax_quote(any()) -> any().
syntax_quote(Form) ->
  IsSpecial    = clj_analyzer:is_special(Form),
  IsSymbol     = clj_rt:'symbol?'(Form),
  IsUnquote    = is_unquote(Form),
  IsUnquoteSpl = is_unquote_splicing(Form),
  IsColl       = clj_rt:'coll?'(Form),
  IsLiteral    = is_literal(Form),

  QuoteSymbol = clj_rt:symbol(<<"quote">>),
  if
    IsSpecial    -> clj_rt:list([QuoteSymbol, Form]);
    IsSymbol     ->
      Symbol = syntax_quote_symbol(Form),
      clj_rt:list([QuoteSymbol, Symbol]);
    IsUnquote    -> clj_rt:second(Form);
    IsUnquoteSpl -> throw(<<"unquote-splice not in list">>);
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
  GensymEnv = case erlang:get(gensym_env) of
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
      erlang:put(gensym_env, GensymEnv#{SymbolName => GenSym1}),
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
  case clj_rt:'meta?'(Form) of
    true ->
      WithMetaSym = clj_rt:symbol(<<"clojure.core">>, <<"with-meta">>),
      Meta = syntax_quote(clj_rt:meta(Form)),
      clj_rt:list([WithMetaSym, Result, Meta]);
    _ ->
      Result
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
    orelse clj_rt:'char?'(Form)
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
read_map(#{ src   := <<"{"/utf8, _/binary>>
          , forms := Forms
          , loc   := Loc
          } = State0
        ) ->
  State  = add_scope(consume_char(State0)),
  State1 = read_until($}, location_started(State#{forms => []}, Loc)),
  State2 = remove_scope(State1),
  #{forms := ReversedItems} = State2,

  case length(ReversedItems) of
    X when X rem 2 == 0 ->
      Items = lists:reverse(ReversedItems),
      Map = 'clojerl.Map':with_meta( clj_rt:hash_map(Items)
                                   , file_location_meta(State0)
                                   ),
      State2#{forms => [Map | Forms]};
    _ ->
      clj_utils:error( <<"Map literal must contain an even number of forms">>
                     , location(State2)
                     )
  end.

%%------------------------------------------------------------------------------
%% Unmatched delimiter
%%------------------------------------------------------------------------------

-spec read_unmatched_delim(state()) -> no_return().
read_unmatched_delim(#{src := <<Delim/utf8, _/binary>>} = State) ->
  clj_utils:error(<<"Umatched delimiter ", Delim/utf8>>, location(State)).

%%------------------------------------------------------------------------------
%% Character
%%------------------------------------------------------------------------------

-spec read_char(state()) -> state().
read_char(#{src := <<"\\"/utf8, _/binary>>} = State0) ->
  State    = consume_char(State0),
  NextChar = case peek_src(State) of
               eof -> clj_utils:error( <<"EOF while reading character">>
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
        clj_utils:error_when( TokenLength =/= 4
                            , [ <<"Invalid unicode character: \\u">>
                              , RestToken
                              ]
                            , location(State1)
                            ),
        {Ch, _} = unicode_char(State1#{src => RestToken}, 16, 4, true),
        Ch;
      <<"o", RestToken/binary>> ->
        read_octal_char(State1#{src => RestToken});
      Ch -> clj_utils:error( <<"Unsupported character: \\", Ch/binary>>
                           , location(State)
                           )
    end,

  CharBin = unicode:characters_to_binary([Char]),
  push_form(CharBin, State1).

-spec read_octal_char(state()) -> char().
read_octal_char(#{src := RestToken} = State) when size(RestToken) > 3 ->
  Size    = size(RestToken),
  SizeBin = integer_to_binary(Size),
  clj_utils:error( <<"Invalid octal escape sequence length: ", SizeBin/binary>>
                 , location(State)
                 );
read_octal_char(#{src := RestToken} = State) ->
  Size = size(RestToken),
  case unicode_char(State, 8, Size, true) of
    {Ch, _} when Ch > 8#377 ->
      clj_utils:error( <<"Octal escape sequence must be in range [0, 377]">>
                     , location(State)
                     );
    {Ch, _} -> Ch
  end.

%%------------------------------------------------------------------------------
%% Argument
%%------------------------------------------------------------------------------

-spec read_arg(state()) -> state().
read_arg(#{src := <<"%"/utf8, _/binary>>} = State) ->
  case erlang:get(arg_env) of
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
          clj_utils:error_when( not is_integer(N)
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
      erlang:put(arg_env, NewArgEnv),
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
      clj_utils:error(<<"EOF while reading dispatch">>, location(State))
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
    $< -> clj_utils:error(<<"Unreadable form">>, location(State));
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
  clj_utils:error_when(erlang:get(arg_env) =/= undefined
                      , <<"Nested #()s are not allowed">>
                      , location(State)
                      ),

  {{Form, NewState}, ArgEnv} =
    try
      erlang:put(arg_env, #{}),
      {read_pop_one(State), erlang:get(arg_env)}
    after
      %% Make sure the process dictionary entry gets removed
      erlang:erase(arg_env)
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
  Set         = clj_rt:hash_set(Items),
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
      clj_utils:error(<<"EOF while reading regex">>, location(State))
  end.

%%------------------------------------------------------------------------------
%% #_ discard
%%------------------------------------------------------------------------------

-spec read_discard(state()) -> state().
read_discard(State) ->
  {_, NewState} = read_pop_one(State),
  %% Can't call read_one here because is might not be a top level form.
  NewState.

%%------------------------------------------------------------------------------
%% #? cond
%%------------------------------------------------------------------------------

-spec read_cond(state()) -> state().
read_cond(#{src := <<>>} = State) ->
  case check_reader(State) of
    {ok, NewState} ->
      read_cond(NewState);
    eof ->
      clj_utils:error(<<"EOF while reading cond">>, location(State))
  end;
read_cond(#{opts := Opts} = State0) ->
  ReadCondOpt = maps:get(?OPT_READ_COND, Opts, ?NIL),
  clj_utils:error_when(not lists:member(ReadCondOpt, [allow, preserve])
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

  clj_utils:error_when( peek_src(State2) =/= $(
                      , <<"read-cond body must be a list">>
                      , location(State1)
                      ),

  OldSupressRead = case erlang:get(supress_read) of
                     undefined -> false;
                     SR -> SR
                   end,
  SupressRead    = OldSupressRead orelse ReadCondOpt == preserve,
  erlang:put(supress_read, SupressRead),

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
    erlang:put(supress_read, OldSupressRead)
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
      clj_utils:error_when( not clj_rt:'sequential?'(Form)
                          , <<"Spliced form list in "
                              "read-cond-splicing must "
                              "extend clojerl.ISequential">>
                          , location(State1)
                          ),

      ReadDelim  = clj_rt:boolean(scope_get(read_delim, State)),
      clj_utils:error_when( IsSplicing andalso not ReadDelim
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
  {match | nomatch, any(), state()} | {finished, state()}.
match_feature(State = #{return_on := ReturnOn, opts := Opts}) ->
  Features = maps:get(?OPT_FEATURES, Opts, clj_rt:hash_set([])),
  try
    %% Change the return_on value so that we only read until the next
    %% ')' char.
    {Feature, State1} = read_pop_one(State#{return_on => $)}),
    clj_utils:error_when( not clj_rt:'keyword?'(Feature)
                        , <<"Feature should be a keyword">>
                        , location(State)
                        ),

    clj_utils:error_when( lists:member(Feature, ?RESERVED_FEATURES)
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
        clj_utils:error( <<"read-cond requires an even number of forms">>
                       , location(State3)
                       )
    end
  catch
    throw:{return_on, State4} ->
      {finished, State4#{return_on => ReturnOn}}
  end.

-spec read_skip_suppress(state()) -> state().
read_skip_suppress(State) ->
  OldSupressRead = case erlang:get(supress_read) of
                     undefined -> false;
                     SR -> SR
                   end,
  erlang:put(supress_read, true),
  try
    {_, NewState} = read_pop_one(State),
    NewState
  after
    erlang:put(supress_read, OldSupressRead)
  end.

%%------------------------------------------------------------------------------
%% # reader tag
%%------------------------------------------------------------------------------

-spec read_tagged(state()) -> state().
read_tagged(State) ->
  {Symbol, State1} = read_pop_one(State),

  clj_utils:error_when( not clj_rt:'symbol?'(Symbol)
                      , <<"Reader tag must be a symbol">>
                      , location(State)
                      ),

  SupressRead = erlang:get(supress_read),
  {Form, State2} = read_pop_one(State1),
  case 'clojerl.Symbol':str(Symbol) of
    <<"erl">> -> erlang_literal(Form, State2);
    <<"bin">> -> erlang_binary(Form, State2);
    <<"as">>  -> erlang_alias(Form, State2);
    _ when SupressRead =:= true ->
      push_form(tagged_literal(Symbol, Form), State2);
    _ ->
      case 'clojerl.String':contains('clojerl.Symbol':name(Symbol), <<".">>) of
        true  -> read_record(Symbol, Form, State2);
        false -> read_tagged(Symbol, Form, location(State), State2)
      end
  end.

-spec erlang_literal(any(), state()) -> state().
erlang_literal(Form, State) ->
  IsList   = clj_rt:'list?'(Form),
  IsMap    = clj_rt:'map?'(Form),
  IsVector = clj_rt:'vector?'(Form),
  IsString = clj_rt:'string?'(Form),

  Value    =
    if
      IsList   ->
        ErlListSym = clj_rt:symbol(<<"erl-list*">>),
        List       = clj_rt:to_list(Form),
        clj_rt:list([ErlListSym | List]);
      IsMap    -> 'clojerl.Map':to_erl_map(Form);
      IsVector -> list_to_tuple('clojerl.Vector':to_list(Form));
      IsString -> clj_rt:list( [ clj_rt:symbol(<<"erl-list*">>)
                               | unicode:characters_to_list(Form)
                               ]
                               );
      true     -> clj_utils:error( [ <<"Can only have list, map, tuple ">>
                                   , <<"or Erlang string literals, had: ">>
                                   , clj_rt:type(Form)
                                   ]
                                 , location(State)
                                 )
    end,

  push_form(Value, State).

-spec erlang_binary(any(), state()) -> state().
erlang_binary(Form, State) ->
  clj_utils:error_when( not clj_rt:'vector?'(Form)
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

  clj_utils:error_when( not clj_rt:boolean('clojerl.Var':deref(ReadEvalVar))
                      , <<"Record construction syntax can only be used "
                          "when *read-eval* == true">>
                      , location(State)
                      ),

  clj_utils:error_when( not clj_rt:'vector?'(Form)
                        andalso not clj_rt:'map?'(Form)
                      , [ <<"Unreadable constructor form starting with \"#">>
                        , Symbol
                        , <<"\"">>
                        ]
                      , location(State)
                      ),

  Type = try erlang:binary_to_existing_atom('clojerl.Symbol':str(Symbol), utf8)
         catch throw:badarg ->
             clj_utils:error( [Symbol, <<" is not loaded or doesn't exist">>]
                            , location(State)
                            )
         end,

  Record =
    case clj_rt:'vector?'(Form) of
      true  ->
        ArgCount = clj_rt:count(Form),
        Exported = erlang:function_exported(Type, ?CONSTRUCTOR, ArgCount),

        clj_utils:error_when( not Exported
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

        clj_utils:error_when( NonKeywords =/= []
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

  clj_utils:error_when( Reader2 =:= ?NIL
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
    false -> {Acc, State}
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
      {Acc, State}
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
      clj_utils:error( [ <<"Started reading at (">>
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
  State1 = read_one(State0),
  #{forms := [Form | Forms]} = State1,
  {Form, State1#{forms => Forms}}.

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
