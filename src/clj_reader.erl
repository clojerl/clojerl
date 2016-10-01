-module(clj_reader).

-include("clojerl.hrl").

-export([ read_fold/4
        , read/1, read/2, read/3
        , location_meta/1
        , remove_location/1
        ]).

-define(PLATFORM_FEATURES, [clje]).

-define(OPT_EOF, eof).
-define(OPT_FEATURES, features).
-define(OPT_READ_COND, 'read-cond').
-define(OPT_IO_READER, 'io-reader').

-define(EOFTHROW, eofthrow).

-type location() :: #{ line   => non_neg_integer()
                     , column => non_neg_integer()
                     , file   => binary()
                     }.

-type opts() :: #{ ?OPT_READ_COND => allow | preserve
                   %% When the value is `allow' then reader conditional will be
                   %% processed. If it is `preserve' then a ReaderConditional
                   %% value will be returned.
                 , ?OPT_FEATURES  => 'clojerl.Set':type()
                   %% Set of features available when processing a reader
                   %% conditional.
                 , ?OPT_EOF       => any()
                   %% When 'eofthrow' then throw, otherwise return value.
                 , file           => file:filename_all()
                   %% Source file being read.
                 , ?OPT_IO_READER => 'erlang.io.IReader':type()
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
                  , return_on     => char()
                  }.

-type read_fold_fun() :: fun((any(), any()) -> any()).

-spec read_fold(read_fold_fun(), binary(), opts(), clj_env:env()) ->
  clj_env:env().
read_fold(Fun, Src, Opts0, Env) ->
  %% Since we want to consume all of the source, we don't want to
  %% throw when eof is reached.
  Opts  = Opts0#{eof => ok},
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

-spec location_meta(any()) -> location().
location_meta(X) ->
  case clj_core:'meta?'(X) of
    true  ->
      Meta = clj_core:meta(X),
      #{ line   => clj_core:get(Meta, line, undefined)
       , column => clj_core:get(Meta, column, undefined)
       , file   => clj_core:get(Meta, file, undefined)
       };
    false -> undefined
  end.

-spec remove_location(any()) -> any().
remove_location(undefined) ->
  undefined;
remove_location(Meta) ->
  Meta1 = clj_core:dissoc(Meta, file),
  Meta2 = clj_core:dissoc(Meta1, line),
  Meta3 = clj_core:dissoc(Meta2, column),
  case clj_core:'empty?'(Meta3) of
    true  -> undefined;
    false -> Meta3
  end.

-spec read(binary()) -> any().
read(Src) ->
  read(Src, #{}).

-spec read(binary(), opts()) -> any().
read(Src, Opts) ->
  read(Src, Opts, clj_env:default()).

%% @doc Reads the next form from the input. Returns the form
%%      or throws if there is no form to read.
-spec read(binary(), opts(), clj_env:env()) -> any().
read(Src, Opts, Env) ->
  State = new_state(Src, Env, Opts),
  try
    ensure_read(State)
  catch
    throw:{eof, Value, _} -> Value
  end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec platform_features(opts() | undefined) -> opts().
platform_features(undefined) ->
  #{?OPT_FEATURES => clj_core:hash_set(?PLATFORM_FEATURES)};
platform_features(#{?OPT_FEATURES := Features} = Opts) ->
  NewFeatures = lists:foldl( fun(Feature, Acc) ->
                                 clj_core:conj(Acc, Feature)
                             end
                           , Features
                           , ?PLATFORM_FEATURES
                           ),
  Opts#{?OPT_FEATURES => NewFeatures};
platform_features(Opts) ->
  Opts#{?OPT_FEATURES => clj_core:hash_set(?PLATFORM_FEATURES)}.

%% @private
-spec new_state(binary(), any(), opts()) -> state().
new_state(Src, Env, Opts) ->
  #{ src           => Src
   , opts          => platform_features(Opts)
   , forms         => []
   , pending_forms => []
   , env           => Env
   , loc           => {1, 1}
   , bindings      => clj_scope:new()
   , return_on     => undefined
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
      clj_utils:throw(<<"EOF">>, location(State));
    eof ->
      throw({eof, Eof, State})
  end;
read_one(#{ src := <<Ch/utf8, _/binary>>, return_on := Ch} = State) ->
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
%% Found closing double quotes
read_string(#{ src     := <<"\"", _/binary>>
             , current := String
             } = State0
           ) ->
  State = consume_char(maps:remove(current, State0)),
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
%% Start consuming string
read_string(#{src := <<"\"", _/binary>>, loc := Loc} = State) ->
  State1 = add_scope(consume_char(State)),
  read_string(location_started(State1#{current => <<>>}, Loc));
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
    eof -> clj_utils:throw(<<"EOF while escaping char">>, location(State));
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
                          , <<"Octal escape sequence must be in range [0, 377]">>
                          , location(State1)
                          ),
      {unicode:characters_to_binary([CodePoint], utf8), State1};
    _ ->
      clj_utils:error( <<"Unsupported escape character: \\", Char>>
                     , location(State)
                     )
  end.

-spec unicode_char(state(), integer(), integer(), Exact :: boolean()) ->
  {binary(), state()}.
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
                   [] -> undefined;
                   [C | _] -> <<C/utf8>>
                 end,
  clj_utils:error_when( InvalidChars =/= []
                      , [ <<"Invalid digit: ">>, InvalidChar]
                      , location(State)
                      ),

  {binary_to_integer(Number, Base), State1}.

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
              {undefined, <<":", Name/binary>>} ->
                Ns    = clj_namespace:current(),
                NsSym = clj_namespace:name(Ns),
                Namespace = clj_core:name(NsSym),
                clj_core:keyword(Namespace, Name);
              {undefined, Name} ->
                clj_core:keyword(Name);
              {<<Ch/utf8, _/binary>> = Namespace, Name} when Ch =/= $: ->
                clj_core:keyword(Namespace, Name);
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
  Meta = file_location_meta(State),

  {Token, State1} = read_token(State),
  Symbol = case clj_utils:parse_symbol(Token) of
             {undefined, <<"nil">>}   -> undefined;
             {undefined, <<"true">>}  -> true;
             {undefined, <<"false">>} -> false;
             {undefined, Name} ->
               clj_core:with_meta(clj_core:symbol(Name), Meta);
             {Ns, Name} ->
               clj_core:with_meta(clj_core:symbol(Ns, Name), Meta);
             undefined ->
               clj_utils:error(<<"Invalid token: ", Token/binary>>
                              , location(State)
                              )
           end,

  push_form(Symbol, State1).

%%------------------------------------------------------------------------------
%% Comment
%%------------------------------------------------------------------------------

-spec read_comment(state()) -> state().
read_comment(State) -> skip_line(State).

%%------------------------------------------------------------------------------
%% Quote
%%------------------------------------------------------------------------------

-spec read_quote(state()) -> state().
read_quote(#{src := <<"'"/utf8, _/binary>>} = State) ->
  Quote = clj_core:symbol(<<"quote">>),
  wrapped_read(Quote, consume_char(State)).

%%------------------------------------------------------------------------------
%% Deref
%%------------------------------------------------------------------------------

-spec read_deref(state()) -> state().
read_deref(#{src := <<"@"/utf8, _/binary>>} = State) ->
  Deref = clj_core:symbol(<<"clojure.core">>, <<"deref">>),
  wrapped_read(Deref, consume_char(State)).

%%------------------------------------------------------------------------------
%% Meta
%%------------------------------------------------------------------------------

-spec read_meta(state()) -> state().
read_meta(#{src := <<"^"/utf8, Src/binary>>} = State) ->
  {SugaredMeta, State1} = read_pop_one(State#{src => Src}),
  Meta    = clj_utils:desugar_meta(SugaredMeta),
  LocMeta = file_location_meta(State),
  AllMeta = clj_core:merge([LocMeta, Meta]),

  {Form, State2} = read_pop_one(State1),
  NewForm = clj_core:with_meta(Form, AllMeta),

  push_form(NewForm, State2).

%%------------------------------------------------------------------------------
%% Syntax quote
%%------------------------------------------------------------------------------

-spec read_syntax_quote(state()) -> state().
read_syntax_quote(#{src := <<"`"/utf8, _/binary>>, env := Env} = State) ->
  {Form, NewState} = read_pop_one(consume_char(State)),

  try
    %% TODO: using process dictionary here might be a code smell
    erlang:put(gensym_env, #{}),
    {QuotedForm, Env1}      = syntax_quote(Form, Env),
    {NewFormWithMeta, Env2} = add_meta(Form, Env1, QuotedForm),

    push_form(NewFormWithMeta, NewState#{env => Env2})
  catch error:Reason ->
      clj_utils:throw(Reason, location(NewState))
  after
    erlang:erase(gensym_env)
  end.

-spec syntax_quote(any(), clj_env:env()) -> {any(), clj_env:env()}.
syntax_quote(Form, Env) ->
  IsSpecial    = clj_analyzer:is_special(Form),
  IsSymbol     = clj_core:'symbol?'(Form),
  IsUnquote    = is_unquote(Form),
  IsUnquoteSpl = is_unquote_splicing(Form),
  IsColl       = clj_core:'coll?'(Form),
  IsLiteral    = is_literal(Form),

  QuoteSymbol = clj_core:symbol(<<"quote">>),
  if
    IsSpecial    -> {clj_core:list([QuoteSymbol, Form]), Env};
    IsSymbol     ->
      Symbol = syntax_quote_symbol(Form),
      {clj_core:list([QuoteSymbol, Symbol]), Env};
    IsUnquote    -> {clj_core:second(Form), Env};
    IsUnquoteSpl -> throw(<<"unquote-splice not in list">>);
    IsColl ->
      IsMap = clj_core:'map?'(Form),
      IsVector = clj_core:'vector?'(Form),
      IsSet = clj_core:'set?'(Form),
      IsList = clj_core:'list?'(Form),
      if
        IsMap ->
          HashMapSymbol = clj_core:symbol(<<"clojure.core">>, <<"hash-map">>),
          syntax_quote_coll(flatten_map(Form), HashMapSymbol, Env);
        IsVector ->
          VectorSymbol = clj_core:symbol(<<"clojure.core">>, <<"vector">>),
          syntax_quote_coll(Form, VectorSymbol, Env);
        IsSet ->
          HashSetSymbol = clj_core:symbol(<<"clojure.core">>, <<"hash-set">>),
          syntax_quote_coll(Form, HashSetSymbol, Env);
        IsList ->
          ListSymbol = clj_core:symbol(<<"clojure.core">>, <<"list">>),
          syntax_quote_coll(Form, ListSymbol, Env);
        true ->
          syntax_quote_coll(Form, undefined, Env)
      end;
    IsLiteral -> {Form, Env};
    true      -> {clj_core:list([QuoteSymbol, Form]), Env}
  end.

-spec flatten_map(any()) -> any().
flatten_map(Map) ->
  MapSeq = clj_core:seq(Map),
  flatten_map(MapSeq, clj_core:vector([])).

-spec flatten_map(any() | undefined, 'clojerl.Vector':type()) -> any().
flatten_map(undefined, Vector) ->
  clj_core:seq(Vector);
flatten_map(MapSeq, Vector) ->
  First = clj_core:first(MapSeq),
  Vector1 = clj_core:conj(Vector, clj_core:first(First)),
  Vector2 = clj_core:conj(Vector1, clj_core:second(First)),
  flatten_map(clj_core:next(MapSeq), Vector2).

-spec syntax_quote_symbol(any()) -> any().
syntax_quote_symbol(Symbol) ->
  NamespaceStr = clj_core:namespace(Symbol),
  NameStr = clj_core:name(Symbol),
  IsGenSym = 'clojerl.String':ends_with(NameStr, <<"#">>),
  case {NamespaceStr, IsGenSym} of
    {undefined, true} ->
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
  case maps:get(clj_core:name(Symbol), GensymEnv, undefined) of
    undefined ->
      NameStr = clj_core:name(Symbol),
      NameStr2 = binary:part(NameStr, 0, byte_size(NameStr) - 1),
      Parts = [NameStr2, <<"__">>, clj_core:next_id(), <<"__auto__">>],
      PartsStr = lists:map(fun clj_core:str/1, Parts),
      GenSym = clj_core:symbol(erlang:iolist_to_binary(PartsStr)),
      SymbolName = clj_core:name(Symbol),
      erlang:put(gensym_env, GensymEnv#{SymbolName => GenSym}),
      GenSym;
    GenSym ->
      GenSym
  end.

-spec resolve_symbol(any()) -> any().
resolve_symbol(Symbol) ->
  case clj_namespace:find_var(Symbol) of
    undefined ->
      case clj_core:namespace(Symbol) of
        undefined ->
          CurrentNs = clj_namespace:current(),
          NameSym   = clj_namespace:name(CurrentNs),
          Namespace = clj_core:name(NameSym),
          Name      = clj_core:name(Symbol),
          clj_core:symbol(Namespace, Name);
        _ ->
          Symbol
      end;
    Var ->
      Namespace = clj_core:namespace(Var),
      Name      = clj_core:name(Var),
      clj_core:symbol(Namespace, Name)
  end.

-spec syntax_quote_coll(any(), 'clojerl.Symbol':type(), clj_env:env()) ->
  {any(), clj_env:env()}.
syntax_quote_coll(List, undefined, Env) ->
  syntax_quote_coll(List, Env);
syntax_quote_coll(List, FunSymbol, Env) ->
  {ExpandedList, Env1} = syntax_quote_coll(List, Env),
  ApplySymbol = clj_core:symbol(<<"clojure.core">>, <<"apply">>),
  {clj_core:list([ApplySymbol, FunSymbol, ExpandedList]), Env1}.

-spec syntax_quote_coll(any(), clj_env:env()) -> {any(), clj_env:env()}.
syntax_quote_coll(List, Env) ->
  case clj_core:'empty?'(List) of
    true ->
      ListSymbol = clj_core:symbol(<<"clojure.core">>, <<"list">>),
      {clj_core:list([ListSymbol]), Env};
    false ->
      {ReversedExpandedItems, Env1} = expand_list(List, Env, []),
      ExpandedItems = lists:reverse(ReversedExpandedItems),
      ConcatSymbol = clj_core:symbol(<<"clojure.core">>, <<"concat">>),
      {clj_core:list([ConcatSymbol | ExpandedItems]), Env1}
  end.

-spec expand_list(any(), clj_env:env(), any()) -> {any(), clj_env:env()}.
expand_list(undefined, Env, Result) ->
  {Result, Env};
expand_list(List, Env, Result) ->
  Item = clj_core:first(List),
  ListSymbol = clj_core:symbol(<<"clojure.core">>, <<"list">>),
  {NewItem, Env2} =
    case {is_unquote(Item), is_unquote_splicing(Item)} of
      {true, _} ->
        {clj_core:list([ListSymbol, clj_core:second(Item)]), Env};
      {_, true} ->
        {clj_core:second(Item), Env};
      _ ->
        {QuotedForm, Env1} = syntax_quote(Item, Env),
        {clj_core:list([ListSymbol, QuotedForm]), Env1}
      end,
  expand_list(clj_core:next(List), Env2, [NewItem | Result]).

-spec add_meta(any(), clj_env:env(), any()) -> {any(), clj_env:env()}.
add_meta(Form, Env, Result) ->
  case clj_core:'meta?'(Form) of
    true ->
      WithMetaSym = clj_core:symbol(<<"clojure.core">>, <<"with-meta">>),
      {Meta, Env1} = syntax_quote(clj_core:meta(Form), Env),
      {clj_core:list([WithMetaSym, Result, Meta]), Env1};
    _ ->
      {Result, Env}
  end.

is_unquote(Form) ->
  clj_core:'seq?'(Form) andalso
    clj_core:first(Form) == clj_core:symbol(<<"clojure.core">>, <<"unquote">>).

is_unquote_splicing(Form) ->
  clj_core:'seq?'(Form) andalso
    clj_core:first(Form) == clj_core:symbol(<<"clojure.core">>,
                                            <<"unquote-splicing">>).

is_literal(Form) ->
  clj_core:'keyword?'(Form)
    orelse clj_core:'number?'(Form)
    orelse clj_core:'char?'(Form)
    orelse clj_core:'string?'(Form)
    orelse clj_core:'nil?'(Form)
    orelse clj_core:'boolean?'(Form)
    orelse clj_core:'regex?'(Form).

%%------------------------------------------------------------------------------
%% Unquote
%%------------------------------------------------------------------------------

-spec read_unquote(state()) -> state().
read_unquote(#{src := <<"\~"/utf8, Src/binary>>} = State) ->
  case Src of
    <<"@", _/binary>> ->
      UnquoteSplicing = clj_core:symbol(<<"clojure.core">>,
                                        <<"unquote-splicing">>),
      wrapped_read(UnquoteSplicing, consume_chars(2, State));
    _ ->
      UnquoteSplicing = clj_core:symbol(<<"clojure.core">>,
                                        <<"unquote">>),
      wrapped_read(UnquoteSplicing, consume_char(State))
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
  List = clj_core:with_meta(clj_core:list(Items), file_location_meta(State0)),

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
  Vector = clj_core:with_meta( clj_core:vector(Items)
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
      Map = clj_core:with_meta( clj_core:hash_map(Items)
                              , file_location_meta(State0)
                              ),
      State2#{forms => [Map | Forms]};
    _ ->
      clj_utils:throw( <<"Map literal must contain an even number of forms">>
                     , location(State2)
                     )
  end.

%%------------------------------------------------------------------------------
%% Unmatched delimiter
%%------------------------------------------------------------------------------

-spec read_unmatched_delim(state()) -> no_return().
read_unmatched_delim(#{src := <<Delim/utf8, _/binary>>} = State) ->
  clj_utils:throw(<<"Umatched delimiter ", Delim/utf8>>, location(State)).

%%------------------------------------------------------------------------------
%% Character
%%------------------------------------------------------------------------------

-spec read_char(state()) -> state().
read_char(#{src := <<"\\"/utf8, NextChar/utf8,  _/binary>>} = State) ->
  {Token, State1} =
    case is_macro_terminating(NextChar) orelse is_whitespace(NextChar) of
      true -> {<<NextChar/utf8>>, consume_chars(2, State)};
      false -> read_token(consume_char(State))
    end,
  Char =
    case Token of
      Ch when size(Ch) == 1 -> Ch;
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
  push_form(CharBin, State1);
read_char(State) ->
  clj_utils:error( <<"EOF while reading character">>
                 , location(State)
                 ).

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
          clj_utils:throw_when( not is_integer(N)
                              , <<"Arg literal must be %, %& or %integer">>
                              , location(State1)
                              ),
          ArgSym = register_arg(N, ArgEnv),
          push_form(ArgSym, State2)
      end
  end.

-spec arg_type(state()) -> atom().
arg_type(State = #{src := <<>>}) ->
  case check_reader(State) of
    eof -> {register_arg_1, State};
    {ok, NewState} -> arg_type(NewState)
  end;
arg_type(State = #{src := <<Char/utf8, _/binary>>}) ->
  IsWhitespace = clj_utils:char_type(Char) == whitespace,
  IsMacroTerminating = is_macro_terminating(Char),
  Type = if
           IsWhitespace orelse IsMacroTerminating -> register_arg_1;
           Char == $& -> register_arg_multi;
           true -> register_arg_n
         end,
  {Type, State}.

-spec register_arg(integer(), map()) -> 'clojerl.Symbol':type().
register_arg(N, ArgEnv) ->
  case maps:get(N, ArgEnv, undefined) of
    undefined ->
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
  NextId = clj_core:next_id(),
  Parts = lists:map(fun clj_core:str/1, [Param, <<"__">>, NextId, <<"#">>]),
  Name = erlang:iolist_to_binary(Parts),
  clj_core:symbol(Name).

%%------------------------------------------------------------------------------
%% Reader dispatch
%%------------------------------------------------------------------------------

-spec read_dispatch(state()) -> state().
read_dispatch(#{src := <<"#">>} = State) ->
  case check_reader(State#{src := <<>>}) of
    {ok, NewState = #{src := NewSrc}}  ->
      read_dispatch(NewState#{src := <<"#", NewSrc/binary>>});
    eof ->
      clj_utils:throw(<<"EOF while reading dispatch">>, location(State))
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
    $[ -> read_tuple(NewState);
    $" -> read_regex(NewState);
    $! -> read_comment(NewState);
    $_ -> read_discard(NewState);
    $? -> read_cond(NewState);
    $< -> clj_utils:throw(<<"Unreadable form">>, location(State));
    _  -> read_tagged(consume_char(State))
  end.

%%------------------------------------------------------------------------------
%% #' var
%%------------------------------------------------------------------------------

-spec read_var(state()) -> state().
read_var(#{src := <<"'", _/binary>>} = State) ->
  VarSymbol = clj_core:symbol(<<"var">>),
  wrapped_read(VarSymbol, consume_char(State)).

%%------------------------------------------------------------------------------
%% #() fn
%%------------------------------------------------------------------------------

-spec read_fn(state()) -> state().
read_fn(State) ->
  case erlang:get(arg_env) of
    undefined -> ok;
    _         -> clj_utils:throw( <<"Nested #()s are not allowed">>
                                , location(State)
                                )
  end,
  erlang:put(arg_env, #{}),
  {Form, NewState} = read_pop_one(State),
  ArgEnv = erlang:erase(arg_env),

  MaxArg = lists:max([0 | maps:keys(ArgEnv)]),
  MapFun = fun(N) ->
               maps:get(N, ArgEnv, gen_arg_sym(N))
           end,
  ArgsSyms = lists:map(MapFun, lists:seq(1, MaxArg)),
  ArgsSyms2 = case maps:get(-1, ArgEnv, undefined) of
                undefined -> ArgsSyms;
                RestArgSym  ->
                  AmpSym = clj_core:symbol(<<"&">>),
                  ArgsSyms ++ [AmpSym, RestArgSym]
              end,
  ArgsVector = clj_core:vector(ArgsSyms2),

  FnSymbol = clj_core:symbol(<<"fn*">>),
  FnForm = clj_core:list([FnSymbol, ArgsVector, Form]),
  FnFormWithMeta = clj_core:with_meta(FnForm, file_location_meta(State)),

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
  Set         = clj_core:hash_set(Items),
  SetWithMeta = clj_core:with_meta(Set, file_location_meta(State0)),

  State2#{forms => [SetWithMeta | Forms]}.

%%------------------------------------------------------------------------------
%% #[] tuple
%%------------------------------------------------------------------------------

-spec read_tuple(state()) -> state().
read_tuple(#{forms := Forms, loc := Loc} = State0) ->
  State  = add_scope(State0),
  State1 = read_until($], location_started(State#{forms => []}, Loc)),
  State2 = remove_scope(State1),
  #{forms := ReversedItems} = State2,

  Items = lists:reverse(ReversedItems),
  Tuple = erlang:list_to_tuple(Items),

  State2#{forms => [Tuple | Forms]}.

%%------------------------------------------------------------------------------
%% #"" regex
%%------------------------------------------------------------------------------

-spec read_regex(state()) -> state().
read_regex(#{src := <<"\\"/utf8, Ch/utf8, _/binary>>} = State) ->
  Current = maps:get(current, State, <<>>),
  NewState = State#{current => <<Current/binary, "\\", Ch/utf8>>},
  read_regex(consume_chars(2, NewState));
read_regex(#{src := <<"\""/utf8, _/binary>>} = State) ->
  Current = maps:get(current, State, <<>>),
  Regex = 'erlang.util.Regex':?CONSTRUCTOR(Current),
  push_form(Regex, consume_char(maps:remove(current, State)));
read_regex(#{src := <<Ch/utf8, _/binary>>} = State) ->
  Current = maps:get(current, State, <<>>),
  NewState = State#{current => <<Current/binary, Ch/utf8>>},
  read_regex(consume_char(NewState));
read_regex(#{src := <<>>} = State) ->
  case check_reader(State) of
    {ok, NewState}  ->
      read_regex(NewState);
    eof ->
      clj_utils:throw(<<"EOF while reading regex">>, location(State))
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
  ReadCondOpt = maps:get(?OPT_READ_COND, Opts, undefined),
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

  OldSupressRead = clj_core:boolean(erlang:get(supress_read)),
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
      clj_utils:error_when( not clj_core:'sequential?'(Form)
                          , <<"Spliced form list in "
                              "read-cond-splicing must "
                              "extend clojerl.ISequential">>
                          , location(State1)
                          ),

      ReadDelim  = clj_core:boolean(scope_get(read_delim, State)),
      clj_utils:error_when( IsSplicing andalso not ReadDelim
                          , <<"Reader conditional splicing "
                              "not allowed at the top level">>
                          , location(State)
                          ),

      State2 = read_until($), fun read_skip_suppress/1, State1),
      Items = lists:reverse(clj_core:seq_to_list(Form)),
      lists:foldl(fun push_pending_form/2, State2, Items);
    {match, Form, State1} ->
      State2 = read_until($), fun read_skip_suppress/1, State1),
      push_form(Form, State2)
  end.

-spec match_feature(state()) ->
  {match | nomatch, any(), state()} | {finished, state()}.
match_feature(State = #{return_on := ReturnOn, opts := Opts}) ->
  Features = maps:get(?OPT_FEATURES, Opts, clj_core:hash_set([])),
  try
    {Feature, State1} = read_pop_one(State#{return_on => $)}),
    clj_utils:error_when( not clj_core:'keyword?'(Feature)
                        , <<"Feature should be a keyword">>
                        , location(State)
                        ),
    try
      case clj_core:'contains?'(Features, Feature) of
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
  OldSupressRead = clj_core:boolean(erlang:get(supress_read)),
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

  clj_utils:throw_when( not clj_core:'symbol?'(Symbol)
                      , <<"Reader tag must be a symbol">>
                      , location(State)
                      ),

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

  DataReaders = clj_core:deref(DataReadersVar),
  Reader0     = clj_core:get(DataReaders, Symbol, undefined),

  DefaultDataReaders = clj_core:deref(DefaultDataReadersVar),
  Reader1 = case
              Reader0 =/= undefined
              orelse clj_core:get(DefaultDataReaders, Symbol, undefined)
            of
              true -> Reader0;
              DefaultDataReader -> DefaultDataReader
            end,

  {IsDefault, Reader2} = case Reader1 =/= undefined of
                           true  -> {false, Reader1};
                           false -> {true, clj_core:deref(DefaultReaderFunVar)}
                         end,

  {Form, State2} = read_pop_one(State1),
  case clj_core:boolean(erlang:get(supress_read)) of
    true ->
      push_form(tagged_literal(Symbol, Form), State2);
    false ->
      clj_utils:error_when( Reader2 =:= undefined
                          , [<<"No reader function for tag ">>, Symbol]
                          , location(State)
                          ),

      ReadForm = case IsDefault of
                   true ->
                     clj_core:invoke(Reader2, [Symbol, Form]);
                   false ->
                     clj_core:invoke(Reader2, [Form])
                 end,
      push_form(ReadForm, State2)
    end.

-spec tagged_literal(any(), any()) -> 'clojerl.reader.Taggedliteral':type().
tagged_literal(Tag, Form) ->
  'clojerl.reader.TaggedLiteral':?CONSTRUCTOR(Tag, Form).

%%------------------------------------------------------------------------------
%% Utility functions
%%------------------------------------------------------------------------------

-spec location(state()) -> location().
location(#{opts := Opts} = State) ->
  {Line, Col} = case maps:get(loc, State, undefined) of
                  undefined -> {undefined, undefined};
                  Loc       -> Loc
                end,
  #{ line   => Line
   , column => Col
   , file   => maps:get(file, Opts, undefined)
   }.

-spec location_started(state(), {non_neg_integer(), non_neg_integer()}) ->
  state().
location_started(State, Loc) ->
  scope_put(loc_started, Loc, State).

-spec consume_char(state()) -> state().
consume_char(#{src := <<"\n"/utf8, Src/binary>>, loc := {Line, _}} = State) ->
  State#{src => Src, loc => {Line + 1, 1}};
consume_char(#{src := <<_/utf8, Src/binary>>, loc := {Line, Col}} = State) ->
  State#{src => Src, loc => {Line, Col + 1}}.

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
  Type = clj_utils:char_type(X, Rest),
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
      clj_utils:throw( [ <<"Started reading at (">>
                       , Line, <<":">>, Col
                       , <<") but found EOF while expecting '">>
                       , <<Delim/utf8>>
                       , <<"'">>
                       ]
                     , location(State)
                     )
  end;
read_until(Delim, _ReadFun, #{src := <<Delim/utf8, _/binary>>} = State) ->
  #{ forms         := Forms
   , pending_forms := PendingForms
   } = State,
  State1 = State#{ forms := lists:reverse(PendingForms) ++ Forms
                 , pending_forms => []
                 },
  consume_char(scope_put(read_delim, false, State1));
read_until(Delim, ReadFun, #{src := <<X/utf8, _/binary>>} = State) ->
  case clj_utils:char_type(X) of
    whitespace ->
      read_until(Delim, ReadFun, consume_char(State));
    _ ->
      State1 = scope_put(read_delim, true, State),
      read_until(Delim, ReadFun, ReadFun(State1))
  end.

-spec is_macro_terminating(char()) -> boolean().
is_macro_terminating(Char) ->
  lists:member(Char,
               [$", $;, $@, $^, $`, $~, $(,
                $), $[, $], ${, $}, $\\ ]).

-spec is_whitespace(char()) -> boolean().
is_whitespace(Char) ->
  clj_utils:char_type(Char, <<>>) =:= whitespace.

-spec skip_line(state()) -> state().
skip_line(State) ->
  NotNewline = fun(C) -> C =/= $\n andalso C =/= $\r end,
  {_, State1} = consume(State, NotNewline),
  State1.

-spec wrapped_read('clojerl.Symbol':type(), state()) -> state().
wrapped_read(Symbol, State) ->
  {Form, NewState} = read_pop_one(State),
  List = clj_core:list([Symbol, Form]),
  push_form(List, NewState).

-spec read_pop_one(state()) -> {any(), state()}.
read_pop_one(State0) ->
  State = read_one(State0),
  #{forms := [Form | Forms]} = State,
  {Form, State#{forms => Forms}}.

-spec push_form(any(), state()) -> state().
push_form(Form, #{forms := Forms} = State) ->
  State#{forms => [Form | Forms]}.

-spec push_pending_form(any(), state()) -> state().
push_pending_form(Form, #{pending_forms := Forms} = State) ->
  State#{pending_forms => [Form | Forms]}.

-spec scope_get(atom(), state()) -> state().
scope_get(Name, #{bindings := Bindings} = _State) ->
  clj_scope:get(Bindings, Name).

-spec scope_put(atom(), any(), state()) -> state().
scope_put(Name, Value, #{bindings := Bindings} = State) ->
  State#{bindings => clj_scope:put(Bindings, Name, Value)}.

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

-spec peek_src(state()) -> integer().
peek_src(#{src := <<First/utf8, _/binary>>}) ->
  First;
peek_src(#{src := <<>>, opts := #{?OPT_IO_READER := Reader}}) ->
  case 'erlang.io.IReader':read(Reader) of
    eof -> eof;
    Ch  ->
      'erlang.io.IReader':unread(Reader, Ch),
      Ch
  end;
peek_src(_State) ->
  eof.

-spec check_reader(state()) -> {ok, state()} | eof.
check_reader(#{src := <<>>, opts := #{?OPT_IO_READER := Reader}} = State)
  when Reader =/= undefined ->
  case 'erlang.io.IReader':read(Reader) of
    eof -> eof;
    Ch ->
      {ok, State#{src := Ch}}
  end;
check_reader(#{src := <<>>}) ->
  eof.
