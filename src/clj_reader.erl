-module(clj_reader).

-export([ read_fold/3
        , read_fold/4
        , read/1, read/2, read/3
        , read_all/1, read_all/2, read_all/3
        , location_meta/1
        ]).

-type location() :: {non_neg_integer(), non_neg_integer()}.

-type opts() :: #{ read_cond    => allow | preserve
                 , features     => 'clojerl.Set':type()
                 , data_readers => #{binary() => function()}
                 , file         => file:filename_all()
                 }.

-export_type([location/0, opts/0]).

-type state() :: #{ src           => binary()
                  , opts          => opts()
                  , forms         => [any()]
                  , pending_forms => [any()]
                  , env           => clj_env:env()
                  , loc           => location()
                  , bindings      => clj_scope:scope()
                  }.

-spec new_state(binary(), any(), opts(), boolean()) -> state().
new_state(Src, Env, Opts, ReadAll) ->
  #{ src           => Src
   , opts          => Opts
   , forms         => []
   , pending_forms => []
   , env           => Env
   , all           => ReadAll
   , loc           => {1, 1}
   , bindings      => clj_scope:new()
   }.

-type read_fold_fun() :: fun((any(), clj_env:env()) -> clj_env:env()).

-spec read_fold(read_fold_fun(), binary(), opts()) -> clj_env:env().
read_fold(Fun, Src, Opts) ->
  read_fold(Fun, Src, Opts, clj_env:default()).

-spec read_fold(read_fold_fun(), binary(), opts(), clj_env:env()) ->
  clj_env:env().
read_fold(Fun, Src, Opts, Env) ->
  State = new_state(Src, Env, Opts, false),
  read_fold_loop(Fun, State).

-spec read_fold_loop(read_fold_fun(), state()) -> clj_env:env().
read_fold_loop(Fun, State) ->
  case dispatch(State) of
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
      #{ loc  => clj_core:get(Meta, loc, undefined)
       , file => clj_core:get(Meta, file, undefined)
       };
    false -> undefined
  end.

-spec read(binary()) -> {any(), binary()} | eof.
read(Src) ->
  read(Src, #{}).

-spec read(binary(), opts()) -> {any(), binary()} | eof.
read(Src, Opts) ->
  read(Src, Opts, clj_env:default()).

%% @doc Reads the next form from the input. Returns the form
%%      or throws if there is no form to read.
-spec read(binary(), opts(), clj_env:env()) -> any().
read(Src, Opts, Env) ->
  State = new_state(Src, Env, Opts, false),
  ensure_read(dispatch(State)).

%% @doc Makes sure a single form is read unless we reach
%%      the enf of file.
%% @private
-spec ensure_read(state()) -> any().
ensure_read(#{src := <<>>, forms := []} = State) ->
  clj_utils:throw(<<"EOF">>, location(State));
ensure_read(#{forms := [Form]}) ->
  Form;
ensure_read(State) ->
  ensure_read(dispatch(State)).

%% @doc Read all forms.
-spec read_all(state()) -> [any()].
read_all(Src) ->
  read_all(Src, #{}).

-spec read_all(state(), opts()) -> [any()].
read_all(Src, Opts) ->
  read_all(Src, Opts, clj_env:default()).

-spec read_all(state(), opts(), clj_env:env()) -> [any()].
read_all(Src, Opts, Env) ->
  State = new_state(Src, Env, Opts, true),
  #{forms := Forms} = dispatch(State),
  lists:reverse(Forms).

-spec dispatch(state()) -> state().
dispatch(#{src := <<>>} = State) ->
  State;
dispatch(#{all := true} = State) ->
  dispatch(read_one(State, false));
dispatch(State) ->
  read_one(State, false).

-spec read_one(state()) -> state().
read_one(State) ->
  read_one(State, true).

-spec read_one(state(), boolean()) -> state().
read_one(#{pending_forms := [Form | PendingForms]} = State, _ThrowEof) ->
  push_form(Form, State#{pending_forms => PendingForms});
read_one(#{src := <<>>} = State, true = _ThrowEof) ->
  %% If we got here it's because we were expecting something
  %% and it wasn't there.
  clj_utils:throw(<<"EOF">>, location(State));
read_one(#{src := <<>>} = State, false = _ThrowEof) ->
  State;
read_one(#{src := <<First/utf8, Rest/binary>>} = State, ThrowEof) ->
  case clj_utils:char_type(First, Rest) of
    whitespace      -> read_one(consume_char(State), ThrowEof);
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
read_string(#{ src     := <<"\"", _/binary>>
             , current := String
             } = State0
           ) ->
  State = consume_char(maps:remove(current, State0)),
  push_form(String, State);
read_string(#{ src := <<"\\", _/binary>>
             , current := String
             } = State0
           ) ->
  {EscapedChar, State00} = escape_char(consume_char(State0)),
  State = State00#{current => <<String/binary, EscapedChar/binary>>},

  read_string(State);
read_string(#{src := <<Char/utf8, _/binary>>,
              current := String} = State0) ->
  State = State0#{current => <<String/binary, Char/utf8>>},
  read_string(consume_char(State));
read_string(#{src := <<"\"", _/binary>>} = State) ->
  State1 = consume_char(State),
  read_string(State1#{current => <<>>});
read_string(#{src := <<>>} = State) ->
  clj_utils:throw(<<"EOF while reading string">>, location(State)).

-spec escape_char(state()) -> {binary(), state()}.
escape_char(State = #{src := <<Char/utf8, Rest/binary>>}) ->
  CharType = clj_utils:char_type(Char, Rest),
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
      {CodePoint, State1} = unicode_char(consume_char(State), 16, 4, true),
      {unicode:characters_to_binary([CodePoint]), State1};
    _ when CharType == number ->
      %% Octal unicode
      case unicode_char(State, 8, 3, false) of
        {CodePoint, State1} when CodePoint > 8#337 ->
          clj_utils:throw( <<"Octal escape sequence must be in range [0, 377]">>
                         , location(State1)
                         );
        {CodePoint, State1} ->
          {unicode:characters_to_binary([CodePoint]), State1}
      end;
    _ ->
      clj_utils:throw( <<"Unsupported escape character: \\", Char>>
                     , location(State)
                     )
  end.

-spec unicode_char(state(), integer(), integer(), Exact :: boolean()) ->
                      {binary(), state()}.
unicode_char(State, Base, Length, IsExact) ->
  {Number, State1} = consume(State, [number, symbol]),
  Size = case IsExact of
           true -> size(Number);
           false -> Length
         end,
  case Size of
    Length ->
      try
        EscapedChar = binary_to_integer(Number, Base),
        {EscapedChar, State1}
      catch
        _:badarg ->
          BaseBin = integer_to_binary(Base),
          clj_utils:throw( <<"Number '", Number/binary,
                             "' is not in base ", BaseBin/binary>>
                          , location(State)
                          )
      end;
    NumLength ->
      LengthBin = integer_to_binary(Length),
      NumLengthBin = integer_to_binary(NumLength),
      clj_utils:throw(<<"Invalid character length: ", NumLengthBin/binary,
                        ", should be ", LengthBin/binary>>
                     , location(State)
                     )
  end.

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
              {Namespace, Name} ->
                clj_core:keyword(Namespace, Name);
              undefined ->
                clj_utils:throw( <<"Invalid token: :", Token/binary>>
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
               clj_utils:throw(<<"Invalid symbol ", Token/binary>>
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
  {SugaredMeta, State1} = pop_form(read_one(State#{src => Src})),
  Meta = clj_utils:desugar_meta(SugaredMeta),

  {Form, State2} = pop_form(read_one(State1)),
  NewForm = clj_core:with_meta(Form, Meta),

  push_form(NewForm, State2).

%%------------------------------------------------------------------------------
%% Syntax quote
%%------------------------------------------------------------------------------

-spec read_syntax_quote(state()) -> state().
read_syntax_quote(#{src := <<"`"/utf8, _/binary>>, env := Env} = State) ->
  {Form, NewState} = pop_form(read_one(consume_char(State))),

  try
    %% TODO: using process dictionary here might be a code smell
    erlang:put(gensym_env, #{}),
    {QuotedForm, Env1}      = syntax_quote(Form, Env),
    {NewFormWithMeta, Env2} = add_meta(Form, Env1, QuotedForm),

    push_form(NewFormWithMeta, NewState#{env => Env2})
  catch _:Reason ->
      erlang:raise(throw, Reason, erlang:get_stacktrace()),
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
  IsGenSym = clj_utils:ends_with(NameStr, <<"#">>),
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
    undefined -> Symbol;
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
read_unmatched_delim(State) ->
  clj_utils:throw(unmatched_delim, location(State)).

%%------------------------------------------------------------------------------
%% Character
%%------------------------------------------------------------------------------

-spec read_char(state()) -> state().
read_char(#{src := <<"\\"/utf8, _/binary>>} = State) ->
  {Token, State1} = read_token(consume_char(State)),
  Char =
    case Token of
      <<>> -> clj_utils:throw(<<"EOF">>, location(State));
      Ch when size(Ch) == 1 -> Ch;
      <<"newline">> -> $\n;
      <<"space">> -> $ ;
      <<"tab">> -> $\t;
      <<"backspace">> -> $\b;
      <<"formfeed">> -> $\f;
      <<"return">> -> $\r;
      <<"u", RestToken/binary>> ->
        {Ch, _} = unicode_char(State1#{src => RestToken}, 16, 4, true),
        Ch;
      <<"o", RestToken/binary>> ->
        read_octal_char(State1#{src => RestToken});
      Ch -> clj_utils:throw( <<"Unsupported character: \\", Ch/binary>>
                           , location(State)
                           )
    end,

  CharBin = unicode:characters_to_binary([Char]),
  push_form(CharBin, State1).

-spec read_octal_char(state()) -> char().
read_octal_char(#{src := RestToken} = State) when size(RestToken) > 3 ->
  Size    = size(RestToken),
  SizeBin = integer_to_binary(Size),
  clj_utils:throw( <<"Invalid octal escape sequence length: ", SizeBin/binary>>
                 , location(State)
                 );
read_octal_char(#{src := RestToken} = State) ->
  Size = size(RestToken),
  case unicode_char(State, 8, Size, true) of
    {Ch, _} when Ch > 8#377 ->
      clj_utils:throw(<<"Octal escape sequence must be in range [0, 377]">>
                     , location(State)
                     );
    {Ch, _} -> Ch
  end.

%%------------------------------------------------------------------------------
%% Argument
%%------------------------------------------------------------------------------

-spec read_arg(state()) -> state().
read_arg(#{src := <<"%"/utf8, Src/binary>>} = State) ->
  case erlang:get(arg_env) of
    undefined ->
      read_symbol(State);
    ArgEnv ->
      case arg_type(Src) of
        register_arg_1 ->
          ArgSym = register_arg(1, ArgEnv),
          push_form(ArgSym, consume_char(State));
        register_arg_multi ->
          ArgSym = register_arg(-1, ArgEnv),
          push_form(ArgSym, consume_chars(2, State));
        register_arg_n ->
          {N, NewState} = pop_form(read_one(consume_char(State))),
          clj_utils:throw_when( not is_integer(N)
                              , <<"Arg literal must be %, %& or %integer">>
                              , location(State)
                              ),
          ArgSym = register_arg(N, ArgEnv),
          push_form(ArgSym, NewState)
      end
  end.

-spec arg_type(binary()) -> atom().
arg_type(<<>>) ->
  register_arg_1;
arg_type(Str) ->
  Char = binary:first(Str),
  IsWhitespace = clj_utils:char_type(Char) == whitespace,
  IsMacroTerminating = is_macro_terminating(Char),
  if
    IsWhitespace orelse IsMacroTerminating -> register_arg_1;
    Char == $& -> register_arg_multi;
    true -> register_arg_n
  end.

-spec register_arg(integer(), map()) -> 'clojerl.Symbol':type().
register_arg(N, ArgEnv) ->
  case maps:get(N, ArgEnv, undefined) of
    undefined ->
      ArgSymbol = gen_arg_sym(N),
      NewArgEnv = maps:put(N, ArgSymbol, ArgEnv),
      put(arg_env, NewArgEnv),
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
  {Form, NewState} = pop_form(read_one(State)),
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
  {Form, NewState} = pop_form(read_one(State)),
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
read_regex(#{src := <<>>} = State) ->
  clj_utils:throw(<<"EOF">>, location(State));
read_regex(#{src := <<"\\"/utf8, Ch/utf8, _/binary>>} = State) ->
  Current = maps:get(current, State, <<>>),
  NewState = State#{current => <<Current/binary, "\\", Ch/utf8>>},
  read_regex(consume_chars(2, NewState));
read_regex(#{src := <<"\""/utf8, _/binary>>} = State) ->
  Current = maps:get(current, State, <<>>),
  {ok, Regex} = re:compile(Current),
  push_form(Regex, consume_char(State));
read_regex(#{src := <<Ch/utf8, _/binary>>} = State) ->
  Current = maps:get(current, State, <<>>),
  NewState = State#{current => <<Current/binary, Ch/utf8>>},
  read_regex(consume_char(NewState)).

%%------------------------------------------------------------------------------
%% #_ discard
%%------------------------------------------------------------------------------

-spec read_discard(state()) -> state().
read_discard(State) ->
  {_, NewState} = pop_form(read_one(State)),
  %% There could be no next forms so don't throw if there isn't
  NewState.

%%------------------------------------------------------------------------------
%% #? cond
%%------------------------------------------------------------------------------

-spec read_cond(state()) -> state().
read_cond(#{src := <<>>} = State) ->
  clj_utils:throw(<<"EOF while reading character">>, location(State));
read_cond(#{src := Src, opts := Opts} = State) ->
  ReadCondOpt = maps:get(read_cond, Opts, undefined),
  case lists:member(ReadCondOpt, [allow, preserve]) of
    false -> clj_utils:throw( <<"Conditional read not allowed">>
                             , location(State)
                             );
    true -> ok
  end,

  ReadDelim = clj_core:boolean(scope_get(read_delim, State)),
  IsSplicing = binary:first(Src) == $@,
  State1 =
    case IsSplicing of
      true when not ReadDelim ->
        clj_utils:throw(<<"cond-splice not in list">>, location(State));
      true ->
        consume_char(State);
      false ->
        State
    end,

  {ListForm, NewState} = pop_form(read_one(State1)),

  case clj_core:'list?'(ListForm) of
    false -> clj_utils:throw( <<"read-cond body must be a list">>
                            , location(State1)
                            );
    true ->
      OldSupressRead = clj_core:boolean(erlang:get(supress_read)),
      SupressRead = OldSupressRead orelse ReadCondOpt == preserve,
      erlang:put(supress_read, SupressRead),
      ReturnState =
        case SupressRead of
          true ->
            ReaderCondForm = reader_conditional(ListForm, IsSplicing),
            push_form(ReaderCondForm, NewState);
          false ->
            read_cond_delimited(ListForm, IsSplicing, NewState)
        end,
      erlang:put(supress_read, OldSupressRead),
      ReturnState
  end.

reader_conditional(List, IsSplicing) ->
  'clojerl.reader.ReaderConditional':new(List, IsSplicing).

read_cond_delimited(List, IsSplicing, #{opts := Opts} = State) ->
  Features = maps:get(features, Opts, clj_core:hash_set([])),
  Forms = clj_core:seq(List),
  case match_feature(Forms, Features, State) of
    nomatch ->
      read_one(State);
    Form when IsSplicing ->
      PendingFormsFun = fun push_pending_form/2,
      case clj_core:'sequential?'(Form) of
        false ->
          clj_utils:throw( <<"Spliced form list in read-cond-splicing must "
                             "extend clojerl.ISequential">>
                         , location(State)
                         );
        true ->
          Seq = clj_core:seq2(Form),
          Items = clj_core:seq2(Seq),
          lists:foldl(PendingFormsFun, State, lists:reverse(Items))
      end;
    Form ->
      push_form(Form, State)
  end.

match_feature([], _Features, _State) ->
  nomatch;
match_feature([Feature, Form | Rest], Features, State) ->
  case clj_core:'contains?'(Features, Feature) of
    true -> Form;
    false -> match_feature(Rest, Features, State)
  end;
match_feature(_, _, State) ->
  clj_utils:throw( <<"read-cond requires an even number of forms">>
                 , location(State)
                 ).

%%------------------------------------------------------------------------------
%% # reader tag
%%------------------------------------------------------------------------------

-spec read_tagged(state()) -> state().
read_tagged(#{opts := Opts} = State) ->
  {Symbol, State1} = pop_form(read_one(State)),

  clj_utils:throw_when( not clj_core:'symbol?'(Symbol)
                      , <<"Reader tag must be a symbol">>
                      , location(State)
                      ),

  DataReaders    = clj_core:get(Opts, 'data_readers'),
  AllDataReaders = clj_core:merge([default_data_readers(), DataReaders]),
  SymbolName     = clj_core:str(Symbol),
  ReadFun        = clj_core:get(AllDataReaders, SymbolName, undefined),

  clj_utils:throw_when( ReadFun == undefined
                      , [<<"No reader function for tag ">>, Symbol]
                      , location(State)
                      ),

  {Form, State2} = pop_form(read_one(State1)),
  ReadForm       = clj_core:invoke(ReadFun, [Form]),

  push_form(ReadForm, State2).

-spec default_data_readers() -> map().
default_data_readers() ->
  #{ <<"inst">> => fun read_instant_date/1
   , <<"uuid">> => fun default_uuid_reader/1
   }.

-spec read_instant_date(binary()) -> any().
read_instant_date(Date) ->
  Date.

-spec default_uuid_reader(binary()) -> any().
default_uuid_reader(UUID) ->
  UUID.

%%------------------------------------------------------------------------------
%% Utility functions
%%------------------------------------------------------------------------------

-spec location(state()) -> state().
location(#{opts := Opts} = State) ->
  #{ loc  => maps:get(loc, State, undefined)
   , file => maps:get(file, Opts, undefined)
   }.

-spec location_started(state(), location()) -> state().
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
  do_consume(State, <<>>, TypesOrPred).

do_consume(State = #{src := <<>>}, Acc, _) ->
  {Acc, State};
do_consume( State = #{src := <<X/utf8, _/binary>>}
          , Acc
          , Pred
          ) when is_function(Pred) ->
  case Pred(X) of
    true  ->
      State1 = consume_char(State),
      do_consume(State1, <<Acc/binary, X/utf8>>, Pred);
    false -> {Acc, State}
  end;
do_consume( State = #{src := <<X/utf8, Rest/binary>>}
          , Acc
          , Types
          ) ->
  Type = clj_utils:char_type(X, Rest),
  case lists:member(Type, Types) of
    true ->
      State1 = consume_char(State),
      do_consume(State1, <<Acc/binary, X/utf8>>, Types);
    false ->
      {Acc, State}
  end.

-spec read_token(state()) -> {binary(), state()}.
read_token(State) ->
  Fun = fun(Char) ->
            (clj_utils:char_type(Char, <<>>) =/= whitespace)
              andalso (not is_macro_terminating(Char))
        end,
  consume(State, Fun).

-spec read_until(char(), state()) -> state().
read_until(Delim, #{src := <<>>} = State) ->
  {Line, Col} = scope_get(loc_started, State),
  LineBin = integer_to_binary(Line),
  ColBin = integer_to_binary(Col),
  clj_utils:throw( [ <<"Started reading at (">>
                   , LineBin, <<":">>, ColBin
                   , <<") but found EOF while expecting '", Delim/utf8, "'">>
                   ]
                 , location(State)
                 );
read_until(Delim, #{src := <<Delim/utf8, _/binary>>} = State) ->
  consume_char(scope_put(read_delim, false, State));
read_until(Delim, #{src := <<X/utf8, _/binary>>} = State) ->
  case clj_utils:char_type(X) of
    whitespace ->
      read_until(Delim, consume_char(State));
    _ ->
      State1 = scope_put(read_delim, true, State),
      read_until(Delim, read_one(State1))
  end.

-spec is_macro_terminating(char()) -> boolean().
is_macro_terminating(Char) ->
  lists:member(Char,
               [$", $;, $@, $^, $`, $~, $(,
                $), $[, $], ${, $}, $\\ ]).

-spec skip_line(state()) -> state().
skip_line(State) ->
  NotNewline = fun(C) -> C =/= $\n andalso C =/= $\r end,
  {_, State1} = consume(State, NotNewline),
  State1.

-spec wrapped_read('clojerl.Symbol':type(), state()) -> state().
wrapped_read(Symbol, State) ->
  {Form, NewState} = pop_form(read_one(State)),
  List = clj_core:list([Symbol, Form]),
  push_form(List, NewState).

-spec pop_form(state()) -> {any(), state()}.
pop_form(#{forms := [Form | Forms]} = State) ->
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
  Loc  = maps:get(loc, State),
  Opts = maps:get(opts, State),

  case maps:is_key(file, Opts) of
    true  -> #{loc => Loc, file => maps:get(file, Opts)};
    false -> #{loc => Loc}
  end.
