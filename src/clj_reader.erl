-module(clj_reader).

-export([
         read_fold/3,
         read_fold/4,
         read/1, read/2, read/3,
         read_all/1, read_all/2, read_all/3
        ]).

-type opts() :: #{read_cond => allow | preserve,
                  features => 'clojerl.Set':type()}.

-type state() :: #{ src           => binary()
                  , opts          => opts()
                  , forms         => [any()]
                  , pending_forms => [any()]
                  , env           => clj_env:env()
                  , location      => {non_neg_integer(), non_neg_integer()}
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
ensure_read(#{src := <<>>, forms := []}) ->
  throw(<<"EOF">>);
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
read_one(#{src := <<>>}, true = _ThrowEof) ->
  %% If we got here it's because we were expecting something
  %% and it wasn't there.
  throw(<<"EOF">>);
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
read_string(#{src := <<>>}) ->
  throw(<<"EOF while reading string">>).

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
        {CodePoint, _} when CodePoint > 8#337 ->
          throw(<<"Octal escape sequence must be in range [0, 377]">>);
        {CodePoint, State1} ->
          {unicode:characters_to_binary([CodePoint]), State1}
      end;
    _ ->
      throw(<<"Unsupported escape character: \\", Char>>)
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
          throw(<<"Number '", Number/binary,
                  "' is not in base ", BaseBin/binary >>)
      end;
    NumLength ->
      LengthBin = integer_to_binary(Length),
      NumLengthBin = integer_to_binary(NumLength),
      throw(<<"Invalid character length: ", NumLengthBin/binary,
              ", should be ", LengthBin/binary>>)
  end.

%%------------------------------------------------------------------------------
%% Keyword
%%------------------------------------------------------------------------------

read_keyword(#{ src := <<":", _/binary>>
              , env := Env
              } = State0) ->
  {Token, State} = read_token(consume_char(State0)),
  Keyword = case clj_utils:parse_symbol(Token) of
              {undefined, <<":", Name/binary>>} ->
                NsSym = clj_env:current_ns(Env),
                Namespace = clj_core:name(NsSym),
                clj_core:keyword(Namespace, Name);
              {undefined, Name} ->
                clj_core:keyword(Name);
              {Namespace, Name} ->
                clj_core:keyword(Namespace, Name);
              undefined ->
                throw(<<"Invalid token: :", Token/binary>>)
            end,
  push_form(Keyword, State).

%%------------------------------------------------------------------------------
%% Symbol
%%------------------------------------------------------------------------------

read_symbol(State) ->
  Loc  = maps:get(loc, State),
  Meta = #{loc => Loc},

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
               throw(<<"Invalid symbol ", Token/binary>>)
           end,

  push_form(Symbol, State1).

%%------------------------------------------------------------------------------
%% Comment
%%------------------------------------------------------------------------------

read_comment(State) -> skip_line(State).

%%------------------------------------------------------------------------------
%% Quote
%%------------------------------------------------------------------------------

read_quote(#{src := <<"'"/utf8, _/binary>>} = State) ->
  Quote = clj_core:symbol(<<"quote">>),
  wrapped_read(Quote, consume_char(State)).

%%------------------------------------------------------------------------------
%% Deref
%%------------------------------------------------------------------------------

read_deref(#{src := <<"@"/utf8, _/binary>>} = State) ->
  Deref = clj_core:symbol(<<"clojure.core">>, <<"deref">>),
  wrapped_read(Deref, consume_char(State)).

%%------------------------------------------------------------------------------
%% Meta
%%------------------------------------------------------------------------------

read_meta(#{src := <<"^"/utf8, Src/binary>>} = State) ->
  {SugaredMeta, State1} = pop_form(read_one(State#{src => Src})),
  Meta = clj_utils:desugar_meta(SugaredMeta),

  {Form, State2} = pop_form(read_one(State1)),
  NewForm = clj_core:with_meta(Form, Meta),

  push_form(NewForm, State2).

%%------------------------------------------------------------------------------
%% Syntax quote
%%------------------------------------------------------------------------------

read_syntax_quote(#{src := <<"`"/utf8, _/binary>>, env := Env} = State) ->
  {Form, NewState} = pop_form(read_one(consume_char(State))),
  %% TODO: using process dictionary here might be a code smell
  erlang:put(gensym_env, #{}),
  NewFormWithMeta = add_meta(Form, Env, syntax_quote(Form, Env)),
  erlang:erase(gensym_env),
  push_form(NewFormWithMeta, NewState).

syntax_quote(Form, Env) ->
  IsSpecial    = clj_analyzer:is_special(Form),
  IsSymbol     = clj_core:'symbol?'(Form),
  IsUnquote    = is_unquote(Form),
  IsUnquoteSpl = is_unquote_splicing(Form),
  IsColl       = clj_core:'coll?'(Form),
  IsLiteral    = is_literal(Form),

  QuoteSymbol = clj_core:symbol(<<"quote">>),
  if
    IsSpecial    -> clj_core:list([QuoteSymbol, Form]);
    IsSymbol     -> syntax_quote_symbol(Form, Env);
    IsUnquote    -> clj_core:second(Form);
    IsUnquoteSpl -> throw(<<"unquote-splice not in list">>);
    IsColl ->
      IsMap = clj_core:'map?'(Form),
      IsVector = clj_core:'vector?'(Form),
      IsSet = clj_core:'set?'(Form),
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
        true ->
          syntax_quote_coll(Form, undefined, Env)
      end;
    IsLiteral -> Form;
    true      -> clj_core:list([QuoteSymbol, Form])
  end.

flatten_map(Map) ->
  MapSeq = clj_core:seq(Map),
  flatten_map(MapSeq, clj_core:vector([])).

flatten_map(undefined, Vector) ->
  clj_core:seq(Vector);
flatten_map(MapSeq, Vector) ->
  First = clj_core:first(MapSeq),
  Vector1 = clj_core:conj(Vector, clj_core:first(First)),
  Vector2 = clj_core:conj(Vector1, clj_core:second(First)),
  flatten_map(clj_core:next(MapSeq), Vector2).

syntax_quote_symbol(Symbol, Env) ->
  NamespaceStr = clj_core:namespace(Symbol),
  NameStr = clj_core:name(Symbol),
  IsGenSym = clj_utils:ends_with(NameStr, <<"#">>),
  case {NamespaceStr, IsGenSym} of
    {undefined, true} ->
      register_gensym(Symbol, Env);
    _ ->
      resolve_symbol(Symbol, Env)
  end.

register_gensym(Symbol, _Env) ->
  GensymEnv = case erlang:get(gensym_env) of
                undefined ->
                  throw(<<"Gensym literal not in syntax-quote">>);
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

resolve_symbol(Symbol, Env) ->
  CurrentNsSym = clj_env:current_ns(Env),
  case clj_core:namespace(Symbol) of
    undefined ->
      case clj_env:find_var(Env, Symbol) of
        undefined ->
          CurrentNsName = clj_core:name(CurrentNsSym),
          NameStr = clj_core:name(Symbol),
          clj_core:symbol(CurrentNsName, NameStr);
        Var ->
          VarNsSym = 'clojerl.Var':namespace(Var),
          VarNameSym = 'clojerl.Var':name(Var),
          clj_core:symbol(clj_core:name(VarNsSym), clj_core:name(VarNameSym))
      end;
    NamespaceStr ->
      case clj_env:resolve_ns(Env, clj_core:symbol(NamespaceStr)) of
        undefined ->
          Symbol;
        Ns ->
          NsNameSym = clj_namespace:name(Ns),
          NsNameStr = clj_core:name(NsNameSym),
          NameStr = clj_core:name(Symbol),
          clj_core:symbol(NsNameStr, NameStr)
      end
  end.

syntax_quote_coll(List, undefined, Env) ->
  syntax_quote_coll(List, Env);
syntax_quote_coll(List, FunSymbol, Env) ->
  ExpandedList = syntax_quote_coll(List, Env),
  ApplySymbol = clj_core:symbol(<<"clojure.core">>, <<"apply">>),
  clj_core:list([ApplySymbol, FunSymbol, ExpandedList]).

syntax_quote_coll(List, Env) ->
  case clj_core:'empty?'(List) of
    true ->
      ListSymbol = clj_core:symbol(<<"clojure.core">>, <<"list">>),
      clj_core:list([ListSymbol]);
    false ->
      ExpandedItems = lists:reverse(expand_list(List, Env, [])),
      ConcatSymbol = clj_core:symbol(<<"clojure.core">>, <<"concat">>),
      clj_core:list([ConcatSymbol | ExpandedItems])
  end.

expand_list(undefined, _Env, Result) ->
  Result;
expand_list(List, Env, Result) ->
  Item = clj_core:first(List),
  ListSymbol = clj_core:symbol(<<"clojure.core">>, <<"list">>),
  NewItem = case {is_unquote(Item), is_unquote_splicing(Item)} of
              {true, _} -> clj_core:list([ListSymbol, clj_core:second(Item)]);
              {_, true} -> clj_core:second(Item);
              _ -> clj_core:list([ListSymbol, syntax_quote(Item, Env)])
            end,
  expand_list(clj_core:next(List), Env, [NewItem | Result]).

add_meta(Form, Env, Result) ->
  case clj_core:'meta?'(Form) of
    true ->
      WithMetaSym = clj_core:symbol(<<"clojure.core">>, <<"with-meta">>),
      Meta = syntax_quote(clj_core:meta(Form), Env),
      clj_core:list([WithMetaSym, Result, Meta]);
    _ ->
      Result
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

read_unquote(#{src := <<"~"/utf8, Src/binary>>} = State) ->
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

read_list(#{ src   := <<"("/utf8, _/binary>>
           , forms := Forms
           , loc   := Loc
           } = State0
         ) ->
  State  = consume_char(State0),
  State1 = read_until($), State#{forms => []}),
  #{forms := ReversedItems} = State1,

  Items = lists:reverse(ReversedItems),
  List = clj_core:with_meta(clj_core:list(Items), #{loc => Loc}),

  State1#{forms => [List | Forms]}.

%%------------------------------------------------------------------------------
%% Vector
%%------------------------------------------------------------------------------

read_vector(#{ src   := <<"["/utf8, _/binary>>
             , forms := Forms
             , loc   := Loc
             } = State0
           ) ->
  State  = consume_char(State0),
  State1 = read_until($], State#{forms => []}),
  #{forms := ReversedItems} = State1,

  Items = lists:reverse(ReversedItems),
  Vector = clj_core:with_meta(clj_core:vector(Items), #{loc => Loc}),

  State1#{forms => [Vector | Forms]}.

%%------------------------------------------------------------------------------
%% Map
%%------------------------------------------------------------------------------

read_map(#{ src   := <<"{"/utf8, _/binary>>
          , forms := Forms
          , loc   := Loc
          } = State0
        ) ->
  State  = consume_char(State0),
  State1 = read_until($}, State#{forms => []}),
  #{forms := ReversedItems} = State1,

  case length(ReversedItems) of
    X when X rem 2 == 0 ->
      Items = lists:reverse(ReversedItems),
      Map = clj_core:with_meta(clj_core:hash_map(Items), #{loc => Loc}),
      State1#{forms => [Map | Forms]};
    _ ->
      throw(<<"Map literal must contain an even number of forms">>)
  end.

%%------------------------------------------------------------------------------
%% Unmatched delimiter
%%------------------------------------------------------------------------------

read_unmatched_delim(_) -> throw(unmatched_delim).

%%------------------------------------------------------------------------------
%% Character
%%------------------------------------------------------------------------------

read_char(#{src := <<"\\"/utf8, _/binary>>} = State) ->
  {Token, State1} = read_token(consume_char(State)),
  Char =
    case Token of
      <<>> -> throw(<<"EOF">>);
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
      Ch -> throw(<<"Unsupported character: \\", Ch/binary>>)
    end,

  CharBin = unicode:characters_to_binary([Char]),
  push_form(CharBin, State1).

-spec read_octal_char(state()) -> char().
read_octal_char(#{src := RestToken}) when size(RestToken) > 3 ->
  Size    = size(RestToken),
  SizeBin = integer_to_binary(Size),
  throw(<<"Invalid octal escape sequence length: ", SizeBin/binary>>);
read_octal_char(#{src := RestToken} = State) ->
  Size = size(RestToken),
  case unicode_char(State, 8, Size, true) of
    {Ch, _} when Ch > 8#377 ->
      throw(<<"Octal escape sequence must be in range [0, 377]">>);
    {Ch, _} -> Ch
  end.

%%------------------------------------------------------------------------------
%% Argument
%%------------------------------------------------------------------------------

read_arg(#{src := <<"%"/utf8, Src/binary>>} = State) ->
  case erlang:get(arg_env) of
    undefined ->
      read_symbol(State);
    _ ->
      case arg_type(Src) of
        register_arg_1 ->
          ArgSym = register_arg(1),
          push_form(ArgSym, consume_char(State));
        register_arg_multi ->
          ArgSym = register_arg(-1),
          push_form(ArgSym, consume_chars(2, State));
        register_arg_n ->
          {N, NewState} = pop_form(read_one(consume_char(State))),
          case is_integer(N) of
            false -> throw(<<"Arg literal must be %, %& or %integer">>);
            true -> ok
          end,
          ArgSym = register_arg(N),
          push_form(ArgSym, NewState)
      end
  end.

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

register_arg(N) ->
  case erlang:get(arg_env) of
    undefined -> throw(<<"Arg literal not in #()">>);
    ArgEnv ->
      case maps:get(N, ArgEnv, undefined) of
        undefined ->
          ArgSymbol = gen_arg_sym(N),
          NewArgEnv = maps:put(N, ArgSymbol, ArgEnv),
          put(arg_env, NewArgEnv),
          ArgSymbol;
        ArgSymbol -> ArgSymbol
      end
  end.

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
    $: -> read_erl_fun(NewState);
    $< -> throw(<<"Unreadable form">>);
    X  -> throw({unsupported_reader, <<"#", X>>})
  end.

%%------------------------------------------------------------------------------
%% #' var
%%------------------------------------------------------------------------------

read_var(#{src := <<"'", _/binary>>} = State) ->
  VarSymbol = clj_core:symbol(<<"var">>),
  wrapped_read(VarSymbol, consume_char(State)).

%%------------------------------------------------------------------------------
%% #() fn
%%------------------------------------------------------------------------------

read_fn(#{loc := Loc} = State) ->
  case erlang:get(arg_env) of
    undefined -> ok;
    _         -> throw(<<"Nested #()s are not allowed">>)
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
  FnFormWithMeta = clj_core:with_meta(FnForm, #{loc => Loc}),

  push_form(FnFormWithMeta, NewState).

%%------------------------------------------------------------------------------
%% #= eval
%%------------------------------------------------------------------------------

read_eval(#{env := Env} = State) ->
  {Form, NewState} = pop_form(read_one(State)),
  {Value, Env1} = clj_compiler:eval(Form, #{}, Env),
  push_form(Value, NewState#{env => Env1}).

%%------------------------------------------------------------------------------
%% #{} set
%%------------------------------------------------------------------------------

read_set(#{forms := Forms, loc := Loc} = State) ->
  State1 = read_until($}, State#{forms => []}),
  #{forms := ReversedItems} = State1,

  Items       = lists:reverse(ReversedItems),
  Set         = clj_core:hash_set(Items),
  SetWithMeta = clj_core:with_meta(Set, #{loc => Loc}),

  State1#{forms => [SetWithMeta | Forms]}.

%%------------------------------------------------------------------------------
%% #[] tuple
%%------------------------------------------------------------------------------

read_tuple(#{forms := Forms} = State) ->
  State1 = read_until($], State#{forms => []}),
  #{forms := ReversedItems} = State1,

  Items = lists:reverse(ReversedItems),
  Tuple = erlang:list_to_tuple(Items),

  State1#{forms => [Tuple | Forms]}.

%%------------------------------------------------------------------------------
%% #"" regex
%%------------------------------------------------------------------------------

read_regex(#{src := <<>>}) ->
  throw(<<"EOF">>);
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

read_discard(State) ->
  {_, NewState} = pop_form(read_one(State)),
  %% There could be no next forms so don't throw if there isn't
  NewState.

%%------------------------------------------------------------------------------
%% #? cond
%%------------------------------------------------------------------------------

read_cond(#{src := <<>>}) ->
  throw(<<"EOF while reading character">>);
read_cond(#{src := Src, opts := Opts} = State) ->
  ReadCondOpt = maps:get(read_cond, Opts, undefined),
  case lists:member(ReadCondOpt, [allow, preserve]) of
    false -> throw(<<"Conditional read not allowed">>);
    true -> ok
  end,

  ReadDelim = clj_core:boolean(erlang:get(read_delim)),
  IsSplicing = binary:first(Src) == $@,
  State1 =
    case IsSplicing of
      true when not ReadDelim ->
        throw(<<"cond-splice not in list">>);
      true ->
        consume_char(State);
      false ->
        State
    end,

  {ListForm, NewState} = pop_form(read_one(State1)),

  case clj_core:'list?'(ListForm) of
    false -> throw(<<"read-cond body must be a list">>);
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
  case match_feature(Forms, Features) of
    nomatch ->
      read_one(State);
    Form when IsSplicing ->
      case clj_core:'sequential?'(Form) of
        false ->
          throw(<<"Spliced form list in read-cond-splicing must "
                  "extend clojerl.ISequential">>);
        true ->
          Seq = clj_core:seq2(Form),
          Items = clj_core:seq2(Seq),
          lists:foldl(fun push_pending_form/2, State, lists:reverse(Items))
      end;
    Form ->
      push_form(Form, State)
  end.

match_feature([], _Features) ->
  nomatch;
match_feature([Feature, Form | Rest], Features) ->
  case clj_core:'contains?'(Features, Feature) of
    true -> Form;
    false -> match_feature(Rest, Features)
  end;
match_feature(_, _) ->
  throw(<<"read-cond requires an even number of forms">>).

%%------------------------------------------------------------------------------
%% #: erlang function
%%------------------------------------------------------------------------------

read_erl_fun(State) ->
  {First, State1} = pop_form(read_one(State)),
  {Second, State2 = #{forms := Forms}} = pop_form(read_one(State1)),

  case valid_erl_fun(First, Second) of
    true ->
      Module = clj_core:namespace(First),
      Function = clj_core:name(First),
      ModuleAtom = binary_to_existing_atom(Module, utf8),
      FunctionAtom = binary_to_existing_atom(Function, utf8),

      Arity = clj_core:first(Second),
      Fun = fun ModuleAtom:FunctionAtom/Arity,
      push_form(Fun, State2);
    false ->
      throw(<<"Reader literal '#:' expects a fully-qualified symbol"
              " followed by a vector with one element.">>)
  end.

valid_erl_fun(First, Second) ->
  case {clj_core:'symbol?'(First), clj_core:'vector?'(Second)} of
    {true, true} ->
      Module = clj_core:namespace(First),
      Count  = clj_core:count(Second),
      Module =/= undefined andalso Count == 1;
    _ ->
      false
  end.

%%------------------------------------------------------------------------------
%% Utility functions
%%------------------------------------------------------------------------------

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
read_until(Delim, #{src := <<Delim/utf8, _/binary>>} = State) ->
  erlang:erase(read_delim),
  consume_char(State);
read_until(Delim, #{src := <<X/utf8, _/binary>>} = State) ->
  case clj_utils:char_type(X) of
    whitespace ->
      read_until(Delim, consume_char(State));
    _ ->
      erlang:put(read_delim, true),
      read_until(Delim, read_one(State))
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
