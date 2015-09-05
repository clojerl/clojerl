-module(clj_reader).

-export([
         read/1,
         read/2,
         read_all/1,
         read_all/2
        ]).

-type state() :: #{src => binary(),
                   forms => [any()],
                   env => map()}.

-spec read(binary()) -> any().
read(Src) ->
  read(Src, clj_env:default()).

-spec read(binary(), clj_env:env()) -> any().
read(Src, Env) ->
  State = #{src => Src,
            forms => [],
            env => Env},
  #{forms := Forms} = dispatch(State),
  case Forms of
    [] -> throw(<<"EOF">>);
    [H | _] -> H
  end.

-spec read_all(state()) -> [any()].
read_all(Src) ->
  read_all(Src, clj_env:default()).

-spec read_all(state(), clj_env:env()) -> [any()].
read_all(Src, Env) ->
  State = #{src => Src,
            forms => [],
            env => Env,
            all => true},
  #{forms := Forms} = dispatch(State),
  lists:reverse(Forms).

-spec dispatch(state()) -> state().
dispatch(#{src := <<>>} = State) ->
  State;
dispatch(State) ->
  next(read_one(State, false)).

-spec read_one(state()) -> state().
read_one(State) ->
  read_one(State, true).

-spec read_one(state(), ThrowEof :: boolean()) -> state().
read_one(#{src := <<>>}, true) ->
  %% If we got here it's because we were expecting something
  %% and it wasn't there.
  throw(<<"EOF">>);
read_one(#{src := <<>>} = State, false) ->
  State;
read_one(#{src := <<First, Rest/binary>>} = State, ThrowEof) ->
  case clj_utils:char_type(First, Rest) of
    whitespace -> read_one(State#{src => Rest}, ThrowEof);
    number -> read_number(State);
    string -> read_string(State);
    keyword -> read_keyword(State);
    comment -> read_comment(State);
    quote -> read_quote(State);
    deref -> read_deref(State);
    meta -> read_meta(State);
    syntax_quote -> read_syntax_quote(State);
    unquote -> read_unquote(State);
    list -> read_list(State);
    vector -> read_vector(State);
    map -> read_map(State);
    unmatched_delim -> read_unmatched_delim(State);
    char -> read_char(State);
    arg -> read_arg(State);
    dispatch -> read_dispatch(State);
    symbol -> read_symbol(State)
  end.

-spec next(state()) -> state().
next(#{all := true} = State) -> dispatch(State);
next(State) -> State.

%%------------------------------------------------------------------------------
%% Numbers
%%------------------------------------------------------------------------------

-spec read_number(state()) -> state().
read_number(#{forms := Forms, src := Src} = State) ->
  {Current, SrcRest} = consume(Src, [number, symbol]),
  Number = clj_utils:parse_number(Current),
  State#{forms => [Number | Forms],
         src   => SrcRest}.

%%------------------------------------------------------------------------------
%% String
%%------------------------------------------------------------------------------

-spec read_string(state()) -> state().
read_string(#{forms := Forms,
              src := <<"\"", SrcRest/binary>>,
              current := String} = State0) ->
  State = maps:remove(current, State0),
  State#{forms => [String | Forms],
         src => SrcRest};
read_string(#{src := <<"\\", SrcRest/binary>>,
              current := String} = State0) ->
  {EscapedChar, Rest} = escape_char(SrcRest),
  State = State0#{current => <<String/binary,
                               EscapedChar/binary>>,
                  src => Rest},
  read_string(State);
read_string(#{src := <<Char, SrcRest/binary>>,
              current := String} = State0) ->
  State = State0#{current => <<String/binary, Char>>,
                  src => SrcRest},
  read_string(State);
read_string(#{src := <<"\"", Rest/binary>>} = State) ->
  read_string(State#{src => Rest, current => <<>>});
read_string(#{src := <<>>}) ->
  throw(<<"EOF while reading string">>).

-spec escape_char(binary()) -> {binary(), binary()}.
escape_char(<<Char, Rest/binary>> = Src) ->
  CharType = clj_utils:char_type(Char, Rest),
  case Char of
    $t -> {<<"\t">>, Rest};
    $r -> {<<"\r">>, Rest};
    $n -> {<<"\n">>, Rest};
    $\\ -> {<<"\\">>, Rest};
    $" -> {<<"\"">>, Rest};
    $b -> {<<"\b">>, Rest};
    $f -> {<<"\f">>, Rest};
    $u ->
      %% Hexa unicode
      {CodePoint, NewRest} = unicode_char(Rest, 16, 4, true),
      {unicode:characters_to_binary([CodePoint]), NewRest};
    _ when CharType == number ->
      %% Octal unicode
      case unicode_char(Src, 8, 3, false) of
        {CodePoint, _} when CodePoint > 8#337 ->
          throw(<<"Octal escape sequence must be in range [0, 377]">>);
        {CodePoint, NewRest} ->
          {unicode:characters_to_binary([CodePoint]), NewRest}
      end;
    _ ->
      throw(<<"Unsupported escape character: \\", Char>>)
  end.

-spec unicode_char(binary(), integer(), integer(), Exact :: boolean()) ->
                      {binary(), binary()}.
unicode_char(Src, Base, Length, IsExact) ->
  {Number, Rest} = consume(Src, [number, symbol]),
  Size = case IsExact of
           true -> size(Number);
           false -> Length
         end,
  case Size of
    Length ->
      try
        EscapedChar = binary_to_integer(Number, Base),
        {EscapedChar, Rest}
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

read_keyword(#{forms := Forms,
               src := <<$:, Src/binary>>,
               env := Env} = State) ->
  {Token, RestSrc} = read_token(Src),
  Keyword = case clj_utils:parse_symbol(Token) of
              {undefined, <<$:, Name/binary>>} ->
                NsSym = clj_env:current_ns(Env),
                Namespace = clj_core:name(NsSym),
                'clojerl.Keyword':new(Namespace, Name);
              {undefined, Name} ->
                'clojerl.Keyword':new(Name);
              {Namespace, Name} ->
                'clojerl.Keyword':new(Namespace, Name);
              undefined ->
                throw(<<"Invalid token: :", Token/binary>>)
            end,
  State#{forms => [Keyword | Forms],
         src => RestSrc}.

%%------------------------------------------------------------------------------
%% Symbol
%%------------------------------------------------------------------------------

read_symbol(#{forms := Forms,
              src := Src} = State) ->
  {Token, RestSrc} = read_token(Src),
  Symbol = case clj_utils:parse_symbol(Token) of
             {undefined, Name} ->
               case Name of
                 <<"nil">> -> undefined;
                 <<"true">> -> true;
                 <<"false">> -> false;
                 _ -> 'clojerl.Symbol':new(Name)
               end;
             {Namespace, Name} ->
               'clojerl.Symbol':new(Namespace, Name);
             undefined ->
               throw(<<"Invalid symbol ", Token/binary>>)
           end,
  State#{forms => [Symbol | Forms],
         src => RestSrc}.

%%------------------------------------------------------------------------------
%% Comment
%%------------------------------------------------------------------------------

read_comment(#{src := Src} = State) ->
  State#{src => skip_line(Src)}.

%%------------------------------------------------------------------------------
%% Quote
%%------------------------------------------------------------------------------

read_quote(#{src := <<$', Src/binary>>} = State) ->
  Quote = 'clojerl.Symbol':new(<<"quote">>),
  wrapped_read(Quote, State#{src => Src}).

%%------------------------------------------------------------------------------
%% Deref
%%------------------------------------------------------------------------------

read_deref(#{src := <<$@, Src/binary>>} = State) ->
  Quote = 'clojerl.Symbol':new(<<"deref">>),
  wrapped_read(Quote, State#{src => Src}).

%%------------------------------------------------------------------------------
%% Meta
%%------------------------------------------------------------------------------

read_meta(#{src := <<$^, Src/binary>>} = State) ->
  {SugaredMeta, State1} = pop_form(read_one(State#{src => Src})),
  Meta = clj_utils:desugar_meta(SugaredMeta),

  {Expr, State2} = pop_form(read_one(State1)),
  NewExpr = 'clojerl.IMeta':with_meta(Expr, Meta),

  push_form(NewExpr, State2).

%%------------------------------------------------------------------------------
%% Syntax quote
%%------------------------------------------------------------------------------

read_syntax_quote(#{src := <<$`, Src/binary>>} = State) ->
  SyntaxQuote = 'clojerl.Symbol':new(<<"syntax-quote">>),
  wrapped_read(SyntaxQuote, State#{src => Src}).

%%------------------------------------------------------------------------------
%% Unquote
%%------------------------------------------------------------------------------

read_unquote(#{src := <<$~, Src/binary>>} = State) ->
  case Src of
    <<$@, RestSrc/binary>> ->
      UnquoteSplicing = 'clojerl.Symbol':new(<<"clojure.core">>,
                                             <<"unquote-splicing">>),
      wrapped_read(UnquoteSplicing, State#{src => RestSrc});
    _ ->
      UnquoteSplicing = 'clojerl.Symbol':new(<<"clojure.core">>,
                                             <<"unquote">>),
      wrapped_read(UnquoteSplicing, State#{src => Src})
  end.

%%------------------------------------------------------------------------------
%% List
%%------------------------------------------------------------------------------

read_list(#{src := <<$(, Src/binary>>,
            forms := Forms} = State) ->
  #{src := RestSrc,
    forms := ReversedItems} = read_until($), State#{src => Src, forms => []}),

  Items = lists:reverse(ReversedItems),
  List = 'clojerl.List':new(Items),

  State#{src => RestSrc, forms => [List | Forms]}.

%%------------------------------------------------------------------------------
%% Vector
%%------------------------------------------------------------------------------

read_vector(#{src := <<$[, Src/binary>>,
              forms := Forms} = State) ->
  #{src := RestSrc,
    forms := ReversedItems} =
    read_until($], State#{src => Src, forms => []}),

  Items = lists:reverse(ReversedItems),
  Vector = 'clojerl.Vector':new(Items),
  State#{src => RestSrc,
         forms => [Vector | Forms]}.

%%------------------------------------------------------------------------------
%% Map
%%------------------------------------------------------------------------------

read_map(#{src := <<${, Src/binary>>,
           forms := Forms} = State) ->
  #{src := RestSrc,
    forms := ReversedItems} =
    read_until($}, State#{src => Src, forms => []}),

  case length(ReversedItems) of
    X when X rem 2 == 0 ->
      Items = lists:reverse(ReversedItems),
      Map = 'clojerl.Map':new(Items),
      State#{src => RestSrc, forms => [Map | Forms]};
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

read_char(#{src := <<$\\, Src/binary>>,
            forms := Forms} = State) ->
  {Token, RestSrc} = read_token(Src),
  Char =
    case Token of
      <<>> -> throw(<<"EOF">>);
      Ch when size(Ch) == 1-> Ch;
      <<"newline">> -> $\n;
      <<"space">> -> $ ;
      <<"tab">> -> $\t;
      <<"backspace">> -> $\b;
      <<"formfeed">> -> $\f;
      <<"return">> -> $\r;
      <<$u, RestToken/binary>> ->
        {Ch, _} = unicode_char(RestToken, 16, 4, true),
        Ch;
      <<$o, RestToken/binary>> ->
        read_octal_char(RestToken);
      Ch -> throw(<<"Unsupported character: \\", Ch/binary>>)
    end,

  CharBin = unicode:characters_to_binary([Char]),
  State#{src => RestSrc, forms => [CharBin | Forms]}.

-spec read_octal_char(binary()) -> char().
read_octal_char(RestToken) when size(RestToken) > 3 ->
  Size = size(RestToken),
  SizeBin = integer_to_binary(Size),
  throw(<<"Invalid octal escape sequence length: ", SizeBin/binary>>);
read_octal_char(RestToken)  ->
  Size = size(RestToken),
  case unicode_char(RestToken, 8, Size, true) of
    {Ch, _} when Ch > 8#377 ->
      throw(<<"Octal escape sequence must be in range [0, 377]">>);
    {Ch, _} -> Ch
  end.

%%------------------------------------------------------------------------------
%% Argument
%%------------------------------------------------------------------------------

read_arg(_) -> throw(unimplemented).

%%------------------------------------------------------------------------------
%% Reader dispatch
%%------------------------------------------------------------------------------

read_dispatch(#{src := <<$#, Src/binary>>} = State) ->
  <<Ch, RestSrc/binary>> = Src,
  NewState = State#{src => RestSrc},
  case Ch of
    $^ -> read_meta(State#{src => Src}); %% deprecated
    $' ->
      VarSymbol = 'clojerl.Symbol':new(<<"var">>),
      wrapped_read(VarSymbol, NewState);
    $( -> read_fn(NewState);
    $= -> read_eval(NewState);
    ${ -> read_set(NewState);
    $< -> throw(unimplemented);
    $" -> read_regex(NewState);
    $! -> read_comment(NewState);
    $_ -> read_discard(NewState);
    $? -> read_cond(NewState);
    $: -> read_erl_fun(NewState);
    X -> throw({unsupported_reader, <<"#", X>>})
  end.

%%------------------------------------------------------------------------------
%% #() fn
%%------------------------------------------------------------------------------

read_fn(_State) -> throw(unimplemented).

%%------------------------------------------------------------------------------
%% #= eval
%%------------------------------------------------------------------------------

read_eval(_State) -> throw(unimplemented).

%%------------------------------------------------------------------------------
%% #{} set
%%------------------------------------------------------------------------------

read_set(#{src := Src,
           forms := Forms} = State) ->
  #{src := RestSrc,
    forms := ReversedItems} =
    read_until($}, State#{src => Src, forms => []}),

  Items = lists:reverse(ReversedItems),
  Set = 'clojerl.Set':new(Items),
  State#{src => RestSrc,
         forms => [Set | Forms]}.

%%------------------------------------------------------------------------------
%% #"" regex
%%------------------------------------------------------------------------------

read_regex(#{src := <<>>}) ->
  throw(<<"EOF">>);
read_regex(#{src := <<$\\, Ch, Src/binary>>} = State) ->
  Current = maps:get(current, State, <<>>),
  NewState = State#{src => Src, current => <<Current/binary, $\\, Ch>>},
  read_regex(NewState);
read_regex(#{src := <<$", Src/binary>>,
             forms := Forms} = State) ->
  Current = maps:get(current, State, <<>>),
  {ok, Regex} = re:compile(Current),
  State#{src => Src, forms => [Regex | Forms]};
read_regex(#{src := <<Ch, Src/binary>>} = State) ->
  Current = maps:get(current, State, <<>>),
  NewState = State#{src => Src, current => <<Current/binary, Ch>>},
  read_regex(NewState).

%%------------------------------------------------------------------------------
%% #_ discard
%%------------------------------------------------------------------------------

read_discard(State) ->
  {_, NewState} = pop_form(read_one(State)),
  read_one(NewState).

%%------------------------------------------------------------------------------
%% #? cond
%%------------------------------------------------------------------------------

read_cond(_State) -> throw(unimplemented).

%%------------------------------------------------------------------------------
%% #: erlang function
%%------------------------------------------------------------------------------

read_erl_fun(State) ->
  {First, State1} = pop_form(read_one(State)),
  {Second, State2 = #{forms := Forms}} = pop_form(read_one(State1)),

  case {clj_core:type(First), clj_core:type(Second)} of
    {symbol, vector} ->
      Module = clj_core:namespace(First),
      Function = clj_core:name(First),
      Arity = clj_core:first(Second),
      Fun = fun Module:Function/Arity,
      State2#{forms => [Fun | Forms]};
    X ->
      erlang:display(X),
      throw(<<"Reader literal '#:' expects a symbol"
              " followed by a vector with one element.">>)
  end.

%%------------------------------------------------------------------------------
%% Utility functions
%%------------------------------------------------------------------------------

-spec consume(binary(), [clj_utils:char_type()] | fun()) ->
  {binary(), binary()}.
consume(Src, TypesOrPred) ->
  do_consume(Src, <<>>, TypesOrPred).

do_consume(<<>>, Acc, _) ->
  {Acc, <<>>};
do_consume(<<X, Rest/binary>> = Src, Acc, Pred) when is_function(Pred) ->
  case Pred(X) of
    true -> do_consume(Rest, <<Acc/binary, X>>, Pred);
    false -> {Acc, Src}
  end;
do_consume(<<X, Rest/binary>> = Src, Acc, Types) ->
  Type = clj_utils:char_type(X, Rest),
  case lists:member(Type, Types) of
    true -> do_consume(Rest, <<Acc/binary, X>>, Types);
    false -> {Acc, Src}
  end.

-spec read_token(binary()) -> {binary(), binary()}.
read_token(Src) ->
  Fun = fun(Char) ->
            (clj_utils:char_type(Char, <<>>) =/= whitespace)
              andalso (not is_macro_terminating(Char))
        end,
  consume(Src, Fun).

-spec read_until(char(), state()) -> state().
read_until(Delim, #{src := <<Delim, Src/binary>>} = State) ->
  State#{src => Src};
read_until(Delim, #{src := <<X, Src/binary>>} = State) ->
  case clj_utils:char_type(X) of
    whitespace ->
      read_until(Delim, State#{src => Src});
    _ ->
      read_until(Delim, read_one(State))
  end.

-spec is_macro_terminating(char()) -> boolean().
is_macro_terminating(Char) ->
  lists:member(Char,
               [$", $;, $@, $^, $`, $~, $(,
                $), $[, $], ${, $}, $\\ ]).

skip_line(Src) ->
  NotNewline = fun(C) -> C =/= $\n andalso C =/= $\r end,
  {_, RestSrc} = consume(Src, NotNewline),
  RestSrc.

wrapped_read(Symbol, State) ->
  {Expr, NewState} = pop_form(read_one(State)),
  List = 'clojerl.List':new([Symbol, Expr]),
  push_form(List, NewState).

-spec pop_form(state()) -> {any(), state()}.
pop_form(#{forms := [Expr | Forms]} = State) ->
  {Expr, State#{forms => Forms}}.

-spec push_form(any(), state()) -> state().
push_form(Expr, #{forms := Forms} = State) ->
  State#{forms => [Expr | Forms]}.
