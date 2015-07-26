-module(clj_reader).

-export([
         read/1,
         read/2,
         read_all/1,
         read_all/2
        ]).

-include("include/clj_types.hrl").

-type state() :: #{src => binary(),
                   forms => [sexpr()],
                   env => map()}.

-spec read(binary()) -> sexpr().
read(Src) ->
  read(Src, clj_env:empty_env()).

-spec read(binary(), clj_env:env()) -> sexpr().
read(Src, Env) ->
  State = #{src => Src,
            forms => [],
            env => Env},
  #{forms := Forms} = dispatch(State),
  hd(Forms).

-spec read_all(state()) -> [sexpr()].
read_all(Src) ->
  read_all(Src, clj_env:empty_env()).

-spec read_all(state(), clj_env:env()) -> [sexpr()].
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
  next(read_one(State)).

-spec read_one(state()) -> state().
read_one(#{src := <<>>}) ->
  %% If we got here it's because we were expecting something
  %% and it wasn't there.
  throw(<<"EOF">>);
read_one(#{src := <<First, Rest/binary>>} = State) ->
  case clj_utils:char_type(First, Rest) of
    whitespace -> dispatch(State#{src => Rest});
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
    _  ->
      %% Octal unicode
      case clj_utils:char_type(Char, Rest) of
        number ->
          case unicode_char(Src, 8, 3, false) of
            {CodePoint, _} when CodePoint > 8#337 ->
              throw(<<"Octal escape sequence must be in range [0, 377]">>);
            {CodePoint, NewRest} ->
              {unicode:characters_to_binary([CodePoint]), NewRest}
          end;
        _ ->
          throw(<<"Unsupported escape character: \\", Char>>)
      end
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
                Namespace = maps:get(ns, Env),
                clj_keyword:new(Namespace,
                                binary_to_atom(Name, utf8));
              {undefined, Name} ->
                clj_keyword:new(binary_to_atom(Name, utf8));
              {Namespace, Name} ->
                clj_keyword:new(binary_to_atom(Namespace, utf8),
                                binary_to_atom(Name, utf8));
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
               clj_symbol:new(binary_to_atom(Name, utf8));
             {Namespace, Name} ->
               clj_symbol:new(binary_to_atom(Namespace, utf8),
                              binary_to_atom(Name, utf8));
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
  Quote = clj_symbol:new('quote'),
  wrapped_read(Quote, State#{src => Src}).

%%------------------------------------------------------------------------------
%% Deref
%%------------------------------------------------------------------------------

read_deref(#{src := <<$@, Src/binary>>} = State) ->
  Quote = clj_symbol:new('deref'),
  wrapped_read(Quote, State#{src => Src}).

%%------------------------------------------------------------------------------
%% Meta
%%------------------------------------------------------------------------------

read_meta(#{src := <<$^, Src/binary>>} = State) ->
  {SugaredMeta, State1} = pop_form(read_one(State#{src => Src})),
  Meta = clj_utils:desugar_meta(SugaredMeta),

  {Expr, State2} = pop_form(read_one(State1)),
  NewExpr = clj_meta:attach(Meta, Expr),

  push_form(NewExpr, State2).

%%------------------------------------------------------------------------------
%% Syntax quote
%%------------------------------------------------------------------------------

read_syntax_quote(#{src := <<$`, Src/binary>>} = State) ->
  SyntaxQuote = clj_symbol:new('syntax-quote'),
  wrapped_read(SyntaxQuote, State#{src => Src}).

%%------------------------------------------------------------------------------
%% Unquote
%%------------------------------------------------------------------------------

read_unquote(#{src := <<$~, Src/binary>>} = State) ->
  case Src of
    <<$@, RestSrc/binary>> ->
      UnquoteSplicing = clj_symbol:new('clojure.core', 'unquote-splicing'),
      wrapped_read(UnquoteSplicing, State#{src => RestSrc});
    _ ->
      UnquoteSplicing = clj_symbol:new('clojure.core', 'unquote'),
      wrapped_read(UnquoteSplicing, State#{src => Src})
  end.

%%------------------------------------------------------------------------------
%% List
%%------------------------------------------------------------------------------

read_list(#{src := <<$(, Src/binary>>} = State) ->
  State#{src => Src}.

%%------------------------------------------------------------------------------
%% Vector
%%------------------------------------------------------------------------------

read_vector(_) -> vector.

%%------------------------------------------------------------------------------
%% Map
%%------------------------------------------------------------------------------

read_map(_) -> map.

%%------------------------------------------------------------------------------
%% Unmatched delimiter
%%------------------------------------------------------------------------------

read_unmatched_delim(_) -> throw(unmatched_delim).

%%------------------------------------------------------------------------------
%% Character
%%------------------------------------------------------------------------------

read_char(_) -> char.

%%------------------------------------------------------------------------------
%% Argument
%%------------------------------------------------------------------------------

read_arg(_) -> arg.

%%------------------------------------------------------------------------------
%% Reader dispatch
%%------------------------------------------------------------------------------

read_dispatch(_) -> dispatch.

%%------------------------------------------------------------------------------
%% Utility functions
%%------------------------------------------------------------------------------

-spec consume(binary(), [clj_utils:char_type()] | fun()) -> {binary(), binary()}.
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

read_token(Src) ->
  Fun = fun(Char) ->
            (clj_utils:char_type(Char, <<>>) =/= whitespace)
              andalso (not is_macro_terminating(Char))
        end,
  consume(Src, Fun).

is_macro_terminating(Char) ->
  lists:member(Char,
               [$", $;, $@, $^, $`, $~, $(,
                $), $[, $], ${, $}, $\\ ]).

skip_line(Src) ->
  NotNewline = fun(C) -> C =/= $\n andalso C =/= $\r end,
  {_, RestSrc} = consume(Src, NotNewline),
  RestSrc.

wrapped_read(Symbol, State) ->
  NewState = read_one(State),
  #{forms := [Expr | Forms]} = NewState,
  NewState#{forms => [[Symbol, Expr] | Forms]}.

-spec pop_form(state()) -> {sexpr(), state()}.
pop_form(#{forms := [Expr | Forms]} = State) ->
  {Expr, State#{forms => Forms}}.

-spec push_form(sexpr(), state()) -> state().
push_form(Expr, #{forms := Forms} = State) ->
  State#{forms => [Expr | Forms]}.
