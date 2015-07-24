-module(clj_reader).

-export([read/1, read_all/1]).

-type attrs() :: #{}.
-type ast_node() :: #{type => atom,
                      attrs => attrs(),
                      children => [atom()]}.

-type state() :: #{src => binary(),
                   forms => [ast_node()]}.

-type char_type() :: whitespace | number | string
                   | keyword | comment | quote
                   | deref | meta | syntax_quote
                   | unquote | list | vector
                   | map | unmatched_delim | char
                   | unmatched_delim | char
                   | arg | dispatch | symbol.

-spec read(binary()) -> ast_node().
read(Src) ->
  State = #{src => Src, forms => []},
  #{forms := Forms} = dispatch(State),
  hd(Forms).

-spec read_all(state()) -> [ast_node()].
read_all(Src) ->
  State = #{src => Src,
            forms => [],
            all => true},
  #{forms := Forms} = dispatch(State),
  lists:reverse(Forms).

-spec dispatch(state()) -> state().
dispatch(#{src := <<>>} = State) ->
  State;
dispatch(#{src := Src} = State) ->
  <<First, Rest/binary>> = Src,
  NewState = case char_type(First, Rest) of
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
             end,
  next(NewState).

-spec next(state()) -> state().
next(#{all := true} = State) -> dispatch(State);
next(State) -> State.

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
      case char_type(Char, Rest) of
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

read_keyword(_) -> keyword.

%%------------------------------------------------------------------------------
%% Comment
%%------------------------------------------------------------------------------

read_comment(_) -> comment.

%%------------------------------------------------------------------------------
%% Quote
%%------------------------------------------------------------------------------

read_quote(_) -> quote.

%%------------------------------------------------------------------------------
%% Deref
%%------------------------------------------------------------------------------

read_deref(#{forms := Forms,
             src := <<_, Src/binary>>} = State) ->
  State#{forms => [deref | Forms],
         src => Src}.

%%------------------------------------------------------------------------------
%% Meta
%%------------------------------------------------------------------------------

read_meta(_) -> meta.

%%------------------------------------------------------------------------------
%% Syntax quote
%%------------------------------------------------------------------------------

read_syntax_quote(_) -> syntax_quote.

%%------------------------------------------------------------------------------
%% Unquote
%%------------------------------------------------------------------------------

read_unquote(_) -> unquote.

%%------------------------------------------------------------------------------
%% List
%%------------------------------------------------------------------------------

read_list(_) -> list.

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
%% Symbol
%%------------------------------------------------------------------------------

read_symbol(_) -> symbol.

%%------------------------------------------------------------------------------
%% Utility functions
%%------------------------------------------------------------------------------

-spec consume(binary(), [char_type()]) -> {binary(), binary()}.
consume(Src, Types) ->
  do_consume(Src, <<>>, Types).

do_consume(<<>>, Acc, _Types) ->
  {Acc, <<>>};
do_consume(<<X, Rest/binary>> = Src, Acc, Types) ->
  Type = char_type(X, Rest),
  case lists:member(Type, Types) of
    true -> do_consume(Rest, <<Acc/binary, X>>, Types);
    false -> {Acc, Src}
  end.
