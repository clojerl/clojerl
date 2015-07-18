-module(clj_reader).

-export([read/1]).

-type attrs() :: #{}.
-type ast_node() :: #{type => atom,
                      attrs => attrs(),
                      children => [atom()]}.

-spec read(binary()) -> ast_node().
read(Src) ->
  dispatch(Src).

dispatch(<<>>) ->
  eof;
dispatch(<<First, Rest/binary>> = Src) ->
  case type_start(First, Rest) of
    whitespace -> dispatch(Rest);
    number -> read_number(Src);
    string -> read_string(Src);
    keyword -> read_keyword(Src);
    comment -> read_comment(Src);
    quote -> read_quote(Src);
    deref -> read_deref(Src);
    meta -> read_meta(Src);
    syntax_quote -> read_syntax_quote(Src);
    unquote -> read_unquote(Src);
    list -> read_list(Src);
    vector -> read_vector(Src);
    map -> read_map(Src);
    unmatched_delim -> read_unmatched_delim(Src);
    char -> read_char(Src);
    arg -> read_arg(Src);
    dispatch -> read_dispatch(Src);
    symbol -> read_symbol(Src)
  end.

type_start(X, _)
  when X == $\n; X == $\t; X == $\r; X == $ ; X == $,->
  whitespace;
type_start(X, _)
  when X >= $0, X =< $9 ->
  number;
type_start(X, <<Y, _/binary>>)
  when (X == $+ orelse X == $-),
       Y >= $0, Y =< $9 ->
  number;
type_start($", _) -> string;
type_start($:, _) -> keyword;
type_start($;, _) -> comment;
type_start($', _) -> quote;
type_start($@, _) -> deref;
type_start($^, _) -> meta;
type_start($`, _) -> syntax_quote;
type_start($~, _) -> unquote;
type_start($(, _) -> list;
type_start($[, _) -> vector;
type_start(${, _) -> map;
type_start(X, _) when X == $); X == $]; X == $} ->
  unmatched_delim;
type_start($\\, _) -> char;
type_start($%, _) -> arg;
type_start($#, _) -> dispatch;
type_start(_, _) -> symbol.

read_number(_) -> number.
read_string(_) -> string.
read_keyword(_) -> keyword.
read_comment(_) -> comment.
read_quote(_) -> quote.
read_deref(_) -> deref.
read_meta(_) -> meta.
read_syntax_quote(_) -> syntax_quote.
read_unquote(_) -> unquote.
read_list(_) -> list.
read_vector(_) -> vector.
read_map(_) -> map.
read_unmatched_delim(_) -> throw(unmatched_delim).
read_char(_) -> char.
read_arg(_) -> arg.
read_dispatch(_) -> dispatch.
read_symbol(_) -> symbol.
