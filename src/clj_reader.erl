-module(clj_reader).

-export([read/1, read_all/1]).

-type attrs() :: #{}.
-type ast_node() :: #{type => atom,
                      attrs => attrs(),
                      children => [atom()]}.

-type state() :: #{src => binary(),
                   forms => [ast_node()]}.

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
  NewState = case type_char(First, Rest) of
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

-spec type_char(non_neg_integer(), binary()) -> atom().
type_char(X, _)
  when X == $\n; X == $\t; X == $\r; X == $ ; X == $,->
  whitespace;
type_char(X, _)
  when X >= $0, X =< $9 ->
  number;
type_char(X, <<Y, _/binary>>)
  when (X == $+ orelse X == $-),
       Y >= $0, Y =< $9 ->
  number;
type_char($", _) -> string;
type_char($:, _) -> keyword;
type_char($;, _) -> comment;
type_char($', _) -> quote;
type_char($@, _) -> deref;
type_char($^, _) -> meta;
type_char($`, _) -> syntax_quote;
type_char($~, _) -> unquote;
type_char($(, _) -> list;
type_char($[, _) -> vector;
type_char(${, _) -> map;
type_char(X, _)
  when X == $); X == $]; X == $} ->
  unmatched_delim;
type_char($\\, _) -> char;
type_char($%, _) -> arg;
type_char($#, _) -> dispatch;
type_char(_, _) -> symbol.

-spec read_number(state()) -> state().
read_number(#{forms := Forms, src := Src} = State) ->
  {SrcRest, Current} = consume(Src, [number, symbol]),
  Number = clj_utils:parse_number(Current),
  State#{forms => [Number | Forms],
         src => SrcRest}.

read_string(_) -> string.
read_keyword(_) -> keyword.
read_comment(_) -> comment.
read_quote(_) -> quote.
read_deref(#{forms := Forms,
             src := <<_, Src/binary>>} = State) ->
  State#{forms => [deref | Forms],
         src => Src}.

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

consume(Src, Types) ->
  do_consume(Src, <<>>, Types).

do_consume(<<>>, Acc, _Types) ->
  {<<>>, Acc};
do_consume(<<X, Rest/binary>> = Src, Acc, Types) ->
  Type = type_char(X, Rest),
  case lists:member(Type, Types) of
    true -> do_consume(Rest, <<Acc/binary, X>>, Types);
    false -> {Src, Acc}
  end.
