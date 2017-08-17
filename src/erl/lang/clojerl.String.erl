-module('clojerl.String').

-compile({no_auto_import, [length/1]}).

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IHash').
-behavior('clojerl.ISequential').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

-export([ substring/2
        , substring/3
        , starts_with/2
        , ends_with/2
        , contains/2
        , append/2
        , index_of/2
        , last_index_of/2
        , last_index_of/3
        , join/2
        , char_at/2
        , to_upper/1
        , to_lower/1
        , is_whitespace/1
        , is_printable/1
        , length/1
        , split/2
        , split/3
        ]).

-export([count/1]).
-export([hash/1]).
-export(['_'/1]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

-spec substring(binary(), integer()) -> binary().
substring(Str, Start) when is_binary(Str), Start >= 0 ->
  do_substring(Str, Start, count(Str), 0, <<>>).

-spec substring(binary(), integer(), integer()) -> binary().
substring(Str, Start, End) when is_binary(Str), Start =< End, Start >= 0 ->
  do_substring(Str, Start, End, 0, <<>>).

-spec do_substring(binary(), integer(), integer(), integer(), binary()) ->
  binary().
do_substring(_Str, _Start, End, End, Acc) ->
  Acc;
do_substring(<<>>, _Start, _End, _Index, Acc) ->
  Acc;
do_substring(<<Ch/utf8, Str/binary>>, Start, End, Index, Acc)
  when Index >= Start ->
  do_substring(Str, Start, End, Index + 1, <<Acc/binary, Ch/utf8>>);
do_substring(<<_/utf8, Str/binary>>, Start, End, Index, Acc) ->
  do_substring(Str, Start, End, Index + 1, Acc).

-spec starts_with(binary(), binary()) -> boolean().
starts_with(Str, Prefix) ->
  Size = size(Prefix),
  case Str of
    <<Prefix:Size/binary, _/binary>> -> true;
    _ -> false
  end.

-spec ends_with(binary(), binary()) -> boolean().
ends_with(Str, Ends) when size(Ends) > size(Str)->
  false;
ends_with(Str, Ends) ->
  StrSize = byte_size(Str),
  EndsSize = byte_size(Ends),
  Ends == binary:part(Str, {StrSize, - EndsSize}).

-spec contains(binary(), binary()) -> boolean().
contains(Subject, Pattern) ->
  [] =/= binary:matches(Subject, Pattern).

-spec append(binary(), binary()) -> binary().
append(X, Y) when is_binary(X), is_binary(Y) ->
  <<X/binary, Y/binary>>.

-spec index_of(binary(), binary()) -> integer().
index_of(<<>>, _Value) ->
  -1;
index_of(Str, Value) ->
  do_index_of(Str, Value, erlang:size(Value), 0).

-spec do_index_of(binary(), binary(), integer(), integer()) -> integer().
do_index_of(<<>>, _, _, _) ->
  -1;
do_index_of(Str, Value, Length, Index) ->
  case Str of
    <<Value:Length/binary, _/binary>> ->
      Index;
    <<_/utf8, Rest/binary>> ->
      do_index_of(Rest, Value, Length, Index + 1)
  end.

-spec last_index_of(binary(), binary()) -> integer().
last_index_of(Str, Value) ->
  last_index_of(Str, Value, 0).

-spec last_index_of(binary(), binary(), integer()) -> integer().
last_index_of(<<>>, _Value, _FromIndex) ->
  -1;
last_index_of(Str, Value, FromIndex) ->
  Length = count(Str) - FromIndex,
  case binary:matches(Str, Value, [{scope, {FromIndex, Length}}]) of
    [] -> -1;
    Matches ->
      {Index, _} = lists:last(Matches),
      Index
  end.

-spec join([binary()], binary()) -> binary().
join([], _) ->
  <<>>;
join([S], _) ->
  clj_rt:str(S);
join([H | T], Sep) ->
  B = << <<Sep/binary, (clj_rt:str(X))/binary>> || X <- T >>,
  HStr = clj_rt:str(H),
  <<HStr/binary, B/binary>>.

-spec char_at(binary(), non_neg_integer()) -> binary().
char_at(<<Ch/utf8, _/binary>>, 0) ->
  <<Ch/utf8>>;
char_at(<<_/utf8, Str/binary>>, Index) when Index >= 0->
  char_at(Str, Index - 1).

-spec to_upper(binary()) -> binary().
to_upper(Str) when is_binary(Str) ->
  List = unicode:characters_to_list(Str),
  unicode:characters_to_binary(string:to_upper(List)).

-spec to_lower(binary()) -> binary().
to_lower(Str) when is_binary(Str) ->
  List = unicode:characters_to_list(Str),
  unicode:characters_to_binary(string:to_lower(List)).

-spec is_whitespace(binary()) -> boolean().
is_whitespace(Str) ->
  Regex = <<"[\\s\\t\\x0B\\x1C\\x1D\\x1E\\x1F\\x{2000}\\x{2002}]">>,
  match =:= re:run(Str, Regex, [{capture, none}, unicode]).

-spec is_printable(binary()) -> boolean().
is_printable(Str) ->
  io_lib:printable_list(unicode:characters_to_list(Str)).

-spec length(binary()) -> non_neg_integer().
length(Str) ->
  case unicode:characters_to_list(Str) of
    {error, _, _} -> error(<<"Invalid unicode binary string">>);
    List -> erlang:length(List)
  end.

-spec split(binary(), binary()) -> [binary()].
split(Str, Pattern) ->
  split(Str, Pattern, [{return, binary}]).

-spec split(binary(), binary(), [any()]) -> [binary()].
split(Str, Pattern, Options) ->
  re:split(Str, Pattern, Options).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(Str) ->
  length(Str).

hash(Str) ->
  erlang:phash2(Str).

'_'(_) -> ?NIL.

seq(<<>>) -> ?NIL;
seq(Str)  -> to_seq(Str, []).

to_list(Str)  -> to_seq(Str, []).

str(Str) -> Str.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

to_seq(<<>>, Result) ->
  lists:reverse(Result);
to_seq(<<Ch/utf8, Rest/binary>>, Result) ->
  to_seq(Rest, [<<Ch/utf8>> | Result]).
