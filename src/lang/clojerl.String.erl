-module('clojerl.String').

-behavior('clojerl.Counted').
-behavior('clojerl.IHash').
-behavior('clojerl.ISequential').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([ starts_with/2
        , ends_with/2
        , contains/2
        , append/2
        , join/2
        , char_at/2
        ]).

-export([count/1]).
-export([seq/1]).
-export([hash/1]).
-export([noop/1]).
-export([str/1]).

-spec starts_with(binary(), binary()) -> boolean().
starts_with(Str, Prefix) ->
  Size = size(Prefix),
  case Str of
    <<Prefix:Size/binary, _/binary>> -> true;
    _ -> false
  end.

-spec ends_with(binary(), binary()) -> ok.
ends_with(Str, Ends) when size(Ends) > size(Str)->
  false;
ends_with(Str, Ends) ->
  StrSize = byte_size(Str),
  EndsSize = byte_size(Ends),
  Ends == binary:part(Str, {StrSize, - EndsSize}).

-spec contains(binary(), binary()) -> boolean().
contains(Subject, Pattern) ->
  [] =/= binary:matches(Subject, Pattern).

-spec append([binary()], binary()) -> binary().
append(X, Y) when is_binary(X), is_binary(Y) ->
  <<X/binary, Y/binary>>.

-spec join([binary()], binary()) -> binary().
join([], _) ->
  <<>>;
join([S], _) when is_binary(S) ->
  S;
join([H | T], Sep) ->
  B = << <<Sep/binary, X/binary>> || X <- T >>,
  <<H/binary, B/binary>>.

-spec char_at(binary(), non_neg_integer()) -> binary().
char_at(Str, Index) ->
  Ch = binary:at(Str, Index),
  <<Ch>>.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(Str) ->
  case unicode:characters_to_list(Str) of
    {error, _, _} -> error(<<"Invalid unicode binary string">>);
    List -> erlang:length(List)
  end.

hash(Str) ->
  erlang:phash2(Str).

noop(_) -> ok.

seq(<<>>) -> undefined;
seq(Str)  -> to_seq(Str, []).

str(Str) -> Str.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

to_seq(<<>>, Result) ->
  lists:reverse(Result);
to_seq(<<Ch/utf8, Rest/binary>>, Result) ->
  to_seq(Rest, [<<Ch/utf8>> | Result]).
