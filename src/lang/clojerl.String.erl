-module('clojerl.String').

-behavior('clojerl.Counted').
-behavior('clojerl.IHash').
-behavior('clojerl.ISequential').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([ starts_with/2
        , char_at/2
        ]).

-export(['clojerl.Counted.count'/1]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.IHash.hash'/1]).
-export(['clojerl.ISequential.noop'/1]).
-export(['clojerl.Stringable.str'/1]).

-spec starts_with(binary(), binary()) -> boolean().
starts_with(Str, Prefix) ->
  Size = size(Prefix),
  case Str of
    <<Prefix:Size/binary, _/binary>> -> true;
    _ -> false
  end.

-spec char_at(binary(), non_neg_integer()) -> binary().
char_at(Str, Index) ->
  Ch = binary:at(Str, Index),
  <<Ch>>.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'(Str) ->
  erlang:length(unicode:characters_to_list(Str)).

'clojerl.IHash.hash'(Str) ->
  erlang:phash2(Str).

'clojerl.ISequential.noop'(_) -> ok.

'clojerl.Seqable.seq'(<<>>) -> undefined;
'clojerl.Seqable.seq'(Str) ->
  to_seq(Str, []).

'clojerl.Stringable.str'(Str) -> Str.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

to_seq(<<>>, Result) ->
  lists:reverse(Result);
to_seq(<<Ch/utf8, Rest/binary>>, Result) ->
  to_seq(Rest, [<<Ch/utf8>> | Result]).
