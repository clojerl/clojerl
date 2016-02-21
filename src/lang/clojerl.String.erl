-module('clojerl.String').

-behaviour('clojerl.Seqable').
-behaviour('clojerl.Stringable').

-export([starts_with/2]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

-spec starts_with(binary(), binary()) -> boolean().
starts_with(Str, Prefix) ->
  Size = size(Prefix),
  case Str of
    <<Prefix:Size/binary, _/binary>> -> true;
    _ -> false
  end.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

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
