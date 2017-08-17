-module('erlang.util.UUID').

-include("clojerl.hrl").

-behavior('clojerl.IStringable').
-behavior('clojerl.IHash').

-export([?CONSTRUCTOR/1, random/0]).

-export([hash/1]).
-export([str/1]).

-type type() :: #?TYPE{data :: binary()}.

-define(UUID_REGEX, "^[a-fA-F0-9]{8}-"
                    "[a-fA-F0-9]{4}-"
                    "[a-fA-F0-9]{4}-"
                    "[a-fA-F0-9]{4}-"
                    "[a-fA-F0-9]{12}$"
       ).

-spec ?CONSTRUCTOR(binary()) -> type().
?CONSTRUCTOR(UUID) when is_binary(UUID) ->
  case is_uuid(UUID) of
    false -> error(<<"Invalid UUID: ", UUID/binary>>);
    true  -> #?TYPE{data = UUID}
  end.

-spec is_uuid(binary()) -> boolean().
is_uuid(MaybeUUID) ->
  re:run(MaybeUUID, ?UUID_REGEX) =/= nomatch.

-spec random() -> type().
random() ->
  <<Rand1:48, _:4, Rand2:12, _:2, Rand3:62>> =
    crypto:strong_rand_bytes(16),
  UUIDBin = <<Rand1:48,
              0:1, 1:1, 0:1, 0:1,  % version 4 bits
              Rand2:12,
              1:1, 0:1,            % RFC 4122 variant bits
              Rand3:62>>,
  ?CONSTRUCTOR(uuid_to_string(UUIDBin)).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

hash(Str) ->
  erlang:phash2(Str).

str(#?TYPE{name = ?M, data = UUID}) -> UUID.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec uuid_to_string(binary()) -> binary().
uuid_to_string(UUIDBin) ->
  do_uuid_to_string(UUIDBin, 0, <<>>).

-spec do_uuid_to_string(binary(), integer(), binary()) -> binary().
do_uuid_to_string(<<>>, _Pos, Acc) ->
  Acc;
do_uuid_to_string(<<X:4, Rest/bits>>, Pos, Acc)
  when Pos =:= 8 orelse
       Pos =:= 12 orelse
       Pos =:= 16 orelse
       Pos =:= 20 ->
  Hex = int_to_binary_hex(X),
  do_uuid_to_string(Rest, Pos + 1, <<Acc/binary, "-", Hex/binary>>);
do_uuid_to_string(<<X:4, Rest/bits>>, Pos, Acc) ->
  Hex = int_to_binary_hex(X),
  do_uuid_to_string(Rest, Pos + 1, <<Acc/binary, Hex/binary>>).

-spec int_to_binary_hex(integer()) -> binary().
int_to_binary_hex(X) when X >= 0 andalso X =< 9 ->
  integer_to_binary(X);
int_to_binary_hex(X) when X >= 10 andalso X =< 15 ->
  <<A/utf8>> = <<"A"/utf8>>,
  Hex = (A + X - 10),
  <<Hex/utf8>>.
