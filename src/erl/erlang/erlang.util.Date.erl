-module('erlang.util.Date').

-include("clojerl.hrl").

%% -behavior('clojerl.Stringable').
-behavior('clojerl.IHash').

-export([?CONSTRUCTOR/1, to_erl/1]).

-export([hash/1]).
%% -export([str/1]).

-type erlang_date() :: {{integer(), integer(), integer()}, {integer(), integer(), integer()}}.
-type type() :: #?TYPE{data :: erlang_date()}.

-spec ?CONSTRUCTOR(erlang_date()) -> type().
?CONSTRUCTOR(Date) -> #?TYPE{data = Date}.

to_erl(#?TYPE{data = Date}) ->
    Date.

%% ------------------------------------------------------------------------------
%% Protocols
%% ------------------------------------------------------------------------------

hash(Str) ->
  erlang:phash2(Str).

%% str(#?TYPE{data = Date}) -> UUID.

%% %%------------------------------------------------------------------------------
%% %% Internal
%% %%------------------------------------------------------------------------------

%% -spec uuid_to_string(binary()) -> binary().
%% uuid_to_string(UUIDBin) ->
%%   do_uuid_to_string(UUIDBin, 0, <<>>).

%% -spec do_uuid_to_string(binary(), integer(), binary()) -> binary().
%% do_uuid_to_string(<<>>, _Pos, Acc) ->
%%   Acc;
%% do_uuid_to_string(<<X:4, Rest/bits>>, Pos, Acc)
%%   when Pos =:= 8 orelse
%%        Pos =:= 12 orelse
%%        Pos =:= 16 orelse
%%        Pos =:= 20 ->
%%   Hex = int_to_binary_hex(X),
%%   do_uuid_to_string(Rest, Pos + 1, <<Acc/binary, "-", Hex/binary>>);
%% do_uuid_to_string(<<X:4, Rest/bits>>, Pos, Acc) ->
%%   Hex = int_to_binary_hex(X),
%%   do_uuid_to_string(Rest, Pos + 1, <<Acc/binary, Hex/binary>>).

%% -spec int_to_binary_hex(integer()) -> binary().
%% int_to_binary_hex(X) when X >= 0 andalso X =< 9 ->
%%   integer_to_binary(X);
%% int_to_binary_hex(X) when X >= 10 andalso X =< 15 ->
%%   <<A/utf8>> = <<"A"/utf8>>,
%%   Hex = (A + X - 10),
%%   <<Hex/utf8>>.
