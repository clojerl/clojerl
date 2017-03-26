-module('clojerl.BitString').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IHash').
-behavior('clojerl.ISequential').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([count/1]).
-export([hash/1]).
-export(['_'/1]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(BitString) ->
  bit_size(BitString).

hash(BitString) ->
  erlang:phash2(BitString).

'_'(_) -> ?NIL.

seq(<<>>) -> ?NIL;
seq(BitString) -> bitstring_to_list(BitString).

to_list(Str)  -> seq(Str).

str(BitString) when is_bitstring(BitString) ->
  Elements    = do_str(bitstring_to_list(BitString), []),
  ElementsStr = string:join(Elements, ","),
  ElementsBin = iolist_to_binary(ElementsStr),

  <<"#bin[", ElementsBin/binary, "]">>.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

do_str([], Acc) ->
  lists:reverse(Acc);
do_str([Byte | Rest], Acc) when is_integer(Byte) ->
  ByteStr = integer_to_list(Byte),
  do_str(Rest, [ByteStr | Acc]);
do_str([BitString | Rest], Acc) when is_bitstring(BitString) ->
  Size           = bit_size(BitString),
  <<Value:Size>> = BitString,
  ValueStr       = integer_to_list(Value),
  SizeStr        = integer_to_list(Size),
  BitStringStr   = ["[", ValueStr, " :unit ", SizeStr, " :size 1]"],
  do_str(Rest, [BitStringStr | Acc]).
