-module('clojerl.TupleChunk').

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IChunk').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IIndexed').
-behavior('clojerl.IReduce').

-export([?CONSTRUCTOR/1, ?CONSTRUCTOR/2, ?CONSTRUCTOR/3]).

-export([count/1]).
-export([drop_first/1]).
-export([equiv/2]).
-export([hash/1]).
-export([nth/2, nth/3]).
-export([reduce/2, reduce/3]).

-type type() :: #{ ?TYPE  => ?M
                 , tuple  => tuple()
                 , offset => non_neg_integer()
                 , size   => non_neg_integer()
                 , meta   => ?NIL | any()
                 }.

-spec ?CONSTRUCTOR(tuple()) -> type().
?CONSTRUCTOR(Tuple) when is_tuple(Tuple) ->
  ?CONSTRUCTOR(Tuple, 0).

-spec ?CONSTRUCTOR(tuple(), non_neg_integer()) -> type().
?CONSTRUCTOR(Tuple, Offset) when is_tuple(Tuple), is_integer(Offset) ->
  ?CONSTRUCTOR(Tuple, Offset, erlang:tuple_size(Tuple)).

-spec ?CONSTRUCTOR(tuple(), non_neg_integer(), non_neg_integer()) -> type().
?CONSTRUCTOR(Tuple, Offset, Size) when is_tuple(Tuple),
                                       is_integer(Offset),
                                       is_integer(Size) ->
  #{ ?TYPE  => ?M
   , tuple  => Tuple
   , offset => Offset
   , size   => Size
   , meta   => ?NIL
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(#{?TYPE := ?M, offset := Offset, size := Size}) ->
  Size - Offset.

drop_first(#{ ?TYPE  := ?M
            , offset := Offset
            } = TupleChunk) ->
  TupleChunk#{offset => Offset + 1}.

equiv( #{?TYPE := ?M, size := S, offset := Offset, tuple := X}
     , #{?TYPE := ?M, size := S, offset := Offset, tuple := Y}
     ) ->
  'erlang.Tuple':equiv(X, Y);
equiv(#{?TYPE := ?M}, _) ->
  false.

hash(#{?TYPE := ?M, tuple := Tuple, offset := Offset, size := Size}) ->
  Indexes = lists:seq(Offset + 1, Size),
  Items   = [erlang:element(I, Tuple)|| I <- Indexes],
  clj_murmur3:ordered(Items).

reduce(#{?TYPE := ?M, tuple := Tuple, offset := Offset, size := Size}, Fun) ->
  Size  = erlang:tuple_size(Tuple),
  Init  = erlang:element(Offset + 1, Tuple),
  Items = case Offset + 1 < Size of
            true ->
              Indexes = lists:seq(Offset + 2, Size),
              [erlang:element(Index, Tuple)|| Index <- Indexes];
            false ->
              []
          end,
  do_reduce(Fun, Init, Items).

reduce(#{ ?TYPE  := ?M
        , tuple  := Tuple
        , offset := Offset
        , size   := Size
        }
      , Fun
      , Init
      ) ->
  Size     = erlang:tuple_size(Tuple),
  Items    = [ erlang:element(Index, Tuple)
               || Index <- lists:seq(Offset + 1, Size)
             ],
  do_reduce(Fun, Init, Items).

do_reduce(_Fun, Acc, []) ->
  Acc;
do_reduce(Fun, Acc, [Item | Items]) ->
  Val = clj_rt:apply(Fun, [Acc, Item]),
  case 'clojerl.Reduced':is_reduced(Val) of
    true  -> Val;
    false -> do_reduce(Fun, Val, Items)
  end.

nth(#{?TYPE := ?M} = TupleChunk, N) ->
  nth(TupleChunk, N, ?NIL).

nth(#{?TYPE := ?M, tuple := Tuple, offset := Offset, size := Size}, N, _Default)
  when N >= 0 andalso N < Size - Offset ->
  erlang:element(Offset + N + 1, Tuple);
nth(#{?TYPE := ?M}, _N, Default) ->
  Default.
