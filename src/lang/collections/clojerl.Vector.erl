-module('clojerl.Vector').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IMeta').
-behavior('clojerl.Indexed').
-behavior('clojerl.ISequential').
-behavior('clojerl.IStack').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([new/1]).

-export(['clojerl.Counted.count'/1]).
-export([ 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        ]).
-export([ 'clojerl.IEquiv.equiv'/2]).
-export(['clojerl.IFn.invoke'/2]).
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export(['clojerl.ISequential.noop'/1]).
-export([ 'clojerl.Indexed.nth'/2
        , 'clojerl.Indexed.nth'/3
        ]).
-export([ 'clojerl.IStack.peek'/1
        , 'clojerl.IStack.pop'/1
        ]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

-type type() :: #?TYPE{}.

-spec new(list()) -> type().
new(Items) when is_list(Items) ->
  #?TYPE{data = array:from_list(Items, none)}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'(#?TYPE{name = ?M, data = Array}) -> array:size(Array).

'clojerl.IColl.cons'(#?TYPE{name = ?M, data = Array} = Vector, X) ->
  NewArray = array:set(array:size(Array), X, Array),
  Vector#?TYPE{data = NewArray}.

'clojerl.IColl.empty'(_) -> new([]).

'clojerl.IEquiv.equiv'( #?TYPE{name = ?M, data = X}
                      , #?TYPE{name = ?M, data = Y}
                      ) ->
  case array:size(X) == array:size(Y) of
    true ->
      X1 = array:to_list(X),
      Y1 = array:to_list(Y),
      clj_core:equiv(X1, Y1);
    false -> false
  end;
'clojerl.IEquiv.equiv'(#?TYPE{name = ?M, data = X}, Y) ->
  case clj_core:'sequential?'(Y) of
    true  -> clj_core:equiv(array:to_list(X), clj_core:seq(Y));
    false -> false
  end.

'clojerl.IFn.invoke'(#?TYPE{name = ?M, data = Array}, [Index]) ->
  array:get(Index, Array);
'clojerl.IFn.invoke'(#?TYPE{name = ?M}, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for vector, got: ", CountBin/binary>>).

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?TYPE{name = ?M, info = Info} = Vector, Metadata) ->
  Vector#?TYPE{info = Info#{meta => Metadata}}.

'clojerl.ISequential.noop'(_) -> ok.

'clojerl.Indexed.nth'(#?TYPE{name = ?M, data = Array}, N) ->
  array:get(N, Array).

'clojerl.Indexed.nth'(#?TYPE{name = ?M, data = Array}, N, NotFound) ->
  case N > array:size(Array) of
    true  -> NotFound;
    false -> array:get(N, Array)
  end.

'clojerl.IStack.peek'(#?TYPE{name = ?M, data = Array}) ->
  case array:size(Array) of
    0    -> undefined;
    Size -> array:get(Size - 1, Array)
  end.

'clojerl.IStack.pop'(#?TYPE{name = ?M, data = Array} = Vector) ->
  case array:size(Array) of
    0    -> clj_utils:throw(<<"Can't pop empty vector">>);
    Size ->
      NewArray = array:resize(Size - 1, Array),
      Vector#?TYPE{data = NewArray}
  end.

'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = Array}) ->
  case array:size(Array) of
    0 -> undefined;
    _ -> array:to_list(Array)
  end.

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = Array}) ->
  Items = lists:map(fun clj_core:str/1, array:to_list(Array)),
  Strs = clj_utils:binary_join(Items, <<", ">>),
  <<"[", Strs/binary, "]">>.
