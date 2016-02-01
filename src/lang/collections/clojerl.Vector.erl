-module('clojerl.Vector').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IMeta').
-behavior('clojerl.ISequential').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([new/1]).

-export(['clojerl.Counted.count'/1]).
-export(['clojerl.Stringable.str'/1]).
-export(['clojerl.Seqable.seq'/1]).
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export([ 'clojerl.IColl.count'/1
        , 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        , 'clojerl.IColl.equiv'/2
        ]).

-type type() :: #?TYPE{}.

-spec new(list()) -> type().
new(Items) when is_list(Items) ->
  #?TYPE{data = array:from_list(Items, none)}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'(#?TYPE{name = ?M, data = Array}) -> array:size(Array).

'clojerl.IColl.count'(#?TYPE{name = ?M, data = Array}) -> array:size(Array).

'clojerl.IColl.cons'(#?TYPE{name = ?M, data = Array} = Vector, X) ->
  NewArray = array:set(array:size(Array), X, Array),
  Vector#?TYPE{data = NewArray}.

'clojerl.IColl.empty'(_) -> new([]).

'clojerl.IColl.equiv'(X, X) -> true;
'clojerl.IColl.equiv'(_, _) -> false.

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?TYPE{name = ?M, info = Info} = Vector, Metadata) ->
  Vector#?TYPE{info = Info#{meta => Metadata}}.

'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = Array}) ->
  case array:size(Array) of
    0 -> undefined;
    _ -> array:to_list(Array)
  end.

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = Array}) ->
  Items = lists:map(fun clj_core:str/1, array:to_list(Array)),
  Strs = clj_utils:binary_join(Items, <<", ">>),
  <<"[", Strs/binary, "]">>.
