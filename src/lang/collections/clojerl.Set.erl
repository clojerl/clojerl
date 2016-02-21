-module('clojerl.Set').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IColl').
-behavior('clojerl.IMeta').
-behavior('clojerl.ISet').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([new/1]).
-export(['clojerl.Counted.count'/1]).
-export([ 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        ]).
-export(['clojerl.IEquiv.equiv'/2]).
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export([ 'clojerl.ISet.disjoin'/2
        , 'clojerl.ISet.contains'/2
        , 'clojerl.ISet.get'/2
        ]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

-type type() :: #?TYPE{}.

-spec new(list()) -> type().
new(Values) when is_list(Values) ->
  KVs = lists:map(fun(X) -> {X, true} end, Values),
  #?TYPE{data = maps:from_list(KVs)}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.Counted

'clojerl.Counted.count'(#?TYPE{name = ?M, data = MapSet}) -> maps:size(MapSet).

%% clojerl.IColl

'clojerl.IColl.cons'(#?TYPE{name = ?M, data = MapSet} = Set, X) ->
  case maps:is_key(X, MapSet) of
    true  -> Set;
    false -> Set#?TYPE{data = MapSet#{X => true}}
  end.

'clojerl.IColl.empty'(_) -> new([]).

%% clojerl.IEquiv

'clojerl.IEquiv.equiv'( #?TYPE{name = ?M, data = X}
                      , #?TYPE{name = ?M, data = Y}
                      ) ->
  clj_core:equiv(X, Y);
'clojerl.IEquiv.equiv'(_, _) ->
  false.

%% clojerl.IMeta

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?TYPE{name = ?M, info = Info} = Set, Metadata) ->
  Set#?TYPE{info = Info#{meta => Metadata}}.

%% clojerl.ISet

'clojerl.ISet.disjoin'(#?TYPE{name = ?M, data = MapSet} = Set, Key) ->
  Set#?TYPE{name = ?M, data = maps:remove(Key, MapSet)}.

'clojerl.ISet.contains'(#?TYPE{name = ?M, data = MapSet}, Key) ->
  maps:is_key(Key, MapSet).

'clojerl.ISet.get'(#?TYPE{name = ?M, data = MapSet}, Key) ->
  case maps:is_key(Key, MapSet) of
    true  -> Key;
    false -> undefined
  end.

%% clojerl.Seqable

'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = MapSet}) ->
  case maps:size(MapSet) of
    0 -> undefined;
    _ -> maps:keys(MapSet)
  end.

%% clojerl.Stringable

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = MapSet}) ->
  Items = lists:map(fun clj_core:str/1, maps:keys(MapSet)),
  Strs = clj_utils:binary_join(Items, <<", ">>),
  <<"#{", Strs/binary, "}">>.
