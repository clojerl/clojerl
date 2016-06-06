-module('clojerl.Set').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
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
-export(['clojerl.IFn.invoke'/2]).
-export(['clojerl.IHash.hash'/1]).
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
  KVs = lists:map(fun(X) -> {'clojerl.IHash':hash(X), X} end, Values),
  #?TYPE{data = maps:from_list(KVs)}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.Counted

'clojerl.Counted.count'(#?TYPE{name = ?M, data = MapSet}) -> maps:size(MapSet).

%% clojerl.IColl

'clojerl.IColl.cons'(#?TYPE{name = ?M, data = MapSet} = Set, X) ->
  Hash = 'clojerl.IHash':hash(X),
  case maps:is_key(Hash, MapSet) of
    true  -> Set;
    false -> Set#?TYPE{data = MapSet#{Hash => X}}
  end.

'clojerl.IColl.empty'(_) -> new([]).

%% clojerl.IEquiv

'clojerl.IEquiv.equiv'( #?TYPE{name = ?M, data = X}
                      , #?TYPE{name = ?M, data = Y}
                      ) ->
  clj_core:equiv(X, Y);
'clojerl.IEquiv.equiv'(_, _) ->
  false.

%% clojerl.IFn

'clojerl.IFn.invoke'(#?TYPE{name = ?M, data = MapSet}, [Item]) ->
  Hash = 'clojerl.IHash':hash(Item),
  case maps:is_key(Hash, MapSet) of
    true  -> Item;
    false -> undefined
  end;
'clojerl.IFn.invoke'(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for set, got: ", CountBin/binary>>).

%% clojerl.IHash

'clojerl.IHash.hash'(#?TYPE{name = ?M, data = MapSet}) ->
  erlang:phash2(MapSet).

%% clojerl.IMeta

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?TYPE{name = ?M, info = Info} = Set, Metadata) ->
  Set#?TYPE{info = Info#{meta => Metadata}}.

%% clojerl.ISet

'clojerl.ISet.disjoin'(#?TYPE{name = ?M, data = MapSet} = Set, Value) ->
  Hash = 'clojerl.IHash':hash(Value),
  Set#?TYPE{name = ?M, data = maps:remove(Hash, MapSet)}.

'clojerl.ISet.contains'(#?TYPE{name = ?M, data = MapSet}, Value) ->
  Hash = 'clojerl.IHash':hash(Value),
  maps:is_key(Hash, MapSet).

'clojerl.ISet.get'(#?TYPE{name = ?M, data = MapSet}, Value) ->
  Hash = 'clojerl.IHash':hash(Value),
  case maps:is_key(Hash, MapSet) of
    true  -> maps:get(Hash, MapSet);
    false -> undefined
  end.

%% clojerl.Seqable

'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = MapSet}) ->
  case maps:size(MapSet) of
    0 -> undefined;
    _ -> maps:values(MapSet)
  end.

%% clojerl.Stringable

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = MapSet}) ->
  Items = lists:map(fun clj_core:str/1, maps:values(MapSet)),
  Strs = clj_utils:binary_join(Items, <<", ">>),
  <<"#{", Strs/binary, "}">>.
