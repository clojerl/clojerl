-module('clojerl.Set').

-include("clojerl.hrl").

-behavior('clojerl.Counted').
-behavior('clojerl.Stringable').
-behavior('clojerl.Seqable').
-behavior('clojerl.ISeq').
-behavior('clojerl.IMeta').
-behavior('clojerl.IColl').
-behavior('clojerl.ILookup').

-export([new/1]).
-export(['clojerl.Counted.count'/1]).
-export(['clojerl.Stringable.str'/1]).
-export(['clojerl.Seqable.seq'/1]).
-export([ 'clojerl.ISeq.first'/1
        , 'clojerl.ISeq.next'/1
        , 'clojerl.ISeq.more'/1
        ]).
-export([ 'clojerl.IMeta.meta'/1
        , 'clojerl.IMeta.with_meta'/2
        ]).
-export([ 'clojerl.IColl.count'/1
        , 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        , 'clojerl.IColl.equiv'/2
        ]).
-export([ 'clojerl.ILookup.get'/2
        , 'clojerl.ILookup.get'/3
        ]).

-type type() :: #?TYPE{}.

-spec new(list()) -> type().
new(Values) when is_list(Values) ->
  #?TYPE{data = gb_sets:from_list(Values)}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'(#?TYPE{name = ?M, data = Set}) -> gb_sets:size(Set).

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = Set}) ->
  Items = lists:map(fun clj_core:str/1, gb_sets:to_list(Set)),
  Strs = clj_utils:binary_join(Items, <<", ">>),
  <<"#{", Strs/binary, "}">>.

'clojerl.Seqable.seq'(#?TYPE{name = ?M, data = Set}) ->
  case gb_sets:size(Set) of
    0 -> undefined;
    _ -> gb_sets:to_list(Set)
  end.

'clojerl.ISeq.first'(#?TYPE{name = ?M, data = Set}) ->
  Iterator = gb_sets:iterator(Set),
  case gb_sets:next(Iterator) of
    none -> undefined;
    {X, _} -> X
  end.

'clojerl.ISeq.next'(#?TYPE{name = ?M, data = Set}) ->
  case gb_sets:to_list(Set) of
    [] -> undefined;
    [_ | []] -> undefined;
    [_ | Items] -> clj_core:list(Items)
  end.

'clojerl.ISeq.more'(#?TYPE{name = ?M, data = GbSet} = Set) ->
  case gb_sets:size(GbSet) of
    0 -> clj_core:list([]);
    _ -> 'clojerl.ISeq.next'(Set)
  end.

'clojerl.IMeta.meta'(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?TYPE{name = ?M, info = Info} = Set, Metadata) ->
  Set#?TYPE{info = Info#{meta => Metadata}}.

'clojerl.IColl.count'(#?TYPE{name = ?M, data = Set}) -> gb_sets:size(Set).

'clojerl.IColl.cons'(#?TYPE{name = ?M, data = Set}, X) ->
  Items = gb_sets:to_list(Set),
  clj_core:list([X | Items]).

'clojerl.IColl.empty'(_) -> new([]).

'clojerl.IColl.equiv'(X, X) -> true;
'clojerl.IColl.equiv'(_, _) -> false.

'clojerl.ILookup.get'(#?TYPE{name = ?M} = Set, Key) ->
  'clojerl.ILookup.get'(Set, Key, undefined).

'clojerl.ILookup.get'(#?TYPE{name = ?M, data = Set}, Key, NotFound) ->
  case gb_sets:is_member(Key, Set) of
    true -> Key;
    false -> NotFound
  end.
