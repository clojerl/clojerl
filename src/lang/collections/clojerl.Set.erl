-module('clojerl.Set').

-behavior('clojerl.Counted').
-behavior('clojerl.Stringable').
-behavior('clojerl.Seqable').
-behavior('clojerl.ISeq').
-behavior('clojerl.IMeta').
-behavior('clojerl.IColl').
-behavior('clojerl.ILookup').

-define(T, ?MODULE).

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

-record(?T, { set        :: gb_sets:set()
            , info = #{} :: map()
            }).

-type type() :: #?T{}.

-spec new(list()) -> type().
new(Values) when is_list(Values) ->
  #?T{set = gb_sets:from_list(Values)}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'(#?T{set = Set}) -> gb_sets:size(Set).

'clojerl.Stringable.str'(#?T{set = Set}) ->
  Items = lists:map(fun clj_core:str/1, gb_sets:to_list(Set)),
  Strs = clj_utils:binary_join(Items, <<", ">>),
  <<"#{", Strs/binary, "}">>.

'clojerl.Seqable.seq'(#?T{set = Set}) ->
  case gb_sets:size(Set) of
    0 -> undefined;
    _ -> gb_sets:to_list(Set)
  end.

'clojerl.ISeq.first'(#?T{set = Set}) ->
  Iterator = gb_sets:iterator(Set),
  case gb_sets:next(Iterator) of
    none -> undefined;
    {X, _} -> X
  end.

'clojerl.ISeq.next'(#?T{set = Set}) ->
  case gb_sets:to_list(Set) of
    [] -> undefined;
    [_ | []] -> undefined;
    [_ | Items] -> clj_core:list(Items)
  end.

'clojerl.ISeq.more'(#?T{set = GbSet} = Set) ->
  case gb_sets:size(GbSet) of
    0 -> clj_core:list([]);
    _ -> 'clojerl.ISeq.next'(Set)
  end.

'clojerl.IMeta.meta'(#?T{info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?T{info = Info} = Set, Metadata) ->
  Set#?T{info = Info#{meta => Metadata}}.

'clojerl.IColl.count'(#?T{set = Set}) -> gb_sets:size(Set).

'clojerl.IColl.cons'(#?T{set = Set}, X) ->
  Items = gb_sets:to_list(Set),
  clj_core:list([X | Items]).

'clojerl.IColl.empty'(_) -> new([]).

'clojerl.IColl.equiv'(X, X) -> true;
'clojerl.IColl.equiv'(_, _) -> false.

'clojerl.ILookup.get'(#?T{} = Set, Key) ->
  'clojerl.ILookup.get'(Set, Key, undefined).

'clojerl.ILookup.get'(#?T{set = Set}, Key, NotFound) ->
  case gb_sets:is_member(Key, Set) of
    true -> Key;
    false -> NotFound
  end.
