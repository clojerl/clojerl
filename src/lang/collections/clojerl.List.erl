-module('clojerl.List').

-behavior('clojerl.Counted').
-behavior('clojerl.Stringable').
-behavior('clojerl.Seqable').
-behavior('clojerl.ISequential').
-behavior('clojerl.ISeq').
-behavior('clojerl.IMeta').
-behavior('clojerl.IColl').

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

-record(?T, { list       :: list()
            , info = #{} :: map()
            }).

-type type() :: #?T{}.

-spec new(list()) -> type().
new(Items) when is_list(Items) ->
  #?T{list = Items}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'(#?T{list = List}) -> length(List).

'clojerl.Stringable.str'(#?T{list = []}) ->
  <<"()">>;
'clojerl.Stringable.str'(#?T{list = Items}) ->
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs = clj_utils:binary_join(ItemsStrs, <<" ">>),
  <<"(", Strs/binary, ")">>.

'clojerl.Seqable.seq'(#?T{list = []}) -> undefined;
'clojerl.Seqable.seq'(#?T{list = Seq}) -> Seq.

'clojerl.ISeq.first'(#?T{list = []}) -> undefined;
'clojerl.ISeq.first'(#?T{list = [First | _]}) -> First.

'clojerl.ISeq.next'(#?T{list = []}) -> undefined;
'clojerl.ISeq.next'(#?T{list = [_ | []]}) -> undefined;
'clojerl.ISeq.next'(#?T{list = [_ | Rest]} = List) -> List#?T{list = Rest}.

'clojerl.ISeq.more'(#?T{list = []}) -> undefined;
'clojerl.ISeq.more'(#?T{list = [_ | Rest]} = List) -> List#?T{list = Rest}.

'clojerl.IMeta.meta'(#?T{info = Info}) ->
  maps:get(meta, Info, undefined).

'clojerl.IMeta.with_meta'(#?T{info = Info} = List, Metadata) ->
  List#?T{info = Info#{meta => Metadata}}.

'clojerl.IColl.count'(#?T{list = Items}) -> length(Items).

'clojerl.IColl.cons'(#?T{list = []} = List, X) -> List#?T{list = [X]};
'clojerl.IColl.cons'(#?T{list = Items} = List, X) -> List#?T{list = [X | Items]}.

'clojerl.IColl.empty'(_) -> new([]).

'clojerl.IColl.equiv'(X, X) -> true;
'clojerl.IColl.equiv'(_, _) -> false.
