-module('clojerl.List').

-behavior('clojerl.Counted').
-behavior('clojerl.Stringable').
-behavior('clojerl.Seqable').
-behavior('clojerl.ISequential').
-behavior('clojerl.ISeq').
-behavior('clojerl.IMeta').
-behavior('clojerl.IColl').

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
        , 'clojerl.IColl.equiv'/2
        ]).

-type type() :: {?MODULE, list()}.

-spec new(list()) -> type().
new(Items) when is_list(Items) ->
  {?MODULE, Items, #{}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'({_, List, _}) -> length(List).

'clojerl.Stringable.str'({'clojerl.List', [], _}) ->
  <<"()">>;
'clojerl.Stringable.str'({'clojerl.List', Items, _}) ->
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs = clj_utils:binary_join(ItemsStrs, <<", ">>),
  <<"(", Strs/binary, ")">>.

'clojerl.Seqable.seq'({_, [], _}) -> undefined;
'clojerl.Seqable.seq'({_, Seq, _}) -> Seq.

'clojerl.ISeq.first'({_, [], _}) -> undefined;
'clojerl.ISeq.first'({_, [First | _], _}) -> First.

'clojerl.ISeq.next'({_, [], _}) -> undefined;
'clojerl.ISeq.next'({_, [_ | []], _}) -> undefined;
'clojerl.ISeq.next'({T, [_ | Rest], Info}) -> {T, Rest, Info}.

'clojerl.ISeq.more'({_, [], _}) -> undefined;
'clojerl.ISeq.more'({T, [_ | Rest], Info}) -> {T, Rest, Info}.

-spec 'clojerl.IMeta.meta'('clojerl.List':type()) -> any().
'clojerl.IMeta.meta'({'clojerl.List', _, Info}) ->
  maps:get(meta, Info, undefined).

-spec 'clojerl.IMeta.with_meta'(any(), 'clojerl.Map':type()) -> any().
'clojerl.IMeta.with_meta'({'clojerl.List', Data, Info}, Metadata) ->
  {'clojerl.List', Data, Info#{meta => Metadata}}.

-spec 'clojerl.IColl.count'('clojerl.List':type()) -> integer().
'clojerl.IColl.count'({_, Items, _}) -> length(Items).

-spec 'clojerl.IColl.cons'('clojerl.List':type(), any()) -> 'clojerl.List':type().
'clojerl.IColl.cons'({T, [], Info}, X) -> {T, [X], Info};
'clojerl.IColl.cons'({T, Items, Info}, X) -> {T, [X | Items], Info}.

-spec 'clojerl.IColl.equiv'('clojerl.List':type(), any()) -> boolean().
'clojerl.IColl.equiv'(X, X) -> true;
'clojerl.IColl.equiv'(_, _) -> false.
