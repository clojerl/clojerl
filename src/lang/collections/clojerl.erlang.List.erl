-module('clojerl.erlang.List').

-behavior('clojerl.Counted').
-behavior('clojerl.IColl').
-behavior('clojerl.ISeq').
-behavior('clojerl.ISequential').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export(['clojerl.Counted.count'/1]).
-export([ 'clojerl.IColl.count'/1
        , 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        , 'clojerl.IColl.equiv'/2
        ]).
-export([ 'clojerl.ISeq.first'/1
        , 'clojerl.ISeq.more'/1
        , 'clojerl.ISeq.next'/1
        ]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Counted.count'(Items) -> length(Items).

'clojerl.Stringable.str'([]) ->
  <<"()">>;
'clojerl.Stringable.str'(Items) ->
  ItemsStrs = lists:map(fun clj_core:str/1, Items),
  Strs = clj_utils:binary_join(ItemsStrs, <<" ">>),
  <<"(", Strs/binary, ")">>.

'clojerl.Seqable.seq'([]) -> undefined;
'clojerl.Seqable.seq'(List) -> List.

'clojerl.ISeq.first'([]) -> undefined;
'clojerl.ISeq.first'([First | _]) -> First.

'clojerl.ISeq.more'([]) -> undefined;
'clojerl.ISeq.more'([_ | Rest]) -> Rest.

'clojerl.ISeq.next'([]) -> undefined;
'clojerl.ISeq.next'([_ | []]) -> undefined;
'clojerl.ISeq.next'([_ | Rest]) -> Rest.

'clojerl.IColl.count'(Items) -> length(Items).

'clojerl.IColl.cons'([], X) ->
  [X];
'clojerl.IColl.cons'(Items, X) ->
  [X | Items].

'clojerl.IColl.empty'(_) -> [].

'clojerl.IColl.equiv'(X, X) -> true;
'clojerl.IColl.equiv'(_, _) -> false.
