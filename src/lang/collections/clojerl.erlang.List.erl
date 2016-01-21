-module('clojerl.erlang.List').

-export(['clojerl.Stringable.str'/1]).
-export(['clojerl.Seqable.seq'/1]).
-export([ 'clojerl.ISeq.first'/1
        , 'clojerl.ISeq.more'/1
        , 'clojerl.ISeq.next'/1
        ]).

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
