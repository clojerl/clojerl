-module('clojerl.Nil').

-behavior('clojerl.IColl').
-behavior('clojerl.ISeq').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([ 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        ]).
-export([ 'clojerl.ISeq.first'/1
        , 'clojerl.ISeq.next'/1
        , 'clojerl.ISeq.more'/1
        ]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

'clojerl.IColl.cons'(undefined, X) -> [X].

'clojerl.IColl.empty'(undefined) -> undefined.

'clojerl.ISeq.first'(undefined) -> undefined.

'clojerl.ISeq.next'(undefined) -> undefined.

'clojerl.ISeq.more'(undefined) -> undefined.

'clojerl.Stringable.str'(undefined) -> <<"">>.

'clojerl.Seqable.seq'(undefined) -> undefined.
