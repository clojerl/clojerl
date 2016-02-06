-module('clojerl.Nil').

-behavior('clojerl.IColl').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([ 'clojerl.IColl.cons'/2
        , 'clojerl.IColl.empty'/1
        ]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

'clojerl.IColl.cons'(undefined, X) -> [X]. 

'clojerl.IColl.empty'(undefined) -> undefined.

'clojerl.Stringable.str'(undefined) -> <<"">>.

'clojerl.Seqable.seq'(undefined) -> undefined.
