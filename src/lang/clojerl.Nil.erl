-module('clojerl.Nil').

-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

'clojerl.Stringable.str'(undefined) -> <<"">>.

'clojerl.Seqable.seq'(undefined) -> undefined.
