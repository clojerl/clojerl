-module('clojerl.Nil').

-behavior('clojerl.IHash').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export(['clojerl.IHash.hash'/1]).
-export(['clojerl.Seqable.seq'/1]).
-export(['clojerl.Stringable.str'/1]).

'clojerl.IHash.hash'(undefined) -> erlang:phash2(undefined).

'clojerl.Stringable.str'(undefined) -> <<"">>.

'clojerl.Seqable.seq'(undefined) -> undefined.
