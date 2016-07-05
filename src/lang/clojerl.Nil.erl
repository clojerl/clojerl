-module('clojerl.Nil').

-behavior('clojerl.IHash').
-behavior('clojerl.Seqable').
-behavior('clojerl.Stringable').

-export([hash/1]).
-export([seq/1]).
-export([str/1]).

hash(undefined) -> erlang:phash2(undefined).
str(undefined) -> <<"">>.
seq(undefined) -> undefined.
