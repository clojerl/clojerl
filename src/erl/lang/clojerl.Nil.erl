-module('clojerl.Nil').

-include("clojerl.hrl").

-behavior('clojerl.IHash').
-behavior('clojerl.ISeqable').
-behavior('clojerl.IStringable').

-export([hash/1]).
-export([ seq/1
        , to_list/1
        ]).
-export([str/1]).

hash(?NIL) -> erlang:phash2(?NIL).
str(?NIL) -> <<"">>.
seq(?NIL) -> ?NIL.
to_list(?NIL) -> [].
