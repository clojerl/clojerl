-module('clojerl.Nil').

-include("clojerl.hrl").

-behavior('clojerl.IHash').
-behavior('clojerl.ISeqable').

-export([hash/1]).
-export([ seq/1
        , to_list/1
        ]).

hash(?NIL) -> 0.

seq(?NIL) -> ?NIL.
to_list(?NIL) -> [].
