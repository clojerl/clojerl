-module('clojerl.Map.clojerl.ISeq').

-behaviour('clojerl.ISeq').

-export([
         first/1,
         next/1,
         more/1
        ]).

-spec first('clojerl.Map':type()) -> undefined | any().
first(Map) -> clj_core:first(clj_core:seq(Map)).

-spec next('clojerl.Set':type()) -> undefined | 'clojerl.List':type().
next(Map) -> clj_core:next(clj_core:seq(Map)).

-spec more('clojerl.Set':type()) -> undefined | 'clojerl.List':type().
more(Map) -> clj_core:rest(clj_core:seq(Map)).
