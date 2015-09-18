-module('clojerl.List.clojerl.ISeq').

-behaviour('clojerl.ISeq').

-export([
         first/1,
         next/1,
         more/1
        ]).

-spec first('clojerl.List':type()) -> undefined | any().
first({_, [], _}) -> undefined;
first({_, [First | _], _}) -> First.

-spec next('clojerl.List':type()) -> undefined | 'clojerl.List':type().
next({_, [], _}) -> undefined;
next({_, [_ | []], _}) -> undefined;
next({T, [_ | Rest], Info}) -> {T, Rest, Info}.

-spec more('clojerl.List':type()) -> undefined | 'clojerl.List':type().
more({_, [], _}) -> undefined;
more({T, [_ | Rest], Info}) -> {T, Rest, Info}.
