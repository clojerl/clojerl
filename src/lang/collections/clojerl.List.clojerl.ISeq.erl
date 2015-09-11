-module('clojerl.List.clojerl.ISeq').

-behaviour('clojerl.ISeq').

-export([
         first/1,
         next/1,
         more/1
        ]).

-spec first('clojerl.List':type()) -> undefined | any().
first({_, []}) -> undefined;
first({_, [First | _]}) -> First.

-spec next('clojerl.List':type()) -> undefined | 'clojerl.List':type().
next({_, []}) -> undefined;
next({_, [_ | []]}) -> undefined;
next({T, [_ | Rest]}) -> {T, Rest}.

-spec more('clojerl.List':type()) -> undefined | 'clojerl.List':type().
more({_, []}) -> undefined;
more({T, [_ | Rest]}) -> {T, Rest}.
