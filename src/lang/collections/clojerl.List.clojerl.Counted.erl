-module('clojerl.List.clojerl.Counted').

-behaviour('clojerl.Counted').

-export([count/1]).

count({_, List, _}) -> length(List).
