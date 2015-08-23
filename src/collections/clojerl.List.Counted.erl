-module('clojerl.List.Counted').

-behaviour('clojerl.Counted').

-export([count/1]).

count({_, List}) -> length(List).
