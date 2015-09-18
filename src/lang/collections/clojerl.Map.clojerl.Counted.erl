-module('clojerl.Map.clojerl.Counted').

-behaviour('clojerl.Counted').

-export([count/1]).

count({_, Map, _}) -> maps:size(Map).
