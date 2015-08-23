-module('clojerl.Map.Counted').

-behaviour('clojerl.Counted').

-export([count/1]).

count({_, Map}) -> maps:size(Map).
