-module('clojerl.Vector.clojerl.Counted').

-export([count/1]).

count({_, Array, _}) -> array:size(Array).
