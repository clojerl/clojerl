-module('clojerl.Vector.Counted').

-export([count/1]).

count({_, Array}) -> array:size(Array).
