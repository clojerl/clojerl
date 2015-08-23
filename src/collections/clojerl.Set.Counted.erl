-module('clojerl.Set.Counted').

-behaviour('clojerl.Counted').

-export([count/1]).

count({_, Set}) -> gb_sets:size(Set).
