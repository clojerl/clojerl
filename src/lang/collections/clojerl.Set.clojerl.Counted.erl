-module('clojerl.Set.clojerl.Counted').

-behaviour('clojerl.Counted').

-export([count/1]).

count({_, Set, _}) -> gb_sets:size(Set).
