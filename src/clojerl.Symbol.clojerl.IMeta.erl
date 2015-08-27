-module('clojerl.Symbol.clojerl.IMeta').
-behavior('clojerl.IMeta').

-export([meta/1]).

-spec meta('clojerl.Symbol':type()) -> any().
meta({'clojerl.Symbol', Data}) ->
  maps:get(meta, Data, undefined).
