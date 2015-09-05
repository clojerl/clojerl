-module('clojerl.Symbol.clojerl.Stringable').

-behaviour('clojerl.Stringable').

-export([str/1]).

str({'clojerl.Symbol', #{ns := undefined, name := Name}}) ->
  Name;
str({'clojerl.Symbol', #{ns := Namespace, name := Name}}) ->
  <<Namespace/binary, "/", Name/binary>>.
