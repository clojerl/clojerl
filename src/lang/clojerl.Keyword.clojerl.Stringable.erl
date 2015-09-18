-module('clojerl.Keyword.clojerl.Stringable').

-behaviour('clojerl.Stringable').

-export([str/1]).

str({'clojerl.Keyword', #{ns := undefined, name := Name}}) ->
  <<":", Name/binary>>;
str({'clojerl.Keyword', #{ns := Namespace, name := Name}}) ->
  <<":", Namespace/binary, "/", Name/binary>>.
