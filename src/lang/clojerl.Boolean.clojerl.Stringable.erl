-module('clojerl.Boolean.clojerl.Stringable').

-behaviour('clojerl.Stringable').

-export([str/1]).

str(true) -> <<"true">>;
str(false) -> <<"false">>.
