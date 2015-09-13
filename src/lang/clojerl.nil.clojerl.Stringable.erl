-module('clojerl.nil.clojerl.Stringable').

-behaviour('clojerl.Stringable').

-export([str/1]).

str(undefined) -> <<"nil">>.
