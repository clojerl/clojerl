-module('clojerl.Stringable').

-export([str/1]).

-type type() ::  any().

-callback str(any()) -> any().

-spec str(type()) -> any().
str(X) -> 'clojerl.protocol':resolve(?MODULE, str, [X]).
