-module('clojerl.Stringable').

-export([str/1]).

-type type() :: binary().

-callback 'clojerl.Stringable.str'(any()) -> type().

-spec str(any()) -> type().
str(X) -> 'clojerl.protocol':resolve(?MODULE, str, [X]).
