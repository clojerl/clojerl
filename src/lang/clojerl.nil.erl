-module('clojerl.nil').

-behaviour('clojerl.Stringable').

-export(['clojerl.Stringable.str'/1]).

'clojerl.Stringable.str'(undefined) -> <<"">>.
