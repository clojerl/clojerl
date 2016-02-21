-module('clojerl.Boolean').

-behaviour('clojerl.Stringable').

-export(['clojerl.Stringable.str'/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Stringable.str'(true) -> <<"true">>;
'clojerl.Stringable.str'(false) -> <<"false">>.
