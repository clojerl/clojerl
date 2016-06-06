-module('clojerl.Boolean').

-behavior('clojerl.IHash').
-behaviour('clojerl.Stringable').

-export(['clojerl.IHash.hash'/1]).
-export(['clojerl.Stringable.str'/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.IHash.hash'(true)  -> erlang:phash2(true);
'clojerl.IHash.hash'(false) -> erlang:phash2(false).

'clojerl.Stringable.str'(true)  -> <<"true">>;
'clojerl.Stringable.str'(false) -> <<"false">>.
