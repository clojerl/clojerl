-module('clojerl.Boolean').

-behavior('clojerl.IHash').
-behavior('clojerl.IStringable').

-export([hash/1]).
-export([str/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

hash(true)  -> erlang:phash2(true);
hash(false) -> erlang:phash2(false).

str(true)  -> <<"true">>;
str(false) -> <<"false">>.
