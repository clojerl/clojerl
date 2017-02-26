-module('clojerl.BitString').

-include("clojerl.hrl").

-behavior('clojerl.Stringable').

-export([str/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

str(_BitStr) ->
  <<"#bin[64]">>.
