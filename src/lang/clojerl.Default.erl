%% Used to allow a 'default' symbol as a type in extend-type form as a
%% catch all implementation
-module('clojerl.Default').

-include("clojerl.hrl").

-behavior('clojerl.Stringable').

-export([str/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

str(_BitStr) ->
  <<"#<default>">>.
