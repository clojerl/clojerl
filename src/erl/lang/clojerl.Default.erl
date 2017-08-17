%% Used to allow a 'default' symbol as a type in extend-type form as a
%% catch all implementation
-module('clojerl.Default').

-behavior('clojerl.IStringable').

-export([str/1]).

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

str(_BitStr) ->
  <<"#<default>">>.
