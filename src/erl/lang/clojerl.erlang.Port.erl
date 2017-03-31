-module('clojerl.erlang.Port').

-behaviour('clojerl.Stringable').
-behaviour('clojerl.IHash').

-export([str/1]).
-export([hash/1]).

%% clojerl.Stringable

str(Port) when is_port(Port) ->
  PortStr = erlang:port_to_list(Port),
  <<"#Port<", PortBin/binary>> = list_to_binary(PortStr),
  <<"#<Port ", PortBin/binary>>.

%% clojerl.IHash

hash(Port) when is_pid(Port) ->
  erlang:phash2(Port).
