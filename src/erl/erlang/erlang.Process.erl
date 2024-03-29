-module('erlang.Process').

-behavior('clojerl.IStringable').
-behavior('clojerl.IHash').

-export([str/1]).
-export([hash/1]).

-export_type([type/0]).
-type type() :: pid().

%% clojerl.IStringable

str(Pid) when is_pid(Pid) ->
  PidStr = pid_to_list(Pid),
  PidBin = list_to_binary(PidStr),
  <<"#", PidBin/binary>>.

%% clojerl.IHash

hash(Pid) when is_pid(Pid) ->
  erlang:phash2(Pid).
