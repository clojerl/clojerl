-module('clojerl.erlang.Process').

-behaviour('clojerl.Stringable').

-export(['clojerl.Stringable.str'/1]).

'clojerl.Stringable.str'(Pid) when is_pid(Pid) ->
  PidStr = pid_to_list(Pid),
  PidBin = list_to_binary(PidStr),
  <<"#<", PidBin/binary, ">">>.
