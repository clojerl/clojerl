-module('erlang.io.File').

-include("clojerl.hrl").

-behaviour('erlang.io.Closeable').
-behaviour('erlang.io.IReader').
-behaviour('erlang.io.IWriter').
-behaviour('clojerl.Stringable').

-export([open/1, open/2]).

-export([close/1]).
-export([  read/1
        ,  read/2
        ,  read_line/1
        ,  skip/2
        ,  unread/2
        ]).
-export([ write/2
        , write/3
        ]).
-export([str/1]).

-type type() :: #?TYPE{data :: pid()}.

-spec open(binary()) -> type().
open(Path) ->
  open(Path, [read]).

-spec open(binary(), [atom()]) -> type().
open(Path, Modes) ->
  case file:open(Path, Modes) of
    {ok, Pid}       -> #?TYPE{data = Pid};
    {error, Reason} -> error(Reason)
  end.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

close(#?TYPE{name = ?M, data = Pid}) ->
  case file:close(Pid) of
    {error, _} -> error(<<"Couldn't close erlang.io.File">>);
    ok         -> undefined
  end.

read(File) ->
  read(File, 1).

read(#?TYPE{name = ?M, data = Pid}, Length) ->
  case io:get_chars(Pid, "", Length) of
    eof -> eof;
    Str -> list_to_binary(Str)
  end.

read_line(#?TYPE{name = ?M, data = Pid}) ->
  case io:request(Pid, {get_line, unicode, ""}) of
    eof -> eof;
    Str -> list_to_binary(Str)
  end.

skip(#?TYPE{name = ?M}, _Length) ->
  TypeName = atom_to_binary(?MODULE, utf8),
  error(<<"Unsupported operation: skip for ", TypeName/binary>>).

unread(#?TYPE{name = ?M}, _Ch) ->
  TypeName = atom_to_binary(?MODULE, utf8),
  error(<<"Unsupported operation: unread for ", TypeName/binary>>).

write(#?TYPE{name = ?M, data = Pid} = SW, Str) ->
  ok = io:put_chars(Pid, Str),
  SW.

write(#?TYPE{name = ?M, data = Pid} = SW, Format, Values) ->
  ok = io:fwrite(Pid, Format, clj_core:seq_to_list(Values)),
  SW.

str(#?TYPE{name = ?M, data = Pid}) ->
  <<"<", PidBin/binary>> = list_to_binary(erlang:pid_to_list(Pid)),
  <<"#<erlang.io.File ", PidBin/binary>>.
