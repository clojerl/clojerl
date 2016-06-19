-module('erlang.io.File').

-include("clojerl.hrl").

-behaviour('clojerl.Closeable').
-behaviour('clojerl.IReader').
-behaviour('clojerl.IWriter').
-behaviour('clojerl.Stringable').

-export([open/1, open/2]).

-export(['clojerl.Closeable.close'/1]).
-export([ 'clojerl.IReader.read'/1
        , 'clojerl.IReader.read'/2
        , 'clojerl.IReader.read_line'/1
        , 'clojerl.IReader.skip'/2
        , 'clojerl.IReader.unread'/2
        ]).
-export([ 'clojerl.IWriter.write'/2
        , 'clojerl.IWriter.write'/3
        ]).
-export(['clojerl.Stringable.str'/1]).

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

'clojerl.Closeable.close'(#?TYPE{name = ?M, data = Pid}) ->
  case file:close(Pid) of
    {error, _} -> error(<<"Couldn't close erlang.io.File">>);
    ok         -> undefined
  end.

'clojerl.IReader.read'(File) ->
  'clojerl.IReader.read'(File, 1).

'clojerl.IReader.read'(#?TYPE{name = ?M, data = Pid}, Length) ->
  case io:get_chars(Pid, "", Length) of
    eof -> eof;
    Str -> list_to_binary(Str)
  end.

'clojerl.IReader.read_line'(#?TYPE{name = ?M, data = Pid}) ->
  case io:request(Pid, {get_line, unicode, ""}) of
    eof -> eof;
    Str -> list_to_binary(Str)
  end.

'clojerl.IReader.skip'(#?TYPE{name = ?M}, _Length) ->
  TypeName = atom_to_binary(?MODULE, utf8),
  error(<<"Unsupported operation: skip for ", TypeName/binary>>).

'clojerl.IReader.unread'(#?TYPE{name = ?M}, _Ch) ->
  TypeName = atom_to_binary(?MODULE, utf8),
  error(<<"Unsupported operation: unread for ", TypeName/binary>>).

'clojerl.IWriter.write'(#?TYPE{name = ?M, data = Pid} = SW, Str) ->
  ok = io:put_chars(Pid, Str),
  SW.

'clojerl.IWriter.write'(#?TYPE{name = ?M, data = Pid} = SW, Format, Values) ->
  ok = io:fwrite(Pid, Format, clj_core:seq_to_list(Values)),
  SW.

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = Pid}) ->
  <<"<", PidBin/binary>> = list_to_binary(erlang:pid_to_list(Pid)),
  <<"#<erlang.io.File ", PidBin/binary>>.
