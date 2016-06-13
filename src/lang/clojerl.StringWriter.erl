-module('clojerl.StringWriter').

-include("clojerl.hrl").

-behaviour('clojerl.Closeable').
-behaviour('clojerl.Stringable').
-behaviour('clojerl.IWriter').

-export([new/0, new/1]).
-export([ start_link/1
        , init/1
        , loop/1
        ]).

-export(['clojerl.Closeable.close'/1]).
-export(['clojerl.Stringable.str'/1]).
-export([ 'clojerl.IWriter.write'/2
        , 'clojerl.IWriter.write'/3
        ]).

-type type() :: #?TYPE{data :: pid()}.

-spec new() -> type().
new() ->
  new(<<>>).

-spec new(binary()) -> type().
new(Str) ->
  #?TYPE{data = start_link(Str)}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Closeable.close'(#?TYPE{name = ?M, data = Pid}) ->
  case send_command(Pid, close) of
    {error, _} -> error(<<"Couldn't close clojerl.StringWriter">>);
    _          -> undefined
  end.

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = Pid}) ->
  case send_command(Pid, str) of
    {error, _} -> error(<<"Couldn't get string from clojerl.StringWriter">>);
    Str        -> Str
  end.

'clojerl.IWriter.write'(#?TYPE{name = ?M, data = Pid} = SW, Str) ->
  ok = io:put_chars(Pid, Str),
  SW.

'clojerl.IWriter.write'(#?TYPE{name = ?M, data = Pid} = SW, Format, Values) ->
  ok = io:fwrite(Pid, Format, clj_core:seq_to_list(Values)),
  SW.

%%------------------------------------------------------------------------------
%% IO server
%%
%% Implementation of a subset of the io protocol in order to only support
%% writing operations.
%%------------------------------------------------------------------------------

-spec send_command(pid(), any()) -> any().
send_command(Pid, Cmd) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, Cmd},
  receive
    {Ref, Result} ->
      erlang:demonitor(Ref, [flush]),
      Result;
    {'DOWN', Ref, _, _, _} ->
      {error, terminated}
  end.

start_link(Str) ->
  spawn_link(?MODULE,init,[Str]).

init(Str) -> ?MODULE:loop(Str).

loop(Str) ->
  receive
    {io_request, From, ReplyAs, Request} ->
      {Reply, NewState} = request(Request, Str),
      reply(From, ReplyAs, Reply),
      ?MODULE:loop(NewState);
    {From, Ref, str} ->
      From ! {Ref, Str},
      ?MODULE:loop(Str);
    {From, Ref, close} ->
      From ! {Ref, ok};
    _Unknown ->
      ?MODULE:loop(Str)
  end.

reply(From, ReplyAs, Reply) ->
  From ! {io_reply, ReplyAs, Reply}.

request({put_chars, Encoding, Chars}, Str) ->
  EncodedChars = unicode:characters_to_list(Chars, Encoding),
  CharsBin = unicode:characters_to_binary(EncodedChars),
  {ok, <<Str/binary, CharsBin/binary>>};
request({put_chars, Encoding, Module, Function, Args}, State) ->
  try
    request({put_chars, Encoding, apply(Module, Function, Args)}, State)
  catch
    _:_ -> {{error, Function}, State}
  end;
request(_Other, State) ->
  {{error, request}, State}.
