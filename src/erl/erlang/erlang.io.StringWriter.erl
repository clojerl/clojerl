-module('erlang.io.StringWriter').

-include("clojerl.hrl").

-behaviour('clojerl.Counted').
-behaviour('clojerl.Stringable').
-behaviour('erlang.io.Closeable').
-behaviour('erlang.io.IWriter').

-export([?CONSTRUCTOR/0, ?CONSTRUCTOR/1]).
-export([ init/1
        , loop/1
        ]).

-export([count/1]).
-export([str/1]).
-export([close/1]).
-export([ write/2
        , write/3
        ]).

-export([delete/3]).

-type type() :: #?TYPE{data :: pid()}.

-spec ?CONSTRUCTOR() -> type().
?CONSTRUCTOR() ->
  ?CONSTRUCTOR(<<>>).

-spec ?CONSTRUCTOR(binary()) -> type().
?CONSTRUCTOR(Str) ->
  #?TYPE{data = start_link(Str)}.

-spec delete(type(), pos_integer(), pos_integer()) -> type().
delete(#?TYPE{name = ?M, data = Pid}, Start, End) ->
  case send_command(Pid, {delete, Start, End}) of
    {error, _} -> error(<<"Couldn't delete range in erlang.io.StringWriter">>);
    Count      -> Count
  end.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(#?TYPE{name = ?M, data = Pid}) ->
  case send_command(Pid, count) of
    {error, _} -> error(<<"Couldn't get length from erlang.io.StringWriter">>);
    Count      -> Count
  end.


str(#?TYPE{name = ?M, data = Pid}) ->
  case send_command(Pid, str) of
    {error, _} -> error(<<"Couldn't get string from erlang.io.StringWriter">>);
    Str        -> Str
  end.

close(#?TYPE{name = ?M, data = Pid}) ->
  case send_command(Pid, close) of
    {error, _} -> error(<<"Couldn't close erlang.io.StringWriter">>);
    _          -> ?NIL
  end.

write(#?TYPE{name = ?M, data = Pid} = SW, Str) ->
  ok = io:put_chars(Pid, Str),
  SW.

write(#?TYPE{name = ?M, data = Pid} = SW, Format, Values) ->
  ok = io:fwrite(Pid, Format, clj_rt:to_list(Values)),
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
  spawn_link(?MODULE, init, [Str]).

init(Str) -> ?MODULE:loop(Str).

loop(Str) ->
  receive
    {io_request, From, ReplyAs, Request} ->
      {Reply, NewStr} = request(Request, Str),
      reply(From, ReplyAs, Reply),
      ?MODULE:loop(NewStr);
    {From, Ref, str} ->
      From ! {Ref, Str},
      ?MODULE:loop(Str);
    {From, Ref, count} ->
      From ! {Ref, 'clojerl.String':count(Str)},
      ?MODULE:loop(Str);
    {From, Ref, {delete, Start, End}} ->
      First  = 'clojerl.String':substring(Str, 0, Start),
      Second = 'clojerl.String':substring(Str, End),
      NewStr = <<First/binary, Second/binary>>,
      From ! {Ref, ok},
      ?MODULE:loop(NewStr);
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
