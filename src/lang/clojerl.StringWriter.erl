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
  Ref = make_ref(),
  Pid ! {self(), Ref, close},
  receive
    {Ref, ok} -> undefined
  after
    1000 -> error(<<"Couldn't close clojerl.StringWriter">>)
  end.

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = Pid}) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, str},
  receive
    {Ref, Str} -> Str
  after
    1000 -> error(<<"Couldn't get string from clojerl.StringWriter">>)
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
  EncodedChars = unicode:characters_to_list(Chars,Encoding),
  CharsBin = unicode:characters_to_binary(EncodedChars),
  {ok, <<Str/binary, CharsBin/binary>>};
request({put_chars, Encoding, Module, Function, Args}, State) ->
  try
    request({put_chars, Encoding, apply(Module, Function, Args)}, State)
  catch
    _:_ -> {error, {error,Function}, State}
  end;
request({put_chars, Chars}, State) ->
  request({put_chars, unicode, Chars}, State);
request({put_chars, M, F, As}, State) ->
  request({put_chars, unicode, M, F, As}, State);
request(_Other, State) ->
  {{error, request}, State}.
