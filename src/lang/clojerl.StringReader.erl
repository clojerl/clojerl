-module('clojerl.StringReader').

-include("clojerl.hrl").

-behaviour('clojerl.Closeable').
-behaviour('clojerl.Stringable').
-behaviour('clojerl.IReader').

-export([new/1]).
-export([ start_link/1
        , init/1
        , loop/1
        , skip/3
        ]).

-export(['clojerl.Closeable.close'/1]).
-export(['clojerl.Stringable.str'/1]).
-export([ 'clojerl.IReader.read'/1
        , 'clojerl.IReader.read'/2
        , 'clojerl.IReader.read_line'/1
        , 'clojerl.IReader.skip'/2
        ]).

-type type() :: #?TYPE{data :: pid()}.

-spec new(binary()) -> type().
new(Str) when is_binary(Str) ->
  #?TYPE{data = start_link(Str)}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

'clojerl.Closeable.close'(#?TYPE{name = ?M, data = Pid}) ->
  case send_command(Pid, close) of
    {error, _} -> error(<<"Couldn't close clojerl.StringReader">>);
    _          -> undefined
  end.

'clojerl.Stringable.str'(#?TYPE{name = ?M, data = Pid}) ->
  <<_/utf8, PidStr/binary>> = erlang:list_to_binary(erlang:pid_to_list(Pid)),
  <<"#<clojerl.StringReader ", PidStr/binary>>.

'clojerl.IReader.read'(#?TYPE{name = ?M, data = Pid}) ->
  io:get_chars(Pid, "", 1).

'clojerl.IReader.read'(#?TYPE{name = ?M, data = Pid}, Length) ->
  io:get_chars(Pid, "", Length).

'clojerl.IReader.read_line'(#?TYPE{name = ?M, data = Pid}) ->
  io:request(Pid, {get_line, unicode, ""}).

'clojerl.IReader.skip'(#?TYPE{name = ?M, data = Pid}, Length) ->
  io:request(Pid, {get_until, unicode, "", ?MODULE, skip, [Length]}).

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
    {From, Ref, close} ->
      From ! {Ref, ok};
    _Unknown ->
      ?MODULE:loop(Str)
  end.

reply(From, ReplyAs, Reply) ->
  From ! {io_reply, ReplyAs, Reply}.

request({get_chars, Encoding, _Prompt, N}, Str) ->
  maybe_encode_result(Encoding, get_chars(N, Str));
request({get_line, Encoding, _Prompt}, Str) ->
  maybe_encode_result(Encoding, get_line(Str));
request({get_until, Encoding, _Prompt, Module, Function, Xargs}, Str) ->
  maybe_encode_result(Encoding, get_until(Module, Function, Xargs, Str));
request(_Other, State) ->
  {{error, request}, State}.

-spec maybe_encode_result(atom(), {term(), binary()}) -> {term(), binary()}.
maybe_encode_result(Encoding, {Result, NewStr}) when is_binary(Result) ->
  {unicode:characters_to_binary(Result, unicode, Encoding), NewStr};
maybe_encode_result(_, X) ->
  X.

-spec get_chars(integer(), binary()) -> {binary() | eof, binary()}.
get_chars(_N, <<>>) ->
  {eof, <<>>};
get_chars(1, <<Ch/utf8, Str/binary>>) ->
  {<<Ch>>, Str};
get_chars(N, Str) ->
  do_get_chars(N, Str, <<>>).

-spec do_get_chars(integer(), binary(), binary()) -> {binary(), binary()}.
do_get_chars(0, Str, Result) ->
  {Result, Str};
do_get_chars(_N, <<>>, Result) ->
  {Result, <<>>};
do_get_chars(N, <<Ch/utf8, NewStr/binary>>, Result) ->
  do_get_chars(N - 1, NewStr, <<Result/binary, Ch/utf8>>).

-spec get_line(binary()) -> {binary() | eof, binary()}.
get_line(<<>>) ->
  {eof, <<>>};
get_line(Str) ->
  do_get_line(Str, <<>>).

-spec do_get_line(binary(), binary()) -> {binary() | eof, binary()}.
do_get_line(<<>>, Result) ->
  {Result, <<>>};
do_get_line(<<"\r\n"/utf8, RestStr/binary>>, Result) ->
  {Result, RestStr};
do_get_line(<<"\n"/utf8, RestStr/binary>>, Result) ->
  {Result, RestStr};
do_get_line(<<"\r"/utf8, RestStr/binary>>, Result) ->
  {Result, RestStr};
do_get_line(<<Ch/utf8, RestStr/binary>>, Result) ->
  do_get_line(RestStr, <<Result/binary, Ch/utf8>>).

-spec get_until(module(), atom(), list(), term()) ->
  {term(), binary()}.
get_until(Module, Function, XArgs, State) ->
  case apply(Module, Function, [State, undefined | XArgs]) of
    {done, Result, NewStr} -> {Result, NewStr};
    {more, NewState} -> get_until(Module, Function, XArgs, NewState)
  end.

-spec skip(binary() | {cont, integer(), binary()}, term(), integer()) ->
  {more, {cont, integer(), binary()}} | {done, integer(), binary()}.
skip(Str, _Data, Length) when is_binary(Str) ->
  {more, {cont, Length, Str}};
skip({cont, 0, Str}, _Data, Length) ->
  {done, Length, Str};
skip({cont, Length, <<>>}, _Data, Length) ->
  {done, eof, <<>>};
skip({cont, Length, <<_/utf8, RestStr/binary>>}, _Data, _Length) ->
  {more, {cont, Length - 1, RestStr}}.
